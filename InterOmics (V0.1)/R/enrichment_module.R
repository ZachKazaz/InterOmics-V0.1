# enrichment_module.R
# Entry points:
#   run_ora(session_id, params)
#   run_topology(session_id, params)
#   render_kegg_pathway(pathway_id, matched_kegg_ids)
#   run_lipid_enrichment(session_id, params)

source("R/utils.R")

# ===========================================================================
# Internal helpers
# ===========================================================================

`%||%` <- function(a, b) if (!is.null(a)) a else b

# ---------------------------------------------------------------------------
# Load id_mapping.json for a session
# ---------------------------------------------------------------------------
.load_id_mapping <- function(session_id) {
  path <- get_session_path(session_id, "id_mapping.json")
  if (!file.exists(path)) stop("id_mapping.json not found for session: ", session_id)
  jsonlite::fromJSON(path, simplifyVector = TRUE)
}

# ---------------------------------------------------------------------------
# Collect all KEGG IDs from preprocessed datasets (background universe)
# ---------------------------------------------------------------------------
.background_kegg_ids <- function(session_id) {
  mapping <- .load_id_mapping(session_id)
  ids <- mapping$kegg_id
  ids[!is.na(ids) & nchar(ids) > 0]
}

# ===========================================================================
# 2.6.1  KEGG retrieval helper with retry
# ===========================================================================

#' Retrieve a KEGG resource with up to max_attempts total attempts.
#'
#' @param pathway_id  KEGG pathway identifier (e.g. "hsa00010")
#' @param option      One of "kgml", "image", or omitted for default entry
#' @param max_attempts Total number of attempts (default 3)
#' @param delay_sec   Seconds to wait between attempts (default 1)
#' @return Result from KEGGREST::keggGet, or NULL after all attempts fail
kegg_get_with_retry <- function(pathway_id, option = NULL,
                                max_attempts = 3, delay_sec = 1) {
  if (!requireNamespace("KEGGREST", quietly = TRUE)) {
    warning("KEGGREST is not installed; KEGG retrieval skipped.")
    return(NULL)
  }

  last_error <- NULL
  for (attempt in seq_len(max_attempts)) {
    result <- tryCatch({
      if (is.null(option)) {
        KEGGREST::keggGet(pathway_id)
      } else {
        KEGGREST::keggGet(pathway_id, option)
      }
    }, error = function(e) {
      last_error <<- conditionMessage(e)
      NULL
    })

    if (!is.null(result)) return(result)

    if (attempt < max_attempts) Sys.sleep(delay_sec)
  }

  warning("KEGG retrieval failed for '", pathway_id,
          "' after ", max_attempts, " attempts. Last error: ", last_error)
  NULL
}

# ===========================================================================
# 2.6.2  ORA — Over-Representation Analysis
# ===========================================================================

#' Run ORA using hypergeometric test against KEGG (and optionally SMPDB).
#'
#' Hypergeometric parameterisation (phyper lower.tail=FALSE):
#'   q = k - 1  (matched - 1, for P(X >= k))
#'   m = K      (pathway size in background)
#'   n = N - K  (background not in pathway)
#'   k = n_query (query list size)
#'
#' @param session_id  Session identifier
#' @param params      List with: database ("KEGG"|"SMPDB"), feature_list (KEGG IDs)
run_ora <- function(session_id, params) {
  database     <- toupper(params$database %||% "KEGG")
  query_ids    <- params$feature_list

  if (length(query_ids) == 0) {
    return(error_response("NO_FILES_FOUND",
                          "feature_list is empty. Provide KEGG IDs for ORA.",
                          "enrichment", recoverable = TRUE))
  }

  background <- .background_kegg_ids(session_id)
  if (length(background) == 0) {
    return(error_response("NO_FILES_FOUND",
                          "No mapped KEGG IDs found in id_mapping.json. Run ID mapping first.",
                          "enrichment", recoverable = TRUE))
  }

  # Restrict query to features present in background
  query_ids <- intersect(query_ids, background)
  if (length(query_ids) == 0) {
    return(error_response("NO_FILES_FOUND",
                          "None of the provided feature_list IDs are present in the background universe.",
                          "enrichment", recoverable = TRUE))
  }

  results <- switch(database,
    KEGG  = .run_ora_kegg(query_ids, background),
    SMPDB = .run_ora_smpdb(query_ids, background),
    return(error_response("ANALYSIS_RUNTIME_ERROR",
                          paste0("Unsupported database: ", database,
                                 ". Use 'KEGG' or 'SMPDB'."),
                          "enrichment", recoverable = TRUE))
  )

  if (is.null(results) || nrow(results) == 0) {
    return(error_response("ANALYSIS_RUNTIME_ERROR",
                          "ORA returned no results. Check KEGG connectivity and feature IDs.",
                          "enrichment", recoverable = TRUE))
  }

  # BH FDR correction
  results$fdr <- p.adjust(results$p_value, method = "BH")
  results     <- results[order(results$p_value), ]

  out_dir <- get_session_path(session_id, "results", "enrichment")
  write.csv(results, file.path(out_dir, "ora_results.csv"), row.names = FALSE)

  # Plots
  plot_ora_bar(results, out_dir)
  plot_ora_bubble(results, out_dir)

  ok_response(list(
    database      = database,
    n_pathways    = nrow(results),
    n_significant = sum(results$fdr < 0.05, na.rm = TRUE),
    output        = "results/enrichment/ora_results.csv"
  ))
}

# Internal: ORA against KEGG pathways
.run_ora_kegg <- function(query_ids, background) {
  # Retrieve pathway → compound mapping from KEGG
  pw_compound <- tryCatch(
    KEGGREST::keggLink("pathway", "compound"),
    error = function(e) NULL
  )
  if (is.null(pw_compound)) {
    warning("Could not retrieve KEGG pathway-compound links.")
    return(NULL)
  }

  # pw_compound is a named character vector: names = "cpd:Cxxxxx", values = "path:xxx"
  # Reformat to pathway → compound list
  compound_ids <- sub("^cpd:", "", names(pw_compound))
  pathway_ids  <- sub("^path:", "", pw_compound)

  pathway_map <- split(compound_ids, pathway_ids)

  N <- length(background)
  n <- length(query_ids)

  rows <- lapply(names(pathway_map), function(pw) {
    pw_cpds <- pathway_map[[pw]]
    K       <- length(intersect(pw_cpds, background))  # pathway size in background
    if (K == 0) return(NULL)
    k       <- length(intersect(query_ids, pw_cpds))   # matched
    if (k == 0) return(NULL)

    p_val <- phyper(k - 1, K, N - K, n, lower.tail = FALSE)

    # Retrieve pathway name (best-effort; use ID if unavailable)
    pw_name <- tryCatch({
      info <- kegg_get_with_retry(paste0("path:", pw))
      if (!is.null(info) && !is.null(info[[1]]$NAME)) info[[1]]$NAME else pw
    }, error = function(e) pw)

    data.frame(
      pathway_id        = pw,
      pathway_name      = pw_name,
      total_compounds   = K,
      matched_compounds = k,
      p_value           = p_val,
      stringsAsFactors  = FALSE
    )
  })

  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) return(data.frame())
  do.call(rbind, rows)
}

# Internal: ORA against SMPDB (stub — returns empty with warning if SMPDB
# compound sets are not available locally; no fabrication)
.run_ora_smpdb <- function(query_ids, background) {
  warning("SMPDB ORA requires a local SMPDB compound set file. ",
          "Provide the file path via options(smpdb_compound_file = '...'). ",
          "Returning empty results.")
  data.frame(pathway_id = character(0), pathway_name = character(0),
             total_compounds = integer(0), matched_compounds = integer(0),
             p_value = numeric(0), stringsAsFactors = FALSE)
}

# ===========================================================================
# 2.6.3  Pathway Topology
# ===========================================================================

#' Combine ORA results with KEGG pathway topology metrics.
#'
#' Impact score = sum(centrality of matched nodes) / sum(centrality of all nodes)
#'
#' @param session_id  Session identifier
#' @param params      List with: topology_metric ("betweenness" | "degree")
run_topology <- function(session_id, params) {
  topology_metric <- tolower(params$topology_metric %||% "betweenness")

  ora_path <- get_session_path(session_id, "results", "enrichment", "ora_results.csv")
  if (!file.exists(ora_path)) {
    return(error_response("PREREQUISITE_NOT_MET",
                          "ORA results not found. Run ORA before pathway topology.",
                          "enrichment", recoverable = TRUE))
  }

  ora <- read.csv(ora_path, stringsAsFactors = FALSE)
  # Work on significant pathways (FDR < 0.05) to limit KEGG API calls
  sig <- ora[!is.na(ora$fdr) & ora$fdr < 0.05, ]
  if (nrow(sig) == 0) sig <- ora  # fall back to all if none significant

  background <- .background_kegg_ids(session_id)
  mapping    <- .load_id_mapping(session_id)
  query_ids  <- mapping$kegg_id[!is.na(mapping$kegg_id)]

  topo_rows <- lapply(seq_len(nrow(sig)), function(i) {
    pw_id <- sig$pathway_id[i]

    kgml <- kegg_get_with_retry(paste0("path:", pw_id), "kgml")
    if (is.null(kgml)) {
      return(data.frame(
        pathway_id   = pw_id,
        pathway_name = sig$pathway_name[i],
        p_value      = sig$p_value[i],
        fdr          = sig$fdr[i],
        impact_score = NA_real_,
        stringsAsFactors = FALSE
      ))
    }

    impact <- tryCatch({
      .compute_impact_score(kgml, query_ids, background, topology_metric)
    }, error = function(e) NA_real_)

    data.frame(
      pathway_id   = pw_id,
      pathway_name = sig$pathway_name[i],
      p_value      = sig$p_value[i],
      fdr          = sig$fdr[i],
      impact_score = impact,
      stringsAsFactors = FALSE
    )
  })

  topo_df <- do.call(rbind, topo_rows)
  topo_df <- topo_df[order(topo_df$p_value), ]

  out_dir <- get_session_path(session_id, "results", "enrichment")
  write.csv(topo_df, file.path(out_dir, "topology_results.csv"), row.names = FALSE)

  plot_topology_bubble(topo_df, out_dir)

  ok_response(list(
    topology_metric = topology_metric,
    n_pathways      = nrow(topo_df),
    output          = "results/enrichment/topology_results.csv"
  ))
}

# Compute impact score from KGML for one pathway
.compute_impact_score <- function(kgml, query_ids, background, metric) {
  if (!requireNamespace("igraph", quietly = TRUE))
    stop("Package 'igraph' is required for topology analysis.")

  # Parse KGML: extract compound entries and reaction edges
  # kgml is a raw character vector (XML); parse with xml2 if available
  if (!requireNamespace("xml2", quietly = TRUE)) {
    warning("Package 'xml2' is required for KGML parsing. Impact score set to NA.")
    return(NA_real_)
  }

  doc   <- xml2::read_xml(paste(kgml, collapse = "\n"))
  nodes <- xml2::xml_find_all(doc, ".//entry[@type='compound']")
  if (length(nodes) == 0) return(NA_real_)

  node_ids <- xml2::xml_attr(nodes, "name")
  # node_ids may be "cpd:C00001 cpd:C00002" — split and clean
  node_ids <- unique(unlist(strsplit(node_ids, " ")))
  node_ids <- sub("^cpd:", "", node_ids)
  node_ids <- node_ids[node_ids %in% background]
  if (length(node_ids) < 2) return(NA_real_)

  # Build edges from reactions
  reactions <- xml2::xml_find_all(doc, ".//reaction")
  edges <- lapply(reactions, function(r) {
    substrates <- xml2::xml_attr(xml2::xml_find_all(r, "substrate"), "name")
    products   <- xml2::xml_attr(xml2::xml_find_all(r, "product"),   "name")
    substrates <- sub("^cpd:", "", substrates)
    products   <- sub("^cpd:", "", products)
    if (length(substrates) == 0 || length(products) == 0) return(NULL)
    expand.grid(from = substrates, to = products, stringsAsFactors = FALSE)
  })
  edges <- do.call(rbind, Filter(Negate(is.null), edges))

  if (is.null(edges) || nrow(edges) == 0) return(NA_real_)

  g <- igraph::graph_from_data_frame(edges, directed = FALSE,
                                     vertices = data.frame(name = node_ids))

  centrality <- switch(metric,
    betweenness = igraph::betweenness(g, normalized = TRUE),
    degree      = igraph::degree(g),
    igraph::betweenness(g, normalized = TRUE)
  )

  all_sum     <- sum(centrality, na.rm = TRUE)
  if (all_sum == 0) return(0)

  matched_nodes   <- intersect(query_ids, node_ids)
  matched_central <- sum(centrality[names(centrality) %in% matched_nodes], na.rm = TRUE)

  matched_central / all_sum
}

# ===========================================================================
# 2.6.4  KEGG pathway diagram rendering
# ===========================================================================

#' Retrieve and optionally annotate a KEGG pathway diagram.
#'
#' Fallback behaviour (as per design.md):
#'   - Image retrieval fails after retries → return list(image=NULL, warning=...)
#'   - Image OK but KGML/coordinate parsing fails → return real image without
#'     highlighting + note
#'   - Image OK and highlighting succeeds → return annotated image
#'
#' @param pathway_id       KEGG pathway ID (e.g. "hsa00010")
#' @param matched_kegg_ids Character vector of matched KEGG compound IDs
#' @return list(image, warning, note)  where image is a raw PNG binary or NULL
render_kegg_pathway <- function(pathway_id, matched_kegg_ids = character(0)) {
  # Step 1: retrieve pathway image (with retry)
  img_raw <- kegg_get_with_retry(pathway_id, "image")

  if (is.null(img_raw)) {
    return(list(
      image   = NULL,
      warning = paste0("KEGG pathway image could not be retrieved. ",
                       "Pathway diagram omitted for ", pathway_id, "."),
      note    = NULL
    ))
  }

  # Step 2: if no matched IDs, return image as-is
  if (length(matched_kegg_ids) == 0) {
    return(list(image = img_raw, warning = NULL, note = NULL))
  }

  # Step 3: attempt KGML retrieval and compound highlighting
  highlighted <- tryCatch(
    .highlight_compounds(img_raw, pathway_id, matched_kegg_ids),
    error = function(e) {
      list(image = img_raw,
           note  = paste0("Compound highlighting unavailable for ", pathway_id, "."))
    }
  )

  list(
    image   = highlighted$image,
    warning = NULL,
    note    = highlighted$note
  )
}

# Internal: overlay compound highlights on a KEGG pathway PNG
.highlight_compounds <- function(img_raw, pathway_id, matched_kegg_ids) {
  if (!requireNamespace("magick", quietly = TRUE) ||
      !requireNamespace("xml2",   quietly = TRUE)) {
    return(list(
      image = img_raw,
      note  = paste0("Compound highlighting unavailable for ", pathway_id,
                     " (magick or xml2 not installed).")
    ))
  }

  kgml <- kegg_get_with_retry(pathway_id, "kgml")
  if (is.null(kgml)) {
    return(list(
      image = img_raw,
      note  = paste0("Compound highlighting unavailable for ", pathway_id,
                     " (KGML retrieval failed).")
    ))
  }

  doc   <- xml2::read_xml(paste(kgml, collapse = "\n"))
  nodes <- xml2::xml_find_all(doc, ".//entry[@type='compound']")

  # Build a lookup: compound ID → (x, y, width, height) from graphics element
  coords <- lapply(nodes, function(n) {
    raw_name <- xml2::xml_attr(n, "name")
    cpd_ids  <- sub("^cpd:", "", unlist(strsplit(raw_name, " ")))
    gfx      <- xml2::xml_find_first(n, "graphics")
    if (is.na(xml2::xml_attr(gfx, "x"))) return(NULL)
    list(
      ids = cpd_ids,
      x   = as.numeric(xml2::xml_attr(gfx, "x")),
      y   = as.numeric(xml2::xml_attr(gfx, "y")),
      w   = as.numeric(xml2::xml_attr(gfx, "width"))  %||% 8,
      h   = as.numeric(xml2::xml_attr(gfx, "height")) %||% 8
    )
  })
  coords <- Filter(Negate(is.null), coords)

  if (length(coords) == 0) {
    return(list(
      image = img_raw,
      note  = paste0("Compound highlighting unavailable for ", pathway_id,
                     " (no coordinate data in KGML).")
    ))
  }

  # Load image with magick
  img <- magick::image_read(img_raw)

  for (entry in coords) {
    if (!any(entry$ids %in% matched_kegg_ids)) next
    # Draw a yellow rectangle around the matched compound node
    img <- magick::image_draw(img)
    rect(entry$x - entry$w / 2, entry$y - entry$h / 2,
         entry$x + entry$w / 2, entry$y + entry$h / 2,
         border = "yellow", lwd = 2)
    grDevices::dev.off()
  }

  annotated_raw <- magick::image_write(img, format = "png")

  list(image = annotated_raw, note = NULL)
}

# ===========================================================================
# 2.6.5  Lipid class enrichment
# ===========================================================================

#' Hypergeometric test on lipid classes.
#'
#' @param session_id  Session identifier
#' @param params      List with: query_features (character vector of lipid feature names)
run_lipid_enrichment <- function(session_id, params) {
  mapping <- .load_id_mapping(session_id)

  # All lipid features in the preprocessed dataset (background)
  lipid_rows  <- mapping[!is.na(mapping$lipid_class) & mapping$lipid_class != "", ]
  if (nrow(lipid_rows) == 0) {
    return(error_response("NO_FILES_FOUND",
                          "No lipid class assignments found in id_mapping.json.",
                          "enrichment", recoverable = TRUE))
  }

  background_features <- lipid_rows$feature
  background_classes  <- lipid_rows$lipid_class
  N <- length(background_features)

  # Query list: user-supplied significant lipid features
  query_features <- params$query_features %||% character(0)
  query_features <- intersect(query_features, background_features)
  n <- length(query_features)

  if (n == 0) {
    return(error_response("NO_FILES_FOUND",
                          "None of the query_features are present in the lipid background.",
                          "enrichment", recoverable = TRUE))
  }

  query_classes <- background_classes[background_features %in% query_features]
  all_classes   <- unique(background_classes)

  rows <- lapply(all_classes, function(cls) {
    K <- sum(background_classes == cls)          # class size in background
    k <- sum(query_classes == cls)               # matched in query
    if (k == 0) return(NULL)
    p_val <- phyper(k - 1, K, N - K, n, lower.tail = FALSE)
    data.frame(
      lipid_class       = cls,
      total_count       = K,
      matched_count     = k,
      p_value           = p_val,
      stringsAsFactors  = FALSE
    )
  })

  rows    <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) {
    return(error_response("ANALYSIS_RUNTIME_ERROR",
                          "No lipid classes were enriched in the query set.",
                          "enrichment", recoverable = TRUE))
  }

  result_df       <- do.call(rbind, rows)
  result_df$fdr   <- p.adjust(result_df$p_value, method = "BH")
  result_df       <- result_df[order(result_df$p_value), ]

  out_dir <- get_session_path(session_id, "results", "enrichment")
  write.csv(result_df, file.path(out_dir, "lipid_enrichment_results.csv"),
            row.names = FALSE)

  plot_lipid_bar(result_df, out_dir)
  plot_lipid_bubble(result_df, out_dir)

  ok_response(list(
    n_classes     = nrow(result_df),
    n_significant = sum(result_df$fdr < 0.05, na.rm = TRUE),
    output        = "results/enrichment/lipid_enrichment_results.csv"
  ))
}

# ===========================================================================
# 2.6.6  Plot generation
# ===========================================================================

.save_gg <- function(p, path_base, width = 8, height = 6) {
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Package 'ggplot2' is required.")
  ggplot2::ggsave(paste0(path_base, ".png"), plot = p,
                  width = width, height = height, dpi = 150)
  ggplot2::ggsave(paste0(path_base, ".pdf"), plot = p,
                  width = width, height = height)
  invisible(NULL)
}

plot_ora_bar <- function(results_df, out_dir) {
  top <- head(results_df[order(results_df$fdr), ], 20)
  top$neg_log10_fdr <- -log10(pmax(top$fdr, 1e-300))
  top$pathway_name  <- factor(top$pathway_name,
                               levels = rev(top$pathway_name))

  p <- ggplot2::ggplot(top,
         ggplot2::aes(x = neg_log10_fdr, y = pathway_name, fill = fdr)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::scale_fill_gradient(low = "red", high = "blue",
                                  name = "FDR") +
    ggplot2::labs(x = "-log10(FDR)", y = NULL,
                  title = "ORA: Top Enriched Pathways") +
    ggplot2::theme_minimal()

  .save_gg(p, file.path(out_dir, "ora_bar"))
}

plot_ora_bubble <- function(results_df, out_dir) {
  df <- results_df
  df$gene_ratio     <- df$matched_compounds / df$total_compounds
  df$neg_log10_p    <- -log10(pmax(df$p_value, 1e-300))

  p <- ggplot2::ggplot(df,
         ggplot2::aes(x = gene_ratio, y = neg_log10_p,
                      size = matched_compounds, color = fdr)) +
    ggplot2::geom_point(alpha = 0.7) +
    ggplot2::scale_color_gradient(low = "red", high = "blue", name = "FDR") +
    ggplot2::scale_size(range = c(2, 10), name = "Matched") +
    ggplot2::labs(x = "Gene Ratio", y = "-log10(p-value)",
                  title = "ORA Bubble Plot") +
    ggplot2::theme_minimal()

  .save_gg(p, file.path(out_dir, "ora_bubble"))
}

plot_topology_bubble <- function(results_df, out_dir) {
  df <- results_df[!is.na(results_df$impact_score), ]
  if (nrow(df) == 0) {
    message("No topology results with impact scores to plot.")
    return(invisible(NULL))
  }
  df$neg_log10_p <- -log10(pmax(df$p_value, 1e-300))

  p <- ggplot2::ggplot(df,
         ggplot2::aes(x = impact_score, y = neg_log10_p,
                      size = matched_compounds, color = fdr)) +
    ggplot2::geom_point(alpha = 0.7) +
    ggplot2::scale_color_gradient(low = "red", high = "blue", name = "FDR") +
    ggplot2::scale_size(range = c(2, 10), name = "Matched") +
    ggplot2::labs(x = "Pathway Impact Score", y = "-log10(p-value)",
                  title = "Pathway Topology Analysis") +
    ggplot2::theme_minimal()

  .save_gg(p, file.path(out_dir, "topology_bubble"))
}

plot_lipid_bar <- function(results_df, out_dir) {
  df <- results_df
  df$neg_log10_fdr <- -log10(pmax(df$fdr, 1e-300))
  df$lipid_class   <- factor(df$lipid_class,
                              levels = rev(df$lipid_class[order(df$fdr)]))

  p <- ggplot2::ggplot(df,
         ggplot2::aes(x = neg_log10_fdr, y = lipid_class, fill = fdr)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::scale_fill_gradient(low = "red", high = "blue", name = "FDR") +
    ggplot2::labs(x = "-log10(FDR)", y = "Lipid Class",
                  title = "Lipid Class Enrichment") +
    ggplot2::theme_minimal()

  .save_gg(p, file.path(out_dir, "lipid_bar"))
}

plot_lipid_bubble <- function(results_df, out_dir) {
  df <- results_df
  df$ratio       <- df$matched_count / df$total_count
  df$neg_log10_p <- -log10(pmax(df$p_value, 1e-300))

  p <- ggplot2::ggplot(df,
         ggplot2::aes(x = ratio, y = neg_log10_p,
                      size = matched_count, color = fdr)) +
    ggplot2::geom_point(alpha = 0.7) +
    ggplot2::scale_color_gradient(low = "red", high = "blue", name = "FDR") +
    ggplot2::scale_size(range = c(2, 10), name = "Matched") +
    ggplot2::labs(x = "Class Ratio", y = "-log10(p-value)",
                  title = "Lipid Class Enrichment Bubble Plot") +
    ggplot2::theme_minimal()

  .save_gg(p, file.path(out_dir, "lipid_bubble"))
}
