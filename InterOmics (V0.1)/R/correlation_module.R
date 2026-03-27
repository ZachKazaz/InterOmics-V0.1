# correlation_module.R
# Entry points:
#   run_correlation_single(session_id, params)
#   run_correlation_cross(session_id, params)

source("R/utils.R")

# ===========================================================================
# Internal helpers
# ===========================================================================

# Load a preprocessed feature matrix (numeric columns only, no SampleID)
.load_preprocessed_mat <- function(session_id, dataset_name) {
  path <- get_session_path(session_id, "preprocessed",
                           paste0(dataset_name, ".csv"))
  if (!file.exists(path)) {
    stop("Preprocessed dataset not found: ", dataset_name)
  }
  df  <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  mat <- as.matrix(df[, setdiff(colnames(df), "SampleID"), drop = FALSE])
  class(mat) <- "numeric"
  mat
}

# Read the scaling method used for a dataset from preprocessing_summary.json
.get_scaling_method <- function(session_id, dataset_name) {
  summary_path <- get_session_path(session_id, "preprocessing_summary.json")
  if (!file.exists(summary_path)) return(NA_character_)
  s <- jsonlite::fromJSON(summary_path, simplifyVector = FALSE)
  per <- s$per_dataset[[dataset_name]]
  if (is.null(per)) return(NA_character_)
  tolower(per$scaling_method %||% "none")
}

# Compute pairwise p-values via cor.test() for Pearson / Spearman
# Returns a symmetric matrix of p-values (diagonal = NA)
.pairwise_cor_test_pvalues <- function(mat, method) {
  n   <- ncol(mat)
  pmat <- matrix(NA_real_, nrow = n, ncol = n,
                 dimnames = list(colnames(mat), colnames(mat)))
  for (i in seq_len(n - 1)) {
    for (j in seq(i + 1, n)) {
      res        <- tryCatch(
        cor.test(mat[, i], mat[, j], method = method),
        error = function(e) list(p.value = NA_real_)
      )
      pmat[i, j] <- res$p.value
      pmat[j, i] <- res$p.value
    }
  }
  pmat
}

# Apply BH FDR to the upper triangle of a symmetric p-value matrix;
# returns a symmetric FDR matrix (diagonal = NA)
.fdr_matrix <- function(pmat) {
  n    <- nrow(pmat)
  fmat <- matrix(NA_real_, nrow = n, ncol = n,
                 dimnames = dimnames(pmat))
  upper_idx <- which(upper.tri(pmat))
  p_vec     <- pmat[upper_idx]
  fdr_vec   <- p.adjust(p_vec, method = "BH")
  fmat[upper_idx]                    <- fdr_vec
  fmat[lower.tri(fmat)]              <- t(fmat)[lower.tri(fmat)]
  fmat
}

# Build a tidy edge list from correlation, p-value, and FDR matrices
# filtered by threshold and direction
.build_edge_list <- function(cor_mat, p_mat, fdr_mat, threshold, direction) {
  n     <- ncol(cor_mat)
  feats <- colnames(cor_mat)
  rows  <- list()

  for (i in seq_len(n - 1)) {
    for (j in seq(i + 1, n)) {
      r <- cor_mat[i, j]
      if (is.na(r)) next
      if (abs(r) < threshold) next
      if (direction == "positive" && r < 0) next
      if (direction == "negative" && r > 0) next
      rows <- c(rows, list(list(
        feature1    = feats[i],
        feature2    = feats[j],
        correlation = r,
        p_value     = p_mat[i, j],
        fdr         = fdr_mat[i, j]
      )))
    }
  }

  if (length(rows) == 0) {
    return(data.frame(
      feature1    = character(0),
      feature2    = character(0),
      correlation = numeric(0),
      p_value     = numeric(0),
      fdr         = numeric(0),
      stringsAsFactors = FALSE
    ))
  }

  do.call(rbind, lapply(rows, as.data.frame, stringsAsFactors = FALSE))
}

# Build igraph from edge list; compute degree, betweenness, closeness
.build_network <- function(edge_list, all_features) {
  if (!requireNamespace("igraph", quietly = TRUE))
    stop("Package 'igraph' is required.")

  g <- igraph::graph_from_data_frame(
    d        = edge_list[, c("feature1", "feature2")],
    directed = FALSE,
    vertices = data.frame(name = all_features, stringsAsFactors = FALSE)
  )
  igraph::E(g)$weight     <- abs(edge_list$correlation)
  igraph::E(g)$correlation <- edge_list$correlation

  node_metrics <- data.frame(
    feature     = igraph::V(g)$name,
    degree      = igraph::degree(g),
    betweenness = igraph::betweenness(g, normalized = TRUE),
    closeness   = igraph::closeness(g, normalized = TRUE),
    stringsAsFactors = FALSE
  )
  list(graph = g, node_metrics = node_metrics)
}

# ===========================================================================
# 2.5.3  Plot generation
# ===========================================================================

.save_plot <- function(expr, path_base, width = 1200, height = 900, res = 150) {
  png_path <- paste0(path_base, ".png")
  pdf_path <- paste0(path_base, ".pdf")

  grDevices::png(png_path, width = width, height = height, res = res)
  tryCatch(eval(expr), finally = grDevices::dev.off())

  grDevices::pdf(pdf_path, width = width / res, height = height / res)
  tryCatch(eval(expr), finally = grDevices::dev.off())

  invisible(list(png = png_path, pdf = pdf_path))
}

plot_correlation_heatmap <- function(cor_mat, out_dir) {
  if (!requireNamespace("pheatmap", quietly = TRUE))
    stop("Package 'pheatmap' is required.")
  path_base <- file.path(out_dir, "correlation_heatmap")
  .save_plot(quote(
    pheatmap::pheatmap(cor_mat,
                       clustering_distance_rows = "euclidean",
                       clustering_distance_cols = "euclidean",
                       main = "Correlation Heatmap",
                       silent = TRUE)
  ), path_base)
}

plot_correlation_threshold_matrix <- function(cor_mat, threshold, out_dir) {
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Package 'ggplot2' is required.")
  if (!requireNamespace("reshape2", quietly = TRUE))
    stop("Package 'reshape2' is required.")

  path_base <- file.path(out_dir, "correlation_threshold_matrix")

  filtered <- cor_mat
  filtered[abs(filtered) < threshold] <- NA

  df <- reshape2::melt(filtered, varnames = c("Feature1", "Feature2"),
                       value.name = "Correlation", na.rm = TRUE)

  p <- ggplot2::ggplot(df, ggplot2::aes(x = Feature1, y = Feature2,
                                         fill = Correlation)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                                   midpoint = 0, limits = c(-1, 1)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)) +
    ggplot2::labs(title = paste0("Thresholded Correlation Matrix (|r| \u2265 ",
                                  threshold, ")"))

  .save_plot(quote(print(p)), path_base)
}

plot_correlation_network <- function(igraph_obj, centrality_metric, out_dir) {
  if (!requireNamespace("ggraph", quietly = TRUE))
    stop("Package 'ggraph' is required.")
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Package 'ggplot2' is required.")

  path_base <- file.path(out_dir, "correlation_network")

  # Attach centrality to vertex attributes
  g <- igraph_obj
  metric_vals <- switch(centrality_metric,
    degree      = igraph::degree(g),
    betweenness = igraph::betweenness(g, normalized = TRUE),
    closeness   = igraph::closeness(g, normalized = TRUE),
    igraph::degree(g)  # default
  )
  igraph::V(g)$centrality <- as.numeric(metric_vals)

  # Edge direction colour
  edge_cors <- igraph::E(g)$correlation
  edge_dir  <- ifelse(is.na(edge_cors), "positive",
                      ifelse(edge_cors >= 0, "positive", "negative"))
  igraph::E(g)$direction <- edge_dir

  p <- ggraph::ggraph(g, layout = "fr") +
    ggraph::geom_edge_link(
      ggplot2::aes(width = abs(igraph::E(g)$correlation),
                   color = igraph::E(g)$direction),
      alpha = 0.6
    ) +
    ggraph::geom_node_point(
      ggplot2::aes(size = igraph::V(g)$centrality),
      color = "steelblue"
    ) +
    ggraph::geom_node_text(
      ggplot2::aes(label = igraph::V(g)$name),
      repel = TRUE, size = 2.5
    ) +
    ggplot2::scale_edge_color_manual(
      values = c("positive" = "red", "negative" = "blue")
    ) +
    ggraph::scale_edge_width(range = c(0.3, 2)) +
    ggplot2::theme_void() +
    ggplot2::labs(title = paste0("Correlation Network (ranked by ", centrality_metric, ")"))

  .save_plot(quote(print(p)), path_base)
}

plot_cross_dataset_circos <- function(igraph_obj, dataset_a, dataset_b, out_dir) {
  if (!requireNamespace("ggraph", quietly = TRUE))
    stop("Package 'ggraph' is required.")
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Package 'ggplot2' is required.")

  path_base <- file.path(out_dir, "cross_dataset_circos")

  g <- igraph_obj
  igraph::V(g)$dataset <- ifelse(
    igraph::V(g)$name %in% igraph::V(g)$name[
      grepl(paste0("^", dataset_a, "__"), igraph::V(g)$name)
    ], dataset_a, dataset_b
  )

  p <- ggraph::ggraph(g, layout = "linear", circular = TRUE) +
    ggraph::geom_edge_arc(
      ggplot2::aes(color = igraph::E(g)$direction,
                   width = abs(igraph::E(g)$correlation)),
      alpha = 0.5
    ) +
    ggraph::geom_node_point(ggplot2::aes(color = igraph::V(g)$dataset), size = 2) +
    ggplot2::scale_edge_color_manual(
      values = c("positive" = "red", "negative" = "blue")
    ) +
    ggraph::scale_edge_width(range = c(0.2, 1.5)) +
    ggplot2::theme_void() +
    ggplot2::labs(title = paste0("Cross-Dataset Circos: ", dataset_a, " vs ", dataset_b))

  .save_plot(quote(print(p)), path_base)
}

plot_cross_dataset_heatmap <- function(cor_mat, out_dir) {
  if (!requireNamespace("pheatmap", quietly = TRUE))
    stop("Package 'pheatmap' is required.")
  path_base <- file.path(out_dir, "cross_dataset_heatmap")
  .save_plot(quote(
    pheatmap::pheatmap(cor_mat,
                       main = "Cross-Dataset Correlation Heatmap",
                       silent = TRUE)
  ), path_base)
}

# ===========================================================================
# 2.5.1  Single-dataset correlation
# ===========================================================================

run_correlation_single <- function(session_id, params) {
  dataset          <- params$dataset
  method           <- tolower(params$method %||% "pearson")
  threshold        <- as.numeric(params$threshold %||% 0.7)
  direction        <- tolower(params$direction %||% "both")
  rank_metric      <- tolower(params$centrality_rank_metric %||% "degree")

  mat <- tryCatch(
    .load_preprocessed_mat(session_id, dataset),
    error = function(e) NULL
  )
  if (is.null(mat)) {
    return(error_response("NO_FILES_FOUND",
                          paste0("Preprocessed dataset '", dataset, "' not found."),
                          "correlation", recoverable = TRUE))
  }

  n_features <- ncol(mat)

  # --- Safeguard: partial correlation feature limit -------------------------
  if (method == "partial" && n_features > 1000) {
    return(error_response(
      "PARTIAL_CORR_FEATURE_LIMIT",
      paste0("Partial correlation requires \u22641000 features. ",
             "Current count: ", n_features,
             ". Please apply additional filtering before running partial correlation."),
      "correlation", recoverable = TRUE
    ))
  }

  # --- Safeguard: pairwise p-value performance warning ----------------------
  perf_warning <- NULL
  if (n_features > 1500 && method != "partial") {
    perf_warning <- paste0(
      "Computing pairwise p-values for ", n_features,
      " features may take significant time. ",
      "Consider filtering to \u22641500 features for faster results."
    )
  }

  # --- Compute correlation matrix -------------------------------------------
  if (method == "partial") {
    if (!requireNamespace("ppcor", quietly = TRUE))
      stop("Package 'ppcor' is required for partial correlation.")
    pcor_result <- ppcor::pcor(mat)
    cor_mat     <- pcor_result$estimate
    p_mat_raw   <- pcor_result$p.value
    diag(p_mat_raw) <- NA_real_
  } else {
    cor_mat   <- cor(mat, method = method, use = "pairwise.complete.obs")
    p_mat_raw <- .pairwise_cor_test_pvalues(mat, method)
  }

  # --- BH FDR correction ----------------------------------------------------
  fdr_mat <- .fdr_matrix(p_mat_raw)

  # --- Edge list ------------------------------------------------------------
  edge_list <- .build_edge_list(cor_mat, p_mat_raw, fdr_mat, threshold, direction)

  # --- Network metrics ------------------------------------------------------
  net <- .build_network(edge_list, colnames(mat))
  node_metrics <- net$node_metrics

  # Rank by selected metric
  node_metrics <- node_metrics[order(node_metrics[[rank_metric]],
                                     decreasing = TRUE), ]

  # --- Write outputs --------------------------------------------------------
  out_dir <- get_session_path(session_id, "results", "correlation")

  write.csv(as.data.frame(cor_mat), file.path(out_dir, "correlation_matrix.csv"))
  write.csv(edge_list,   file.path(out_dir, "edge_list.csv"),   row.names = FALSE)
  write.csv(node_metrics, file.path(out_dir, "node_metrics.csv"), row.names = FALSE)

  # --- Plots ----------------------------------------------------------------
  plot_correlation_heatmap(cor_mat, out_dir)
  plot_correlation_threshold_matrix(cor_mat, threshold, out_dir)
  if (nrow(edge_list) > 0) {
    plot_correlation_network(net$graph, rank_metric, out_dir)
  }

  # --- Update pipeline state ------------------------------------------------
  state <- read_pipeline_state(session_id)
  state$steps$correlation <- "complete"
  write_pipeline_state(session_id, state)

  ok_response(list(
    dataset          = dataset,
    method           = method,
    threshold        = threshold,
    n_features       = n_features,
    n_edges          = nrow(edge_list),
    performance_warning = perf_warning,
    outputs = list(
      correlation_matrix = "results/correlation/correlation_matrix.csv",
      edge_list          = "results/correlation/edge_list.csv",
      node_metrics       = "results/correlation/node_metrics.csv"
    )
  ))
}

# ===========================================================================
# 2.5.2  Cross-dataset correlation
# ===========================================================================

run_correlation_cross <- function(session_id, params) {
  dataset_a   <- params$dataset_a
  dataset_b   <- params$dataset_b
  method      <- tolower(params$method %||% "pearson")
  threshold   <- as.numeric(params$threshold %||% 0.7)
  direction   <- tolower(params$direction %||% "both")

  # --- Verify preprocessing and scaling consistency -------------------------
  scale_a <- .get_scaling_method(session_id, dataset_a)
  scale_b <- .get_scaling_method(session_id, dataset_b)

  if (is.na(scale_a) || scale_a == "none") {
    return(error_response(
      "CROSS_DATASET_SCALING_MISMATCH",
      paste0("Dataset '", dataset_a,
             "' has not been scaled. Both datasets must be scaled before ",
             "cross-dataset correlation is computed."),
      "correlation", recoverable = TRUE
    ))
  }
  if (is.na(scale_b) || scale_b == "none") {
    return(error_response(
      "CROSS_DATASET_SCALING_MISMATCH",
      paste0("Dataset '", dataset_b,
             "' has not been scaled. Both datasets must be scaled before ",
             "cross-dataset correlation is computed."),
      "correlation", recoverable = TRUE
    ))
  }
  if (scale_a != scale_b) {
    return(error_response(
      "CROSS_DATASET_SCALING_MISMATCH",
      paste0("Scaling methods differ across datasets ('", dataset_a, "': ", scale_a,
             ", '", dataset_b, "': ", scale_b,
             "). Both datasets must use the same scaling method."),
      "correlation", recoverable = TRUE
    ))
  }

  # --- Load matrices --------------------------------------------------------
  mat_a <- tryCatch(.load_preprocessed_mat(session_id, dataset_a), error = function(e) NULL)
  mat_b <- tryCatch(.load_preprocessed_mat(session_id, dataset_b), error = function(e) NULL)

  if (is.null(mat_a)) {
    return(error_response("NO_FILES_FOUND",
                          paste0("Preprocessed dataset '", dataset_a, "' not found."),
                          "correlation", recoverable = TRUE))
  }
  if (is.null(mat_b)) {
    return(error_response("NO_FILES_FOUND",
                          paste0("Preprocessed dataset '", dataset_b, "' not found."),
                          "correlation", recoverable = TRUE))
  }

  # --- Compute cross-dataset correlation matrix -----------------------------
  # cor(mat_a, mat_b) → rows = features of A, cols = features of B
  cross_cor <- cor(mat_a, mat_b, method = method, use = "pairwise.complete.obs")

  # --- Compute p-values via cor.test() for each pair ------------------------
  n_a <- ncol(mat_a)
  n_b <- ncol(mat_b)
  p_mat <- matrix(NA_real_, nrow = n_a, ncol = n_b,
                  dimnames = list(colnames(mat_a), colnames(mat_b)))

  for (i in seq_len(n_a)) {
    for (j in seq_len(n_b)) {
      res <- tryCatch(
        cor.test(mat_a[, i], mat_b[, j], method = method),
        error = function(e) list(p.value = NA_real_)
      )
      p_mat[i, j] <- res$p.value
    }
  }

  # BH FDR on all cross-dataset p-values
  fdr_vec <- p.adjust(as.vector(p_mat), method = "BH")
  fdr_mat <- matrix(fdr_vec, nrow = n_a, ncol = n_b,
                    dimnames = dimnames(p_mat))

  # --- Build cross-dataset edge list ----------------------------------------
  cross_edges <- list()
  for (i in seq_len(n_a)) {
    for (j in seq_len(n_b)) {
      r <- cross_cor[i, j]
      if (is.na(r) || abs(r) < threshold) next
      if (direction == "positive" && r < 0) next
      if (direction == "negative" && r > 0) next
      cross_edges <- c(cross_edges, list(list(
        feature1    = colnames(mat_a)[i],
        feature2    = colnames(mat_b)[j],
        correlation = r,
        p_value     = p_mat[i, j],
        fdr         = fdr_mat[i, j]
      )))
    }
  }

  edge_df <- if (length(cross_edges) > 0) {
    do.call(rbind, lapply(cross_edges, as.data.frame, stringsAsFactors = FALSE))
  } else {
    data.frame(feature1 = character(0), feature2 = character(0),
               correlation = numeric(0), p_value = numeric(0),
               fdr = numeric(0), stringsAsFactors = FALSE)
  }

  # --- Build igraph for cross-dataset network --------------------------------
  all_nodes <- data.frame(
    name    = c(colnames(mat_a), colnames(mat_b)),
    dataset = c(rep(dataset_a, n_a), rep(dataset_b, n_b)),
    stringsAsFactors = FALSE
  )

  node_metrics <- data.frame(
    feature     = all_nodes$name,
    dataset     = all_nodes$dataset,
    degree      = 0L,
    stringsAsFactors = FALSE
  )

  if (nrow(edge_df) > 0) {
    if (!requireNamespace("igraph", quietly = TRUE))
      stop("Package 'igraph' is required.")

    g <- igraph::graph_from_data_frame(
      d        = edge_df[, c("feature1", "feature2")],
      directed = FALSE,
      vertices = all_nodes
    )
    igraph::E(g)$weight      <- abs(edge_df$correlation)
    igraph::E(g)$correlation <- edge_df$correlation
    igraph::E(g)$direction   <- ifelse(edge_df$correlation >= 0, "positive", "negative")

    deg <- igraph::degree(g)
    node_metrics$degree <- as.integer(deg[node_metrics$feature])

    # Identify hub nodes: top 10% by degree
    deg_threshold <- quantile(deg, 0.9)
    node_metrics$is_hub <- node_metrics$degree >= deg_threshold
  } else {
    g <- igraph::make_empty_graph(n = nrow(all_nodes), directed = FALSE)
    igraph::V(g)$name    <- all_nodes$name
    igraph::V(g)$dataset <- all_nodes$dataset
    node_metrics$is_hub  <- FALSE
  }

  # --- Write outputs --------------------------------------------------------
  out_dir <- get_session_path(session_id, "results", "correlation", "cross")

  write.csv(cross_cor, file.path(out_dir, "correlation_matrix.csv"))
  write.csv(edge_df,   file.path(out_dir, "edge_list.csv"),    row.names = FALSE)
  write.csv(node_metrics, file.path(out_dir, "node_metrics.csv"), row.names = FALSE)

  # --- Plots ----------------------------------------------------------------
  plot_cross_dataset_heatmap(cross_cor, out_dir)
  if (nrow(edge_df) > 0) {
    plot_cross_dataset_circos(g, dataset_a, dataset_b, out_dir)
  }

  ok_response(list(
    dataset_a   = dataset_a,
    dataset_b   = dataset_b,
    method      = method,
    threshold   = threshold,
    n_edges     = nrow(edge_df),
    hub_features = node_metrics$feature[isTRUE(node_metrics$is_hub)],
    outputs = list(
      correlation_matrix = "results/correlation/cross/correlation_matrix.csv",
      edge_list          = "results/correlation/cross/edge_list.csv",
      node_metrics       = "results/correlation/cross/node_metrics.csv"
    )
  ))
}

# ---------------------------------------------------------------------------
# Null-coalescing helper (defined here in case module is sourced standalone)
# ---------------------------------------------------------------------------
if (!exists("%||%")) {
  `%||%` <- function(a, b) if (!is.null(a)) a else b
}
