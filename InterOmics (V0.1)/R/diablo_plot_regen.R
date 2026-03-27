# diablo_plot_regen.R
# Lightweight plot-regeneration helpers for DIABLO results.
# These functions load SAVED artifacts (CSVs, model-derived coordinates)
# and regenerate only the requested PNG, overwriting the existing file.
# NO model fitting, tuning, or preprocessing is performed here.
#
# Sourced by plumber.R alongside diablo_module.R.
# Also sources diablo_worker_helpers.R for shared plot primitives.

library(jsonlite)

# ---------------------------------------------------------------------------
# .regen_load_session
# Loads the minimum saved artifacts needed for regeneration.
# Returns list(results_dir, manifest, summary, blocks_needed)
# ---------------------------------------------------------------------------
.regen_load_session <- function(session_id) {
  results_dir  <- get_session_path(session_id, "results", "diablo")
  summary_path <- file.path(results_dir, "diablo_results_summary.json")
  manifest_path <- file.path(results_dir, "results_manifest.json")

  if (!file.exists(summary_path))
    stop("diablo_results_summary.json not found — run DIABLO first")

  summary  <- fromJSON(summary_path, simplifyVector = FALSE)
  manifest <- if (file.exists(manifest_path))
    fromJSON(manifest_path, simplifyVector = FALSE) else list()

  list(results_dir = results_dir, summary = summary, manifest = manifest)
}

# ---------------------------------------------------------------------------
# .regen_load_edge_csv
# Reads a saved edge CSV and returns a data.frame.
# ---------------------------------------------------------------------------
.regen_load_edge_csv <- function(csv_path) {
  if (!file.exists(csv_path)) stop("Edge CSV not found: ", csv_path)
  df <- read.csv(csv_path, stringsAsFactors = FALSE, check.names = FALSE)
  df
}

# ---------------------------------------------------------------------------
# .regen_filter_edges
# Applies abs(correlation_r) >= cutoff filter to an edge data.frame.
# ---------------------------------------------------------------------------
.regen_filter_edges <- function(edge_df, cutoff) {
  if (is.null(edge_df) || nrow(edge_df) == 0) return(edge_df)
  edge_df[abs(as.numeric(edge_df$correlation_r)) >= cutoff, , drop = FALSE]
}

# ---------------------------------------------------------------------------
# .regen_network_plot_from_edges
# Regenerates the network PNG from a (possibly filtered) edge data.frame.
# Positive r = BLUE, Negative r = RED (biological convention).
# ---------------------------------------------------------------------------
.regen_network_plot_from_edges <- function(edge_df, out_dir, block_names,
                                           log_fn = message) {
  if (!requireNamespace("igraph", quietly = TRUE))
    stop("igraph package required for network plot")

  if (is.null(edge_df) || nrow(edge_df) == 0) {
    log_fn("regen_network: no edges after filtering — writing blank plot")
    png_path <- file.path(out_dir, "network.png")
    pdf_path <- file.path(out_dir, "network.pdf")
    for (dev_fn in list(
      list(fn = png,  path = png_path, w = 2200, h = 2200, r = 150),
      list(fn = pdf,  path = pdf_path, w = 14,   h = 14,   r = NULL)
    )) {
      if (is.null(dev_fn$r)) dev_fn$fn(dev_fn$path, width = dev_fn$w, height = dev_fn$h)
      else dev_fn$fn(dev_fn$path, width = dev_fn$w, height = dev_fn$h, res = dev_fn$r)
      plot.new()
      text(0.5, 0.5, "No edges meet the selected correlation cutoff.",
           cex = 1.2, col = "grey50")
      dev.off()
    }
    return(invisible(list(png_path = png_path, pdf_path = pdf_path)))
  }

  all_node_keys <- unique(c(
    paste(edge_df$feature_1, edge_df$block_1, sep = "|||"),
    paste(edge_df$feature_2, edge_df$block_2, sep = "|||")
  ))
  degree_map <- table(c(
    paste(edge_df$feature_1, edge_df$block_1, sep = "|||"),
    paste(edge_df$feature_2, edge_df$block_2, sep = "|||")
  ))

  g <- igraph::graph_from_data_frame(
    d = data.frame(
      from   = paste(edge_df$feature_1, edge_df$block_1, sep = "|||"),
      to     = paste(edge_df$feature_2, edge_df$block_2, sep = "|||"),
      weight = abs(as.numeric(edge_df$correlation_r)),
      r      = as.numeric(edge_df$correlation_r),
      stringsAsFactors = FALSE
    ),
    directed = FALSE,
    vertices = data.frame(name = all_node_keys, stringsAsFactors = FALSE)
  )

  block_palette <- c("#1f77b4","#ff7f0e","#2ca02c","#d62728",
                     "#9467bd","#8c564b","#e377c2","#7f7f7f")
  block_col_map <- setNames(
    block_palette[seq_along(block_names) %% length(block_palette) + 1],
    block_names
  )

  v_names  <- igraph::V(g)$name
  v_blocks <- sapply(v_names, function(nd) {
    parts <- strsplit(nd, "|||", fixed = TRUE)[[1]]
    if (length(parts) > 1) parts[2] else ""
  })
  v_degree <- as.integer(degree_map[v_names])
  deg_min  <- max(1L, min(v_degree))
  deg_max  <- max(deg_min + 1L, max(v_degree))
  v_size   <- 8 + 20 * (v_degree - deg_min) / (deg_max - deg_min)
  v_col    <- ifelse(v_blocks %in% names(block_col_map),
                     block_col_map[v_blocks], "#aaaaaa")

  e_r     <- igraph::E(g)$r
  e_abs   <- igraph::E(g)$weight
  e_width <- 0.5 + 3.5 * e_abs
  # CONVENTION: positive r = BLUE, negative r = RED
  e_col   <- ifelse(e_r >= 0,
                    grDevices::adjustcolor("#1f77b4", alpha.f = 0.7),
                    grDevices::adjustcolor("#d62728", alpha.f = 0.7))

  set.seed(42L)
  layout_mat <- igraph::layout_with_fr(g, weights = e_abs)

  v_labels <- sapply(v_names, function(nd) {
    feat <- strsplit(nd, "|||", fixed = TRUE)[[1]][1]
    if (nchar(feat) > 18) paste0(substr(feat, 1, 16), "..") else feat
  })

  .save_plot(out_dir, "network",
    {
      op <- par(mar = c(1, 1, 2, 1), bg = "white")
      on.exit(par(op), add = TRUE)
      igraph::plot.igraph(
        g,
        layout             = layout_mat,
        vertex.size        = v_size,
        vertex.color       = v_col,
        vertex.label       = v_labels,
        vertex.label.cex   = 0.55,
        vertex.label.color = "#1a1a24",
        vertex.frame.color = NA,
        edge.width         = e_width,
        edge.color         = e_col,
        edge.curved        = 0.15,
        main               = "DIABLO - Feature Correlation Network"
      )
      legend("bottomleft", legend = block_names,
             fill = block_col_map[block_names],
             border = NA, bty = "n", cex = 0.7, title = "Block")
      legend("bottomright",
             legend = c("Positive r", "Negative r"),
             col    = c(grDevices::adjustcolor("#1f77b4", alpha.f = 0.7),
                        grDevices::adjustcolor("#d62728", alpha.f = 0.7)),
             lwd = 2, bty = "n", cex = 0.7)
    },
    width_px = 2200, height_px = 2200, res = 150,
    width_in = 14,   height_in = 14)
}

# ---------------------------------------------------------------------------
# regen_network
# Entry point: regenerate network PNG from saved edge CSV.
# params: list(cutoff = numeric)
# ---------------------------------------------------------------------------
regen_network <- function(session_id, params) {
  ctx <- tryCatch(.regen_load_session(session_id), error = function(e)
    return(error_response("REGEN_LOAD_FAILED", conditionMessage(e), "diablo", TRUE)))
  if (!is.null(ctx$error_code)) return(ctx)

  cutoff      <- max(0, min(1, as.numeric(params$cutoff %||% 0)))
  results_dir <- ctx$results_dir
  block_names <- as.character(unlist(ctx$summary$block_names))

  edge_csv <- file.path(results_dir, "network_edges.csv")
  edge_df  <- tryCatch(.regen_load_edge_csv(edge_csv),
                       error = function(e) return(
                         error_response("REGEN_EDGE_CSV_MISSING",
                                        conditionMessage(e), "diablo", TRUE)))
  if (!is.null(edge_df$error_code)) return(edge_df)

  filtered <- .regen_filter_edges(edge_df, cutoff)

  # Rewrite filtered edge CSV for table display
  filtered_csv <- file.path(results_dir, "network_edges_filtered.csv")
  tryCatch(write.csv(filtered, filtered_csv, row.names = FALSE),
           error = function(e) message("[regen_network] filtered CSV write failed: ", e))

  result <- tryCatch(
    .regen_network_plot_from_edges(filtered, results_dir, block_names),
    error = function(e) return(
      error_response("REGEN_NETWORK_FAILED", conditionMessage(e), "diablo", TRUE))
  )
  if (!is.null(result$error_code)) return(result)

  ok_response(list(
    status    = "regenerated",
    plot      = "network",
    cutoff    = cutoff,
    n_edges   = nrow(filtered),
    timestamp = .now_utc()
  ))
}

# ---------------------------------------------------------------------------
# regen_circos
# Regenerates circos PNG from saved edge CSV using mixOmics::circosPlot
# with a reconstructed minimal model-like structure, OR falls back to a
# custom chord-diagram approach using the edge CSV directly.
# params: list(cutoff = numeric)
# ---------------------------------------------------------------------------
regen_circos <- function(session_id, params) {
  ctx <- tryCatch(.regen_load_session(session_id), error = function(e)
    return(error_response("REGEN_LOAD_FAILED", conditionMessage(e), "diablo", TRUE)))
  if (!is.null(ctx$error_code)) return(ctx)

  cutoff      <- max(0, min(1, as.numeric(params$cutoff %||% 0.3)))
  results_dir <- ctx$results_dir
  block_names <- as.character(unlist(ctx$summary$block_names))

  # Use the full (unfiltered) circos edge CSV as source of truth
  edge_csv <- file.path(results_dir, "circos_edges.csv")
  # Fall back to network edges if circos CSV not present
  if (!file.exists(edge_csv)) edge_csv <- file.path(results_dir, "network_edges.csv")

  edge_df <- tryCatch(.regen_load_edge_csv(edge_csv),
                      error = function(e) return(
                        error_response("REGEN_EDGE_CSV_MISSING",
                                       conditionMessage(e), "diablo", TRUE)))
  if (!is.null(edge_df$error_code)) return(edge_df)

  filtered <- .regen_filter_edges(edge_df, cutoff)

  # Rewrite filtered edge CSV for table display
  filtered_csv <- file.path(results_dir, "circos_edges_filtered.csv")
  tryCatch(write.csv(filtered, filtered_csv, row.names = FALSE),
           error = function(e) message("[regen_circos] filtered CSV write failed: ", e))

  result <- tryCatch(
    .regen_circos_from_edges(filtered, results_dir, block_names, cutoff),
    error = function(e) return(
      error_response("REGEN_CIRCOS_FAILED", conditionMessage(e), "diablo", TRUE))
  )
  if (!is.null(result$error_code)) return(result)

  ok_response(list(
    status    = "regenerated",
    plot      = "circos",
    cutoff    = cutoff,
    n_edges   = nrow(filtered),
    timestamp = .now_utc()
  ))
}

# ---------------------------------------------------------------------------
# .regen_circos_from_edges
# Custom chord-diagram circos plot from edge data.frame.
# Uses base R graphics (no mixOmics circosPlot dependency).
# Positive r = BLUE, Negative r = RED.
# ---------------------------------------------------------------------------
.regen_circos_from_edges <- function(edge_df, out_dir, block_names,
                                     cutoff, log_fn = message) {
  png_path <- file.path(out_dir, "circos.png")
  pdf_path <- file.path(out_dir, "circos.pdf")

  .draw_circos <- function() {
    if (is.null(edge_df) || nrow(edge_df) == 0) {
      plot.new()
      text(0.5, 0.5, paste0("No edges meet cutoff |r| \u2265 ", round(cutoff, 2)),
           cex = 1.2, col = "grey50")
      return(invisible(NULL))
    }

    block_palette <- c("#1f77b4","#ff7f0e","#2ca02c","#d62728",
                       "#9467bd","#8c564b","#e377c2","#7f7f7f")
    block_col_map <- setNames(
      block_palette[seq_along(block_names) %% length(block_palette) + 1],
      block_names
    )

    # Collect all unique features per block
    feat_block <- unique(rbind(
      data.frame(feature = edge_df$feature_1, block = edge_df$block_1,
                 stringsAsFactors = FALSE),
      data.frame(feature = edge_df$feature_2, block = edge_df$block_2,
                 stringsAsFactors = FALSE)
    ))
    feat_block <- feat_block[order(feat_block$block, feat_block$feature), ]

    n_feat  <- nrow(feat_block)
    if (n_feat == 0) { plot.new(); return(invisible(NULL)) }

    # Assign angular positions
    angles <- seq(0, 2 * pi, length.out = n_feat + 1)[seq_len(n_feat)]
    feat_block$angle <- angles
    feat_block$key   <- paste(feat_block$feature, feat_block$block, sep = "|||")

    op <- par(mar = c(1, 1, 2, 1))
    on.exit(par(op), add = TRUE)
    plot.new()
    plot.window(xlim = c(-1.3, 1.3), ylim = c(-1.3, 1.3), asp = 1)
    title(main = paste0("DIABLO - Circos Plot (|r| \u2265 ", round(cutoff, 2), ")"),
          cex.main = 0.9)

    # Draw block arcs
    for (b in block_names) {
      idx <- which(feat_block$block == b)
      if (length(idx) == 0) next
      a_start <- feat_block$angle[min(idx)] - pi / n_feat
      a_end   <- feat_block$angle[max(idx)] + pi / n_feat
      arc_a   <- seq(a_start, a_end, length.out = 60)
      r_arc   <- 1.05
      polygon(c(cos(arc_a) * r_arc, cos(rev(arc_a)) * (r_arc + 0.06)),
              c(sin(arc_a) * r_arc, sin(rev(arc_a)) * (r_arc + 0.06)),
              col = block_col_map[b], border = NA)
    }

    # Draw chord links
    for (i in seq_len(nrow(edge_df))) {
      k1 <- paste(edge_df$feature_1[i], edge_df$block_1[i], sep = "|||")
      k2 <- paste(edge_df$feature_2[i], edge_df$block_2[i], sep = "|||")
      r1 <- feat_block$angle[feat_block$key == k1]
      r2 <- feat_block$angle[feat_block$key == k2]
      if (length(r1) == 0 || length(r2) == 0) next
      rv <- as.numeric(edge_df$correlation_r[i])
      # CONVENTION: positive r = BLUE, negative r = RED
      lc <- if (rv >= 0) grDevices::adjustcolor("#1f77b4", alpha.f = 0.4)
            else          grDevices::adjustcolor("#d62728", alpha.f = 0.4)
      lw <- 0.5 + 2.5 * abs(rv)
      x1 <- cos(r1); y1 <- sin(r1)
      x2 <- cos(r2); y2 <- sin(r2)
      # Bezier-like curve via xspline
      xspline(c(x1, 0, x2), c(y1, 0, y2),
              shape = c(0, 1, 0), open = TRUE,
              lwd = lw, col = lc, border = NA)
    }

    # Draw feature points
    points(cos(feat_block$angle), sin(feat_block$angle),
           pch = 16, cex = 0.6,
           col = block_col_map[feat_block$block])

    # Legend
    legend("bottomleft", legend = block_names,
           fill = block_col_map[block_names],
           border = NA, bty = "n", cex = 0.65, title = "Block")
    legend("bottomright",
           legend = c("Positive r", "Negative r"),
           col    = c(grDevices::adjustcolor("#1f77b4", alpha.f = 0.7),
                      grDevices::adjustcolor("#d62728", alpha.f = 0.7)),
           lwd = 2, bty = "n", cex = 0.65)
  }

  png(png_path, width = 1600, height = 1600, res = 150)
  tryCatch(.draw_circos(), finally = dev.off())

  pdf(pdf_path, width = 11, height = 11)
  tryCatch(.draw_circos(), finally = dev.off())

  log_fn("regen_circos: saved circos.png (", nrow(edge_df %||% data.frame()), " edges)")
  invisible(list(png_path = png_path, pdf_path = pdf_path))
}

# ---------------------------------------------------------------------------
# regen_scores_plot
# Regenerates a scores PNG (multiblock consensus OR single block).
# Loads saved score coordinates from CSV (written by worker).
# params: list(
#   plot_id   = "multiblock_scores" | "block_{safe_b}_scores",
#   colors    = named list(group -> hex),   # optional
#   show_labels = TRUE/FALSE                # optional
# )
# ---------------------------------------------------------------------------
regen_scores_plot <- function(session_id, params) {
  ctx <- tryCatch(.regen_load_session(session_id), error = function(e)
    return(error_response("REGEN_LOAD_FAILED", conditionMessage(e), "diablo", TRUE)))
  if (!is.null(ctx$error_code)) return(ctx)

  plot_id     <- params$plot_id %||% "multiblock_scores"
  show_labels <- isTRUE(params$show_labels)
  user_colors <- params$colors  # named list or NULL
  results_dir <- ctx$results_dir
  block_names <- as.character(unlist(ctx$summary$block_names))
  groups_used <- as.character(unlist(ctx$summary$selected_groups))

  # Determine which scores CSV to load
  scores_csv <- file.path(results_dir, paste0(plot_id, "_coords.csv"))

  if (!file.exists(scores_csv)) {
    return(error_response("REGEN_SCORES_CSV_MISSING",
                          paste0("Scores coordinates CSV not found: ", scores_csv,
                                 ". Re-run DIABLO to generate regeneration-ready artifacts."),
                          "diablo", TRUE))
  }

  coords <- tryCatch(read.csv(scores_csv, stringsAsFactors = FALSE, check.names = FALSE),
                     error = function(e) return(
                       error_response("REGEN_SCORES_CSV_READ_FAILED",
                                      conditionMessage(e), "diablo", TRUE)))
  if (!is.null(coords$error_code)) return(coords)

  # Build colour map
  palette_cols <- c("#388ECC","#F68B33","#C2C2C2","#009E73","#CC79A7",
                    "#56B4E9","#E69F00","#D55E00","#0072B2","#F0E442")
  group_levels <- unique(coords$group)
  group_col_map <- setNames(
    palette_cols[seq_along(group_levels) %% length(palette_cols) + 1],
    group_levels
  )
  # Apply user overrides
  if (!is.null(user_colors) && is.list(user_colors)) {
    for (grp in names(user_colors)) {
      col_val <- trimws(as.character(user_colors[[grp]]))
      if (grepl("^#[0-9A-Fa-f]{6}$", col_val) && grp %in% names(group_col_map))
        group_col_map[grp] <- col_val
    }
  }

  result <- tryCatch(
    .regen_draw_scores(coords, group_col_map, show_labels,
                       plot_id, results_dir),
    error = function(e) return(
      error_response("REGEN_SCORES_DRAW_FAILED", conditionMessage(e), "diablo", TRUE))
  )
  if (!is.null(result$error_code)) return(result)

  ok_response(list(
    status    = "regenerated",
    plot      = plot_id,
    timestamp = .now_utc()
  ))
}

# ---------------------------------------------------------------------------
# .regen_draw_scores
# Draws a 2D scores plot from a coords data.frame.
# coords columns: sample_id, group, x, y
# ---------------------------------------------------------------------------
.regen_draw_scores <- function(coords, group_col_map, show_labels,
                                plot_id, out_dir) {
  group_levels <- names(group_col_map)
  x_coords     <- as.numeric(coords$x)
  y_coords     <- as.numeric(coords$y)
  groups       <- coords$group
  sample_ids   <- coords$sample_id
  point_cols   <- group_col_map[groups]

  xlab <- if ("xlab" %in% colnames(coords)) coords$xlab[1] else "Component 1"
  ylab <- if ("ylab" %in% colnames(coords)) coords$ylab[1] else "Component 2"
  main_title <- if (grepl("^block_", plot_id)) {
    b <- sub("^block_(.+)_scores$", "\\1", plot_id)
    paste0("2D Scores Plot - ", gsub("_", " ", b))
  } else "2D Scores Plot - Multi-Block sPLS-DA"

  .compute_ellipse_local <- function(idx) {
    tryCatch({
      xy  <- cbind(x_coords[idx], y_coords[idx])
      cxy <- cov(xy)
      if (!is.finite(det(cxy)) || det(cxy) < .Machine$double.eps) return(NULL)
      mu    <- colMeans(xy)
      eig   <- eigen(cxy, symmetric = TRUE)
      scale <- sqrt(qchisq(0.95, df = 2) * pmax(eig$values, 0))
      theta <- seq(0, 2 * pi, length.out = 200)
      t(mu + eig$vectors %*% (scale * rbind(cos(theta), sin(theta))))
    }, error = function(e) NULL)
  }

  ellipse_list <- setNames(
    lapply(group_levels, function(grp) {
      idx <- which(groups == grp)
      if (length(idx) >= 3) .compute_ellipse_local(idx) else NULL
    }),
    group_levels
  )

  all_x <- x_coords; all_y <- y_coords
  for (ell in ellipse_list) {
    if (!is.null(ell)) { all_x <- c(all_x, ell[,1]); all_y <- c(all_y, ell[,2]) }
  }
  pad   <- 0.05
  x_rng <- range(all_x, finite = TRUE); y_rng <- range(all_y, finite = TRUE)
  x_pad <- (x_rng[2] - x_rng[1]) * pad; y_pad <- (y_rng[2] - y_rng[1]) * pad
  xlim  <- c(x_rng[1] - x_pad, x_rng[2] + x_pad)
  ylim  <- c(y_rng[1] - y_pad, y_rng[2] + y_pad)

  .render <- function() {
    op <- par(mar = c(5, 5, 4, 8), xpd = FALSE)
    on.exit(par(op), add = TRUE)
    plot(x_coords, y_coords, type = "n",
         xlab = xlab, ylab = ylab, main = main_title,
         xlim = xlim, ylim = ylim, bty = "l", las = 1)
    abline(h = 0, v = 0, col = "grey80", lty = 2, lwd = 0.8)
    for (grp in group_levels) {
      ell <- ellipse_list[[grp]]
      if (!is.null(ell)) {
        polygon(ell[,1], ell[,2],
                col = grDevices::adjustcolor(group_col_map[grp], alpha.f = 0.12),
                border = NA)
        lines(ell, col = group_col_map[grp], lwd = 1.5)
      }
    }
    points(x_coords, y_coords, pch = 16, cex = 2.0, col = point_cols)
    if (show_labels) {
      text(x_coords, y_coords, labels = sample_ids,
           cex = 0.6, pos = 3, col = "#333333")
    }
    par(xpd = TRUE)
    legend("topright", inset = c(-0.18, 0),
           legend = group_levels, col = group_col_map[group_levels],
           pch = 16, pt.cex = 1.4, cex = 0.85, bty = "n", title = "Group")
  }

  .save_plot(out_dir, plot_id, .render(),
             width_px = 1400, height_px = 1000, res = 150,
             width_in = 9, height_in = 7)
}

# ---------------------------------------------------------------------------
# regen_loadings_plot
# Regenerates a single-block loadings PNG from the saved loadings CSV.
# params: list(
#   plot_id = "block_{safe_b}_loadings",
#   top_n   = integer
# )
# ---------------------------------------------------------------------------
regen_loadings_plot <- function(session_id, params) {
  ctx <- tryCatch(.regen_load_session(session_id), error = function(e)
    return(error_response("REGEN_LOAD_FAILED", conditionMessage(e), "diablo", TRUE)))
  if (!is.null(ctx$error_code)) return(ctx)

  plot_id     <- params$plot_id %||% ""
  results_dir <- ctx$results_dir

  # Derive safe block name from plot_id: "block_{safe_b}_loadings"
  safe_b <- sub("^block_(.+)_loadings$", "\\1", plot_id)
  if (safe_b == plot_id || nchar(safe_b) == 0)
    return(error_response("REGEN_INVALID_PLOT_ID",
                          paste0("Cannot parse block name from plot_id: ", plot_id),
                          "diablo", TRUE))

  csv_path <- file.path(results_dir, paste0("loadings_", safe_b, "_comp1.csv"))
  if (!file.exists(csv_path))
    return(error_response("REGEN_LOADINGS_CSV_MISSING",
                          paste0("Loadings CSV not found: ", csv_path),
                          "diablo", TRUE))

  tbl <- tryCatch(read.csv(csv_path, stringsAsFactors = FALSE, check.names = FALSE),
                  error = function(e) return(
                    error_response("REGEN_LOADINGS_CSV_READ", conditionMessage(e), "diablo", TRUE)))
  if (!is.null(tbl$error_code)) return(tbl)

  # Filter to selected features only, rank by abs_loading
  tbl <- tbl[as.logical(tbl$selected) %in% TRUE | tbl$selected == "TRUE", , drop = FALSE]
  tbl <- tbl[order(abs(as.numeric(tbl$abs_loading)), decreasing = TRUE), ]

  top_n <- suppressWarnings(as.integer(params$top_n))
  if (is.na(top_n) || top_n < 1) top_n <- 30L
  top_n <- min(top_n, nrow(tbl))
  if (top_n == 0)
    return(error_response("REGEN_NO_SELECTED_FEATURES",
                          "No selected features in loadings CSV.", "diablo", TRUE))

  tbl_plot <- tbl[seq_len(top_n), ]
  tbl_plot <- tbl_plot[rev(seq_len(nrow(tbl_plot))), ]  # bottom-to-top

  bar_cols       <- ifelse(as.numeric(tbl_plot$loading) > 0, "#388ECC", "#F68B33")
  plot_height_px <- max(700L, 28L * top_n)
  plot_height_in <- max(5.0, 0.22 * top_n)
  block_label    <- gsub("_", " ", safe_b)

  result <- tryCatch(
    .save_plot(results_dir, plot_id,
      {
        op <- par(mar = c(4, max(8, max(nchar(tbl_plot$feature)) * 0.55), 3, 2))
        on.exit(par(op), add = TRUE)
        barplot(as.numeric(tbl_plot$loading),
                horiz = TRUE, col = bar_cols, border = NA,
                names.arg = tbl_plot$feature,
                las = 1, cex.names = 0.72,
                xlab = paste0("Loading (comp 1) — top ", top_n, " features"),
                main = paste0("Loadings Plot - ", block_label))
        abline(v = 0, col = "grey40", lwd = 1)
        legend("bottomright", legend = c("Positive","Negative"),
               fill = c("#388ECC","#F68B33"), border = NA, bty = "n", cex = 0.8)
      },
      width_px = 1600, height_px = plot_height_px,
      width_in = 10,   height_in = plot_height_in),
    error = function(e) return(
      error_response("REGEN_LOADINGS_DRAW_FAILED", conditionMessage(e), "diablo", TRUE))
  )
  if (!is.null(result$error_code)) return(result)

  ok_response(list(status = "regenerated", plot = plot_id,
                   top_n = top_n, timestamp = .now_utc()))
}

# ---------------------------------------------------------------------------
# regen_consensus_loadings
# Regenerates consensus loadings PNG from saved consensus_loadings.csv.
# params: list(top_n = integer)
# ---------------------------------------------------------------------------
regen_consensus_loadings <- function(session_id, params) {
  ctx <- tryCatch(.regen_load_session(session_id), error = function(e)
    return(error_response("REGEN_LOAD_FAILED", conditionMessage(e), "diablo", TRUE)))
  if (!is.null(ctx$error_code)) return(ctx)

  results_dir <- ctx$results_dir
  csv_path    <- file.path(results_dir, "consensus_loadings.csv")
  if (!file.exists(csv_path))
    return(error_response("REGEN_CSV_MISSING",
                          "consensus_loadings.csv not found.", "diablo", TRUE))

  tbl <- tryCatch(read.csv(csv_path, stringsAsFactors = FALSE, check.names = FALSE),
                  error = function(e) return(
                    error_response("REGEN_CSV_READ", conditionMessage(e), "diablo", TRUE)))
  if (!is.null(tbl$error_code)) return(tbl)

  tbl <- tbl[order(abs(as.numeric(tbl$abs_consensus)), decreasing = TRUE), ]

  top_n <- suppressWarnings(as.integer(params$top_n))
  if (is.na(top_n) || top_n < 1) top_n <- 40L
  top_n <- min(top_n, nrow(tbl))
  if (top_n == 0)
    return(error_response("REGEN_NO_FEATURES", "No features in consensus_loadings.csv.",
                          "diablo", TRUE))

  tbl_plot       <- tbl[rev(seq_len(top_n)), ]
  bar_cols       <- ifelse(tbl_plot$sign == "+", "#388ECC", "#F68B33")
  plot_height_px <- max(700L, 28L * top_n)
  plot_height_in <- max(5.0, 0.22 * top_n)

  result <- tryCatch(
    .save_plot(results_dir, "consensus_loadings",
      {
        op <- par(mar = c(4, max(8, max(nchar(tbl_plot$feature)) * 0.55), 3, 2))
        on.exit(par(op), add = TRUE)
        barplot(as.numeric(tbl_plot$consensus_contribution),
                horiz = TRUE, col = bar_cols, border = NA,
                names.arg = paste0(tbl_plot$feature, " [", tbl_plot$block, "]"),
                las = 1, cex.names = 0.72,
                xlab = paste0("Consensus Contribution — top ", top_n, " features"),
                main = "Loadings Plot - Multi-Block sPLS-DA")
        abline(v = 0, col = "grey40", lwd = 1)
        legend("bottomright", legend = c("Positive","Negative"),
               fill = c("#388ECC","#F68B33"), border = NA, bty = "n", cex = 0.8)
      },
      width_px = 1600, height_px = plot_height_px,
      width_in = 10,   height_in = plot_height_in),
    error = function(e) return(
      error_response("REGEN_CONSENSUS_LOADINGS_FAILED", conditionMessage(e), "diablo", TRUE))
  )
  if (!is.null(result$error_code)) return(result)

  ok_response(list(status = "regenerated", plot = "consensus_loadings",
                   top_n = top_n, timestamp = .now_utc()))
}

# ---------------------------------------------------------------------------
# regen_consensus_vip
# Regenerates consensus VIP PNG from saved consensus_vip.csv.
# params: list(top_n = integer)
# ---------------------------------------------------------------------------
regen_consensus_vip <- function(session_id, params) {
  ctx <- tryCatch(.regen_load_session(session_id), error = function(e)
    return(error_response("REGEN_LOAD_FAILED", conditionMessage(e), "diablo", TRUE)))
  if (!is.null(ctx$error_code)) return(ctx)

  results_dir <- ctx$results_dir
  csv_path    <- file.path(results_dir, "consensus_vip.csv")
  if (!file.exists(csv_path))
    return(error_response("REGEN_CSV_MISSING",
                          "consensus_vip.csv not found.", "diablo", TRUE))

  tbl <- tryCatch(read.csv(csv_path, stringsAsFactors = FALSE, check.names = FALSE),
                  error = function(e) return(
                    error_response("REGEN_CSV_READ", conditionMessage(e), "diablo", TRUE)))
  if (!is.null(tbl$error_code)) return(tbl)

  tbl <- tbl[order(as.numeric(tbl$consensus_vip), decreasing = TRUE), ]

  top_n <- suppressWarnings(as.integer(params$top_n))
  if (is.na(top_n) || top_n < 1) top_n <- 40L
  top_n <- min(top_n, nrow(tbl))
  if (top_n == 0)
    return(error_response("REGEN_NO_FEATURES", "No features in consensus_vip.csv.",
                          "diablo", TRUE))

  tbl_plot       <- tbl[rev(seq_len(top_n)), ]
  plot_height_px <- max(700L, 28L * top_n)
  plot_height_in <- max(5.0, 0.22 * top_n)

  vip_vals <- as.numeric(tbl_plot$consensus_vip)
  x_hi     <- ceiling(max(vip_vals, na.rm = TRUE) / 0.5) * 0.5 + 0.5
  x_lo     <- 0
  tick_at  <- seq(x_lo, x_hi, by = 0.5)
  tick_lbl <- ifelse(tick_at == round(tick_at), as.character(round(tick_at)), "")

  result <- tryCatch(
    .save_plot(results_dir, "consensus_vip",
      {
        op <- par(mar = c(4, max(8, max(nchar(tbl_plot$feature)) * 0.55), 3, 2))
        on.exit(par(op), add = TRUE)
        barplot(vip_vals,
                horiz = TRUE, col = "#7b1c2e", border = NA,
                names.arg = tbl_plot$feature,
                las = 1, cex.names = 0.72,
                xlim = c(x_lo, x_hi), xaxt = "n",
                xlab = paste0("Consensus VIP — top ", top_n, " features"),
                main = "Consensus VIP - Multi-Block sPLS-DA")
        axis(1, at = tick_at, labels = tick_lbl, las = 1, cex.axis = 0.85)
        abline(v = 0, col = "grey40", lwd = 1)
      },
      width_px = 1600, height_px = plot_height_px,
      width_in = 10,   height_in = plot_height_in),
    error = function(e) return(
      error_response("REGEN_VIP_FAILED", conditionMessage(e), "diablo", TRUE))
  )
  if (!is.null(result$error_code)) return(result)

  ok_response(list(status = "regenerated", plot = "consensus_vip",
                   top_n = top_n, timestamp = .now_utc()))
}
