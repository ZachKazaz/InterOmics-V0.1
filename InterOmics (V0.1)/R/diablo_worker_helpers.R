# diablo_worker_helpers.R
# Plot and edge helpers for the DIABLO background worker.
# Sourced by run_diablo_worker() INSTEAD of the full diablo_module.R so the
# worker subprocess only loads what it actually needs.
#
# Dependencies (must already be loaded before sourcing this file):
#   - mixOmics  (for plotIndiv, plotLoadings, circosPlot)
#   - igraph    (for network plot; checked with requireNamespace)
#   - jsonlite  (write_json / fromJSON)
#   - utils.R   (get_session_path, .now_utc, %||%)

# ---------------------------------------------------------------------------
# .save_plot
# ---------------------------------------------------------------------------
.save_plot <- function(out_dir, filename_base, plot_expr,
                       width_px = 1400, height_px = 1000, res = 150,
                       width_in = 9, height_in = 7) {
  png_path <- file.path(out_dir, paste0(filename_base, ".png"))
  pdf_path <- file.path(out_dir, paste0(filename_base, ".pdf"))

  png(png_path, width = width_px, height = height_px, res = res)
  tryCatch(force(plot_expr), finally = dev.off())

  pdf(pdf_path, width = width_in, height = height_in)
  tryCatch(force(plot_expr), finally = dev.off())

  invisible(list(png_path = png_path, pdf_path = pdf_path))
}

# ---------------------------------------------------------------------------
# .choose_score_block  [REMOVED — not used]
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# .compute_weighted_average_scores
# Manually computes the weighted-average multiblock sample scores.
#
# VERIFIED: mixOmics 6.26.0 block.splsda stores per-block variates in
# model$variates (slots named after each X block, preserving original names).
# model$weights is a DATA FRAME [blocks x ncomp] — rownames have spaces
# converted to dots (e.g. "DIABLO Serum Lipids" → "DIABLO.Serum.Lipids").
# names(model$X) preserves the original names with spaces.
#
# ROOT CAUSE OF "missing value where TRUE/FALSE needed":
#   model$weights["DIABLO Serum Lipids", 1] returns NA because the data.frame
#   rowname is "DIABLO.Serum.Lipids" (dots). Name-based indexing fails silently.
#   w_total accumulates NA, and if(w_total > 0) throws the error.
#
# FIX: Use positional indexing model$weights[i, comp] where i is the block's
# position in names(model$X), not the block name string.
#
# Formula:
#   wa[sample, comp] = sum_b(w_b * variates[[b]][sample, comp])
#                      / sum_b(w_b)
# where the sum is over X blocks only (not Y), using positional weights.
# ---------------------------------------------------------------------------
.compute_weighted_average_scores <- function(model) {
  block_names <- names(model$X)
  ncomp       <- model$ncomp[1]

  # Validate that at least one block has variates
  first_valid <- NULL
  for (b in block_names) {
    v <- model$variates[[b]]
    if (!is.null(v) && is.matrix(v) && nrow(v) > 0) { first_valid <- b; break }
  }
  if (is.null(first_valid)) stop("No valid block variates found in model$variates")

  n_samples  <- nrow(model$variates[[first_valid]])
  sample_ids <- rownames(model$variates[[first_valid]])
  if (is.null(sample_ids)) sample_ids <- paste0("s", seq_len(n_samples))

  wa <- matrix(0.0, nrow = n_samples, ncol = ncomp,
               dimnames = list(sample_ids, paste0("comp", seq_len(ncomp))))

  for (comp in seq_len(ncomp)) {
    w_total    <- 0.0
    contrib    <- numeric(n_samples)
    n_contrib  <- 0L

    for (i in seq_along(block_names)) {
      b <- block_names[i]

      # Positional weight extraction — avoids the dots-vs-spaces rowname mismatch
      w <- tryCatch(as.numeric(model$weights[i, comp]), error = function(e) NA_real_)
      if (!is.finite(w) || w < 0) next

      # Extract score vector for this block and component
      v <- tryCatch(model$variates[[b]], error = function(e) NULL)
      if (is.null(v)) next
      if (!is.matrix(v)) v <- as.matrix(v)
      if (ncol(v) < comp) next  # this block doesn't have component comp

      score <- as.numeric(v[, comp, drop = FALSE])
      if (length(score) != n_samples) next
      if (!all(is.finite(score))) next

      contrib   <- contrib + w * score
      w_total   <- w_total + w
      n_contrib <- n_contrib + 1L
    }

    if (n_contrib == 0L) {
      stop("No valid X-block contributions found for component ", comp,
           ". Check that model$variates and model$weights are populated.")
    }
    if (is.finite(w_total) && w_total > 0) {
      wa[, comp] <- contrib / w_total
    } else {
      wa[, comp] <- contrib  # equal-weight fallback (w_total=0 shouldn't happen)
    }
  }
  wa
}

# ---------------------------------------------------------------------------
# plot_diablo_consensus
# Generates the true multiblock 2D scores plot.
#
# ROOT CAUSE OF BLANK PLOT: plotIndiv(blocks="weighted.average") fails with
# "non-conformable arguments" when ncomp=1 because it tries to plot comp1 vs
# comp2 but comp2 doesn't exist. The real app run typically has ncomp_opt=1
# from tuning, causing a silent blank/failed plot.
#
# FIX: Compute weighted-average coordinates manually using
# .compute_weighted_average_scores(), then render with base R graphics.
# This works for ncomp=1 (plots comp1 on x-axis, zeros on y-axis) and
# ncomp>=2 (plots comp1 vs comp2). Generic for 2-4 blocks.
#
# Explained variance: from model$prop_expl_var[[block]][comp] averaged
# across X blocks, weighted by block weights.
# ---------------------------------------------------------------------------
plot_diablo_consensus <- function(model, out_dir, log_fn = message) {
  log_fn("multiblock scores plot: computing weighted-average coordinates manually")

  block_names <- names(model$X)
  ncomp       <- model$ncomp[1]
  Y           <- model$Y
  groups      <- as.character(Y)
  group_levels <- levels(Y)

  # Compute weighted-average sample scores
  wa <- tryCatch(
    .compute_weighted_average_scores(model),
    error = function(e) {
      stop("Failed to compute weighted-average scores: ", conditionMessage(e))
    }
  )

  # Extract x/y coordinates
  x_coords <- wa[, 1]
  y_coords <- if (ncomp >= 2) wa[, 2] else rep(0.0, nrow(wa))

  # Axis labels: weighted-average explained variance across X blocks
  .wa_expl_var <- function(comp_idx) {
    tryCatch({
      vals <- sapply(block_names, function(b) {
        ev <- model$prop_expl_var[[b]]
        if (length(ev) >= comp_idx) ev[comp_idx] else NA_real_
      })
      w_col <- model$weights[block_names, min(comp_idx, ncol(model$weights))]
      round(weighted.mean(vals, w_col, na.rm = TRUE) * 100, 1)
    }, error = function(e) NA_real_)
  }
  ev1 <- .wa_expl_var(1)
  ev2 <- if (ncomp >= 2) .wa_expl_var(2) else NA_real_

  xlab <- if (!is.na(ev1)) paste0("Component 1 (", ev1, "% expl. var)") else "Component 1"
  ylab <- if (ncomp >= 2 && !is.na(ev2)) paste0("Component 2 (", ev2, "% expl. var)") else "Component 2"

  # Colour palette (consistent with mixOmics defaults)
  palette_cols <- c("#388ECC","#F68B33","#C2C2C2","#009E73","#CC79A7",
                    "#56B4E9","#E69F00","#D55E00","#0072B2","#F0E442")
  group_col_map <- setNames(
    palette_cols[seq_along(group_levels) %% length(palette_cols) + 1],
    group_levels
  )
  point_cols <- group_col_map[groups]

  # Pre-compute ellipse paths for all groups so we can derive axis limits
  # from the full rendered geometry (points + ellipses) before calling plot().
  .compute_ellipse <- function(idx) {
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
      if (length(idx) >= 3) .compute_ellipse(idx) else NULL
    }),
    group_levels
  )

  # Compute axis limits from union of point coords and all valid ellipse coords
  all_x <- x_coords
  all_y <- y_coords
  for (ell in ellipse_list) {
    if (!is.null(ell)) {
      all_x <- c(all_x, ell[, 1])
      all_y <- c(all_y, ell[, 2])
    }
  }
  pad   <- 0.05  # 5% padding so ellipse borders are not flush with axes
  x_rng <- range(all_x, finite = TRUE)
  y_rng <- range(all_y, finite = TRUE)
  x_pad <- (x_rng[2] - x_rng[1]) * pad
  y_pad <- (y_rng[2] - y_rng[1]) * pad
  xlim  <- c(x_rng[1] - x_pad, x_rng[2] + x_pad)
  ylim  <- c(y_rng[1] - y_pad, y_rng[2] + y_pad)

  # Render plot
  .render_wa_plot <- function(with_ellipse) {
    op <- par(mar = c(5, 5, 4, 8), xpd = FALSE)
    on.exit(par(op), add = TRUE)

    # Draw empty plot frame first (type="n") so ellipse fills go under points
    plot(x_coords, y_coords,
         type = "n",
         xlab = xlab, ylab = ylab,
         main = "2D Scores Plot - Multi-Block sPLS-DA",
         xlim = xlim, ylim = ylim,
         bty = "l", las = 1)
    abline(h = 0, v = 0, col = "grey80", lty = 2, lwd = 0.8)

    # Draw shaded ellipse fills first (under points)
    if (with_ellipse) {
      for (grp in group_levels) {
        ell <- ellipse_list[[grp]]
        if (!is.null(ell)) {
          fill_col <- grDevices::adjustcolor(group_col_map[grp], alpha.f = 0.12)
          polygon(ell[, 1], ell[, 2], col = fill_col, border = NA)
        }
      }
    }

    # Draw ellipse borders (above fill, below points)
    if (with_ellipse) {
      for (grp in group_levels) {
        ell <- ellipse_list[[grp]]
        if (!is.null(ell)) {
          lines(ell, col = group_col_map[grp], lwd = 1.5)
        }
      }
    }

    # Draw points on top of ellipses
    points(x_coords, y_coords, pch = 16, cex = 2.0, col = point_cols)

    # Legend outside plot area
    par(xpd = TRUE)
    legend("topright", inset = c(-0.18, 0),
           legend = group_levels,
           col    = group_col_map[group_levels],
           pch    = 16, pt.cex = 1.4, cex = 0.85,
           bty    = "n", title = "Group")
  }

  result <- tryCatch({
    .save_plot(out_dir, "multiblock_scores",
      .render_wa_plot(with_ellipse = TRUE),
      width_px = 1400, height_px = 1000, res = 150,
      width_in = 9, height_in = 7)
  }, error = function(e) {
    log_fn("multiblock plot with ellipse failed (", conditionMessage(e), "), retrying without")
    .save_plot(out_dir, "multiblock_scores",
      .render_wa_plot(with_ellipse = FALSE),
      width_px = 1400, height_px = 1000, res = 150,
      width_in = 9, height_in = 7)
  })

  log_fn("saved multiblock_scores plot (ncomp=", ncomp, ", n_samples=", nrow(wa), ")")

  # Save score coordinates CSV for plot regeneration (no model rerun needed)
  tryCatch({
    coords_df <- data.frame(
      sample_id = rownames(wa),
      group     = groups,
      x         = x_coords,
      y         = y_coords,
      xlab      = xlab,
      ylab      = ylab,
      stringsAsFactors = FALSE
    )
    write.csv(coords_df, file.path(out_dir, "multiblock_scores_coords.csv"),
              row.names = FALSE)
    log_fn("saved multiblock_scores_coords.csv")
  }, error = function(e) log_fn("WARNING: coords CSV failed: ", conditionMessage(e)))

  invisible(result)
}
# Shared edge computation for network AND circos.
#
# DESIGN NOTE on cutoffs:
#   - Network plot uses cutoff = 0 (all edges) to show the full correlation
#     structure. Node/edge aesthetics (size, width, colour) handle density.
#   - Circos plot uses cutoff = 0.7 (fallback 0.3) to show only strong
#     correlations. Circos becomes unreadable with too many edges, so a
#     higher cutoff is intentional and correct.
#   The two tables therefore have different row counts by design.
#
# Returns data.frame with columns:
#   feature_1, block_1, feature_2, block_2, component,
#   correlation_r, abs_correlation, p_value, within_block
# Includes BOTH within-block and between-block pairs for all components.
# ---------------------------------------------------------------------------
compute_diablo_edges <- function(model, cutoff = 0, log_fn = message) {
  ncomp       <- model$ncomp[1]
  block_names <- names(model$X)
  edge_rows   <- list()

  for (comp in seq_len(ncomp)) {
    sel_per_block <- lapply(block_names, function(b) {
      lv <- tryCatch(model$loadings[[b]][, comp], error = function(e) NULL)
      if (is.null(lv)) return(character(0))
      names(lv[lv != 0])
    })
    names(sel_per_block) <- block_names

    for (i in seq_along(block_names)) {
      for (j in seq_along(block_names)) {
        if (j < i) next
        bn1    <- block_names[i]
        bn2    <- block_names[j]
        sel1   <- sel_per_block[[bn1]]
        sel2   <- sel_per_block[[bn2]]
        within <- (i == j)
        if (length(sel1) == 0 || length(sel2) == 0) next

        for (f1 in sel1) {
          f2_set <- if (within) sel2[sel2 > f1] else sel2
          for (f2 in f2_set) {
            if (within && f1 == f2) next
            r <- tryCatch(
              cor(model$X[[bn1]][, f1], model$X[[bn2]][, f2],
                  use = "pairwise.complete.obs"),
              error = function(e) NA_real_
            )
            if (is.na(r) || abs(r) < cutoff) next
            # Compute Pearson p-value via cor.test (same method as cor())
            pv <- tryCatch({
              x1 <- model$X[[bn1]][, f1]
              x2 <- model$X[[bn2]][, f2]
              ok <- complete.cases(x1, x2)
              if (sum(ok) < 3) NA_real_
              else round(cor.test(x1[ok], x2[ok], method = "pearson")$p.value, 6)
            }, error = function(e) NA_real_)
            edge_rows[[length(edge_rows) + 1]] <- data.frame(
              feature_1       = f1,
              block_1         = bn1,
              feature_2       = f2,
              block_2         = bn2,
              component       = paste0("comp", comp),
              correlation_r   = round(r, 4),
              abs_correlation = round(abs(r), 4),
              p_value         = pv,
              within_block    = within,
              stringsAsFactors = FALSE
            )
          }
        }
      }
    }
  }

  if (length(edge_rows) == 0) return(NULL)
  edge_df <- do.call(rbind, edge_rows)
  edge_df <- edge_df[order(edge_df$abs_correlation, decreasing = TRUE), ]
  log_fn("compute_diablo_edges: ", nrow(edge_df), " edges (cutoff=", cutoff, ")")
  edge_df
}

# ---------------------------------------------------------------------------
# plot_diablo_sample_scores
# In block.splsda, model$variates only contains per-block scores (X1, X2, Y).
# The weighted.average IS the multiblock sample scores, computed by plotIndiv.
# No distinct sample-scores artifact exists. Kept for backward compatibility.
# ---------------------------------------------------------------------------
plot_diablo_sample_scores <- function(model, out_dir, log_fn = message) {
  log_fn("plot_diablo_sample_scores: no distinct sample-scores artifact for block.splsda; skipped.")
  invisible(list())
}

# ---------------------------------------------------------------------------
# plot_diablo_blocks
# Per-block x per-component sample plots with shaded 95% confidence ellipses.
# Returns named list keyed by "<block>_comp<n>" -> c(png_path, pdf_path)
# Observations rendered as filled circles (pch=16), ellipses shaded lightly.
# ---------------------------------------------------------------------------
plot_diablo_blocks <- function(model, out_dir, log_fn = message) {
  block_names <- names(model$X)
  ncomp       <- model$ncomp[1]
  all_results <- list()

  Y            <- model$Y
  groups       <- as.character(Y)
  group_levels <- levels(Y)

  palette_cols <- c("#388ECC","#F68B33","#C2C2C2","#009E73","#CC79A7",
                    "#56B4E9","#E69F00","#D55E00","#0072B2","#F0E442")
  group_col_map <- setNames(
    palette_cols[seq_along(group_levels) %% length(palette_cols) + 1],
    group_levels
  )
  point_cols <- group_col_map[groups]

  .block_ellipse <- function(x_c, y_c, idx) {
    tryCatch({
      xy  <- cbind(x_c[idx], y_c[idx])
      cxy <- cov(xy)
      if (!is.finite(det(cxy)) || det(cxy) < .Machine$double.eps) return(NULL)
      mu    <- colMeans(xy)
      eig   <- eigen(cxy, symmetric = TRUE)
      scale <- sqrt(qchisq(0.95, df = 2) * pmax(eig$values, 0))
      theta <- seq(0, 2 * pi, length.out = 200)
      t(mu + eig$vectors %*% (scale * rbind(cos(theta), sin(theta))))
    }, error = function(e) NULL)
  }

  for (b in block_names) {
    safe_b <- gsub("[^A-Za-z0-9_]", "_", b)
    for (comp in seq_len(ncomp)) {
      comp2 <- if (ncomp >= 2) min(comp + 1L, ncomp) else comp

      # Extract score coordinates for this block
      x_c <- tryCatch({
        v <- model$variates[[b]]
        if (!is.matrix(v)) v <- as.matrix(v)
        as.numeric(v[, comp, drop = TRUE])
      }, error = function(e) NULL)

      y_c <- tryCatch({
        v <- model$variates[[b]]
        if (!is.matrix(v)) v <- as.matrix(v)
        if (ncomp >= 2 && ncol(v) >= comp2) as.numeric(v[, comp2, drop = TRUE])
        else rep(0.0, nrow(v))
      }, error = function(e) NULL)

      if (is.null(x_c) || is.null(y_c)) {
        # Fallback to mixOmics plotIndiv if variate extraction fails
        result <- tryCatch({
          comp_arg <- if (ncomp >= 2) c(comp, min(comp + 1L, ncomp)) else comp
          r <- tryCatch(
            .save_plot(out_dir, paste0("block_", safe_b, "_comp", comp),
              mixOmics::plotIndiv(model, blocks = b, comp = comp_arg,
                                  ind.names = FALSE, pch = 16, cex = 2.0,
                                  legend = TRUE, ellipse = TRUE,
                                  title = paste0("2D Scores Plot - ", b))),
            error = function(e) {
              log_fn("block '", b, "' comp", comp, " with ellipse failed, retrying without")
              .save_plot(out_dir, paste0("block_", safe_b, "_comp", comp),
                mixOmics::plotIndiv(model, blocks = b, comp = comp_arg,
                                    ind.names = FALSE, pch = 16, cex = 2.0,
                                    legend = TRUE, ellipse = FALSE,
                                    title = paste0("2D Scores Plot - ", b)))
            })
          log_fn("saved block score plot for '", b, "' comp", comp, " (fallback)")
          r
        }, error = function(e) {
          log_fn("WARNING: block plot failed for '", b, "' comp", comp, ": ", conditionMessage(e))
          NULL
        })
        if (!is.null(result)) all_results[[paste0(b, "_comp", comp)]] <- c(result$png_path, result$pdf_path)
        next
      }

      # Pre-compute ellipses for axis limits
      ellipse_list_b <- setNames(
        lapply(group_levels, function(grp) {
          idx <- which(groups == grp)
          if (length(idx) >= 3) .block_ellipse(x_c, y_c, idx) else NULL
        }),
        group_levels
      )

      all_x <- x_c; all_y <- y_c
      for (ell in ellipse_list_b) {
        if (!is.null(ell)) { all_x <- c(all_x, ell[, 1]); all_y <- c(all_y, ell[, 2]) }
      }
      pad   <- 0.05
      x_rng <- range(all_x, finite = TRUE); y_rng <- range(all_y, finite = TRUE)
      x_pad <- (x_rng[2] - x_rng[1]) * pad; y_pad <- (y_rng[2] - y_rng[1]) * pad
      xlim_b <- c(x_rng[1] - x_pad, x_rng[2] + x_pad)
      ylim_b <- c(y_rng[1] - y_pad, y_rng[2] + y_pad)

      xlab_b <- paste0("Component ", comp)
      ylab_b <- if (ncomp >= 2) paste0("Component ", comp2) else paste0("Component ", comp)

      result <- tryCatch({
        .save_plot(out_dir, paste0("block_", safe_b, "_comp", comp),
          {
            op <- par(mar = c(5, 5, 4, 8), xpd = FALSE)
            on.exit(par(op), add = TRUE)
            plot(x_c, y_c, type = "n",
                 xlab = xlab_b, ylab = ylab_b,
                 main = paste0("2D Scores Plot - ", b),
                 xlim = xlim_b, ylim = ylim_b,
                 bty = "l", las = 1)
            abline(h = 0, v = 0, col = "grey80", lty = 2, lwd = 0.8)
            # Shaded fills first
            for (grp in group_levels) {
              ell <- ellipse_list_b[[grp]]
              if (!is.null(ell)) {
                fill_col <- grDevices::adjustcolor(group_col_map[grp], alpha.f = 0.12)
                polygon(ell[, 1], ell[, 2], col = fill_col, border = NA)
              }
            }
            # Ellipse borders
            for (grp in group_levels) {
              ell <- ellipse_list_b[[grp]]
              if (!is.null(ell)) lines(ell, col = group_col_map[grp], lwd = 1.5)
            }
            # Points on top
            points(x_c, y_c, pch = 16, cex = 2.0, col = point_cols)
            par(xpd = TRUE)
            legend("topright", inset = c(-0.18, 0),
                   legend = group_levels, col = group_col_map[group_levels],
                   pch = 16, pt.cex = 1.4, cex = 0.85, bty = "n", title = "Group")
          },
          width_px = 1400, height_px = 1000, res = 150, width_in = 9, height_in = 7)
      }, error = function(e) {
        log_fn("WARNING: block plot failed for '", b, "' comp", comp, ": ", conditionMessage(e))
        NULL
      })

      if (!is.null(result)) {
        all_results[[paste0(b, "_comp", comp)]] <- c(result$png_path, result$pdf_path)
        log_fn("saved block score plot for '", b, "' comp", comp)

        # Save score coordinates CSV for regeneration
        tryCatch({
          coords_df <- data.frame(
            sample_id = rownames(model$variates[[b]]),
            group     = groups,
            x         = x_c,
            y         = y_c,
            xlab      = xlab_b,
            ylab      = ylab_b,
            stringsAsFactors = FALSE
          )
          write.csv(coords_df,
                    file.path(out_dir, paste0("block_", safe_b, "_scores_coords.csv")),
                    row.names = FALSE)
        }, error = function(e)
          log_fn("WARNING: block coords CSV failed for '", b, "' comp", comp, ": ", conditionMessage(e)))
      }
    }
  }
  invisible(all_results)
}

# ---------------------------------------------------------------------------
# plot_diablo_loadings
# Loadings plots + CSV tables.
# Returns named list keyed by "<block>_comp<n>" -> c(png, pdf)
# and "<block>_comp<n>_csv" -> csv_path
# ---------------------------------------------------------------------------
plot_diablo_loadings <- function(model, out_dir, log_fn = message) {
  ncomp       <- model$ncomp[1]
  all_results <- list()

  for (b in names(model$X)) {
    safe_b <- gsub("[^A-Za-z0-9_]", "_", b)
    for (comp in seq_len(ncomp)) {
      lv <- tryCatch(model$loadings[[b]][, comp], error = function(e) NULL)
      if (is.null(lv) || all(lv == 0)) {
        log_fn("skipped loadings for '", b, "' comp", comp, " (all zero or missing)")
        next
      }

      result <- tryCatch({
        .save_plot(out_dir, paste0("loadings_", safe_b, "_comp", comp),
          mixOmics::plotLoadings(model, comp = comp, block = b,
                                 contrib = "max", method = "median",
                                 title = paste("Loadings -", b, "Comp", comp)),
          width_px = 1400, height_px = max(800, 40 * min(length(lv[lv != 0]), 30)),
          width_in = 9, height_in = max(5, 0.3 * min(length(lv[lv != 0]), 30)))
      }, error = function(e) {
        log_fn("loadings plot failed for '", b, "' comp", comp, ": ", conditionMessage(e))
        NULL
      })
      if (!is.null(result)) {
        all_results[[paste0(b, "_comp", comp)]] <- c(result$png_path, result$pdf_path)
      }

      tryCatch({
        ranked_order <- order(abs(lv), decreasing = TRUE)
        tbl <- data.frame(
          feature     = names(lv)[ranked_order],
          block       = b,
          component   = paste0("comp", comp),
          loading     = lv[ranked_order],
          abs_loading = abs(lv[ranked_order]),
          sign        = ifelse(lv[ranked_order] > 0, "+", "-"),
          rank        = seq_along(ranked_order),
          selected    = lv[ranked_order] != 0,
          stringsAsFactors = FALSE
        )
        csv_path <- file.path(out_dir, paste0("loadings_", safe_b, "_comp", comp, ".csv"))
        write.csv(tbl, csv_path, row.names = FALSE)
        all_results[[paste0(b, "_comp", comp, "_csv")]] <- csv_path
        log_fn("saved loadings CSV for '", b, "' comp", comp)
      }, error = function(e) {
        log_fn("WARNING: loadings CSV failed for '", b, "' comp", comp, ": ", conditionMessage(e))
      })
    }
  }
  invisible(all_results)
}

# ---------------------------------------------------------------------------
# plot_diablo_network
# Custom igraph-based renderer. Uses compute_diablo_edges(cutoff=0) -- all
# edges, full network structure.
# (Circos uses a higher cutoff; see compute_diablo_edges for design rationale.)
#
# Visual encoding:
#   - Node size    : proportional to degree (number of edges), scaled [8, 28]
#   - Node colour  : one colour per omics block
#   - Edge width   : proportional to abs(correlation), scaled [0.5, 4]
#   - Edge colour  : red = positive correlation, blue = negative correlation
#   - Labels       : small (cex 0.55), do not inflate node size
#   - Layout       : Fruchterman-Reingold weighted by abs_correlation
# ---------------------------------------------------------------------------
plot_diablo_network <- function(model, out_dir, log_fn = message) {

  if (!requireNamespace("igraph", quietly = TRUE)) {
    log_fn("igraph not available -- skipping network plot")
    return(invisible(list()))
  }

  result <- list()

  # -- Shared edge data (cutoff = 0: all edges) --
  edge_df <- tryCatch(
    compute_diablo_edges(model, cutoff = 0, log_fn = log_fn),
    error = function(e) { log_fn("edge computation failed: ", conditionMessage(e)); NULL }
  )

  if (is.null(edge_df) || nrow(edge_df) == 0) {
    log_fn("skipped network plot: no edges computed")
    return(invisible(result))
  }

  # -- Edge CSV --
  tryCatch({
    edge_csv <- file.path(out_dir, "network_edges.csv")
    write.csv(edge_df[, c("feature_1","block_1","feature_2","block_2",
                           "component","correlation_r","abs_correlation",
                           "p_value","within_block")],
              edge_csv, row.names = FALSE)
    result$edge_csv <- edge_csv
    log_fn("saved network edge CSV (", nrow(edge_df), " edges)")
  }, error = function(e) log_fn("WARNING: network edge CSV failed: ", conditionMessage(e)))

  # -- Node key helpers --
  all_node_keys <- unique(c(
    paste(edge_df$feature_1, edge_df$block_1, sep = "|||"),
    paste(edge_df$feature_2, edge_df$block_2, sep = "|||")
  ))
  degree_map <- table(c(
    paste(edge_df$feature_1, edge_df$block_1, sep = "|||"),
    paste(edge_df$feature_2, edge_df$block_2, sep = "|||")
  ))

  # -- Build igraph object --
  g <- tryCatch({
    igraph::graph_from_data_frame(
      d = data.frame(
        from   = paste(edge_df$feature_1, edge_df$block_1, sep = "|||"),
        to     = paste(edge_df$feature_2, edge_df$block_2, sep = "|||"),
        weight = edge_df$abs_correlation,
        r      = edge_df$correlation_r,
        stringsAsFactors = FALSE
      ),
      directed = FALSE,
      vertices = data.frame(name = all_node_keys, stringsAsFactors = FALSE)
    )
  }, error = function(e) {
    log_fn("igraph construction failed: ", conditionMessage(e))
    NULL
  })

  if (is.null(g)) return(invisible(result))

  # -- Node metrics --
  ig_btw <- tryCatch(igraph::betweenness(g, normalized = TRUE), error = function(e) NULL)
  ig_cls <- tryCatch(igraph::closeness(g,   normalized = TRUE), error = function(e) NULL)

  node_rows <- lapply(all_node_keys, function(nd) {
    parts <- strsplit(nd, "|||", fixed = TRUE)[[1]]
    feat  <- parts[1]
    blk   <- if (length(parts) > 1) parts[2] else ""
    deg   <- as.integer(degree_map[nd])
    btw   <- if (!is.null(ig_btw)) round(ig_btw[nd], 4) else NA_real_
    cls   <- if (!is.null(ig_cls)) round(ig_cls[nd], 4) else NA_real_
    data.frame(feature = feat, block = blk, degree = deg,
               betweenness = btw, closeness = cls, stringsAsFactors = FALSE)
  })
  node_df      <- do.call(rbind, node_rows)
  node_df      <- node_df[order(node_df$degree, decreasing = TRUE), ]
  node_df$rank <- seq_len(nrow(node_df))

  tryCatch({
    node_csv <- file.path(out_dir, "network_nodes.csv")
    write.csv(node_df, node_csv, row.names = FALSE)
    result$node_csv <- node_csv
    log_fn("saved network node CSV (", nrow(node_df), " nodes)")
  }, error = function(e) log_fn("WARNING: network node CSV failed: ", conditionMessage(e)))

  # -- Custom igraph plot --
  plot_result <- tryCatch({
    block_names   <- names(model$X)
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

    # Node size: scale degree to [8, 28]
    deg_min <- max(1L, min(v_degree))
    deg_max <- max(deg_min + 1L, max(v_degree))
    v_size  <- 8 + 20 * (v_degree - deg_min) / (deg_max - deg_min)

    v_col <- ifelse(v_blocks %in% names(block_col_map),
                    block_col_map[v_blocks], "#aaaaaa")

    e_r     <- igraph::E(g)$r
    e_abs   <- igraph::E(g)$weight
    # Edge width: scale abs_correlation to [0.5, 4]
    e_width <- 0.5 + 3.5 * e_abs
    # Edge colour: BLUE = positive correlation, RED = negative correlation
    e_col   <- ifelse(e_r >= 0,
                      grDevices::adjustcolor("#1f77b4", alpha.f = 0.7),
                      grDevices::adjustcolor("#d62728", alpha.f = 0.7))

    set.seed(42L)
    layout_mat <- igraph::layout_with_fr(g, weights = e_abs)

    # Truncate long feature names for readability
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
        legend("bottomleft",
               legend = block_names,
               fill   = block_col_map[block_names],
               border = NA, bty = "n", cex = 0.7, title = "Block")
        legend("bottomright",
               legend = c("Positive r", "Negative r"),
               col    = c(grDevices::adjustcolor("#1f77b4", alpha.f = 0.7),
                          grDevices::adjustcolor("#d62728", alpha.f = 0.7)),
               lwd = 2, bty = "n", cex = 0.7)
      },
      width_px = 2200, height_px = 2200, res = 150,
      width_in = 14,   height_in = 14)
  }, error = function(e) {
    log_fn("custom igraph network plot failed: ", conditionMessage(e))
    NULL
  })

  if (!is.null(plot_result)) {
    result$png_path <- plot_result$png_path
    result$pdf_path <- plot_result$pdf_path
  }

  log_fn("network artifacts complete")
  invisible(result)
}

# ---------------------------------------------------------------------------
# plot_diablo_consensus_loadings
# Computes and plots the multiblock consensus feature contribution for comp 1.
#
# DIAGNOSIS: plotLoadings(model, comp=1) with no block= argument only
# concatenates per-block comp-1 loadings side by side. It does NOT combine
# them into a single consensus metric. That is NOT a true consensus loading.
#
# CORRECT FORMULA:
#   consensus_contribution[feature] = w_b * |loading_b[feature, comp1]|
#   where w_b = model$weights[b, 1] (the block weight for comp 1)
#   and loading_b[feature, comp1] = model$loadings[[b]][feature, 1]
#
# This gives each feature a single consensus contribution score that reflects
# its contribution to the multiblock consensus direction, weighted by how
# much that block contributes to the overall multiblock model.
#
# Features that exist only in one block get their contribution from that
# block's weighted loading. Features with loading=0 (not selected) get
# consensus_contribution=0.
#
# The sign is preserved from the original loading (positive = positively
# associated with the consensus direction).
#
# The plot is a horizontal bar chart of the top selected features ranked
# by |consensus_contribution|, colored by sign.
# ---------------------------------------------------------------------------
plot_diablo_consensus_loadings <- function(model, out_dir, log_fn = message) {
  result      <- list()
  block_names <- names(model$X)

  # Build consensus contribution table
  # Use positional indexing for model$weights to avoid dots-vs-spaces mismatch
  rows <- list()
  for (i in seq_along(block_names)) {
    b  <- block_names[i]
    lv <- tryCatch(model$loadings[[b]][, 1], error = function(e) NULL)
    if (is.null(lv)) next
    # Positional weight extraction — model$weights rownames use dots, names(model$X) use spaces
    w_b <- tryCatch(as.numeric(model$weights[i, 1]), error = function(e) NA_real_)
    if (!is.finite(w_b) || w_b <= 0) {
      log_fn("WARNING: block '", b, "' weight is NA/non-positive for comp 1; skipping")
      next
    }

    # Only include selected features (non-zero loading)
    selected_idx <- which(lv != 0)
    if (length(selected_idx) == 0) next

    for (j in selected_idx) {
      rows[[length(rows) + 1]] <- data.frame(
        feature                = names(lv)[j],
        block                  = b,
        loading_comp1          = lv[j],
        block_weight_comp1     = round(w_b, 4),
        consensus_contribution = round(w_b * lv[j], 4),
        abs_consensus          = round(w_b * abs(lv[j]), 4),
        sign                   = ifelse(lv[j] > 0, "+", "-"),
        stringsAsFactors       = FALSE
      )
    }
  }

  if (length(rows) == 0) {
    log_fn("consensus loadings: no selected features found for comp 1")
    return(invisible(result))
  }

  tbl <- do.call(rbind, rows)
  tbl <- tbl[order(tbl$abs_consensus, decreasing = TRUE), ]
  tbl$rank <- seq_len(nrow(tbl))

  # Save CSV
  tryCatch({
    csv_path <- file.path(out_dir, "consensus_loadings.csv")
    write.csv(tbl, csv_path, row.names = FALSE)
    result$csv_path <- csv_path
    log_fn("saved consensus_loadings CSV (", nrow(tbl), " selected features)")
  }, error = function(e) {
    log_fn("WARNING: consensus loadings CSV failed: ", conditionMessage(e))
  })

  # Plot: horizontal bar chart of top features by |consensus_contribution|
  n_show <- min(nrow(tbl), 40L)
  tbl_plot <- tbl[seq_len(n_show), ]
  # Reverse for bottom-to-top display
  tbl_plot <- tbl_plot[rev(seq_len(nrow(tbl_plot))), ]

  bar_cols <- ifelse(tbl_plot$sign == "+", "#388ECC", "#F68B33")
  plot_height_px <- max(700L, 28L * n_show)
  plot_height_in <- max(5.0, 0.22 * n_show)

  plot_result <- tryCatch({
    .save_plot(out_dir, "consensus_loadings",
      {
        op <- par(mar = c(4, max(8, max(nchar(tbl_plot$feature)) * 0.55), 3, 2))
        on.exit(par(op), add = TRUE)
        bp <- barplot(tbl_plot$consensus_contribution,
                      horiz    = TRUE,
                      col      = bar_cols,
                      border   = NA,
                      names.arg = tbl_plot$feature,
                      las      = 1,
                      cex.names = 0.72,
                      xlab     = "Consensus Contribution (block weight × loading, comp 1)",
                      main     = "Loadings Plot - Multi-Block sPLS-DA")
        abline(v = 0, col = "grey40", lwd = 1)
        legend("bottomright",
               legend = c("Positive", "Negative"),
               fill   = c("#388ECC", "#F68B33"),
               border = NA, bty = "n", cex = 0.8)
      },
      width_px = 1600, height_px = plot_height_px,
      width_in = 10,   height_in = plot_height_in)
  }, error = function(e) {
    log_fn("consensus loadings plot failed: ", conditionMessage(e))
    NULL
  })

  if (!is.null(plot_result)) {
    result$png_path <- plot_result$png_path
    result$pdf_path <- plot_result$pdf_path
    log_fn("saved consensus_loadings plot (", n_show, " features shown)")
  }

  invisible(result)
}

# ---------------------------------------------------------------------------
# plot_diablo_consensus_vip
# Computes and plots the multiblock consensus VIP for component 1.
#
# FORMULA:
#   consensus_vip(feature in block b) = block_weight_b_comp1 × vip_b(feature, comp1)
#
# Where:
#   - vip_b(feature, comp1) is extracted from mixOmics::vip(model) for block b,
#     column comp1. mixOmics::vip() on a block.splsda object returns a named
#     list (one matrix per X block), each matrix [features × ncomp].
#   - block_weight_b_comp1 = model$weights[i, 1] using positional indexing
#     (avoids dots-vs-spaces rowname mismatch).
#
# Only X blocks are used. Y block is excluded.
# No normalisation is applied after weighting.
# ---------------------------------------------------------------------------
plot_diablo_consensus_vip <- function(model, out_dir, log_fn = message) {
  result      <- list()
  block_names <- names(model$X)

  # Guard: model$weights must exist and have at least one row
  if (is.null(model$weights) || nrow(model$weights) == 0) {
    log_fn("consensus VIP: model$weights is NULL or empty — skipping")
    return(invisible(result))
  }

  # -- Compute per-block VIP manually --
  # mixOmics::vip() does not support block.splsda objects.
  # VIP for sPLS-DA (per block, comp 1 only) is computed as:
  #   VIP_j = sqrt( p * sum_h( (R2_h / R2_total) * w_jh^2 ) )
  # where:
  #   p       = number of features in the block
  #   R2_h    = R^2 between the h-th variate and Y (approximated by
  #             the squared correlation between variate and each Y dummy,
  #             summed across classes)
  #   R2_total = sum of R2_h across all components
  #   w_jh    = loading weight for feature j on component h
  #             (from model$loadings[[b]][j, h])
  #
  # For comp 1 only (h=1), this simplifies to:
  #   VIP_j = sqrt( p * w_j1^2 )  (since R2_1/R2_total = 1 when ncomp=1)
  # For ncomp > 1 we use the full formula.
  #
  # This is the standard VIP formula used in mixOmics for splsda objects.

  .compute_block_vip <- function(b, comp_max) {
    tryCatch({
      lmat <- model$loadings[[b]]
      if (is.null(lmat)) return(NULL)
      if (!is.matrix(lmat)) lmat <- as.matrix(lmat)
      p     <- nrow(lmat)
      ncomp <- min(comp_max, ncol(lmat))
      if (p == 0 || ncomp == 0) return(NULL)

      # Build Y dummy matrix defensively (avoid model.matrix formula issues)
      Y_fac  <- model$Y
      Y_lvls <- levels(Y_fac)
      Y_mat  <- do.call(cbind, lapply(Y_lvls, function(lv) as.integer(Y_fac == lv)))
      colnames(Y_mat) <- Y_lvls

      # Compute R2 per component: sum of squared correlations between variate and Y columns
      r2_h  <- numeric(ncomp)
      for (h in seq_len(ncomp)) {
        vt <- tryCatch({
          v <- model$variates[[b]]
          if (!is.matrix(v)) v <- as.matrix(v)
          if (ncol(v) < h) return(NULL)
          as.numeric(v[, h, drop = TRUE])
        }, error = function(e) NULL)
        if (is.null(vt) || length(vt) != nrow(Y_mat) || !all(is.finite(vt))) {
          r2_h[h] <- 0
          next
        }
        r2_h[h] <- sum(apply(Y_mat, 2, function(y) {
          tryCatch(cor(vt, y)^2, error = function(e) 0)
        }))
      }
      r2_total <- sum(r2_h)
      if (!is.finite(r2_total) || r2_total <= 0) {
        # Fallback: equal weight across components
        r2_h     <- rep(1.0, ncomp)
        r2_total <- ncomp
      }

      # VIP for each feature: sqrt(p * sum_h((R2_h/R2_total) * w_jh^2))
      vip_vals <- numeric(p)
      for (j in seq_len(p)) {
        vip_vals[j] <- sqrt(p * sum((r2_h / r2_total) * lmat[j, seq_len(ncomp), drop = TRUE]^2))
      }
      names(vip_vals) <- rownames(lmat)
      vip_vals
    }, error = function(e) {
      log_fn("WARNING: VIP computation failed for block '", b, "': ", conditionMessage(e))
      NULL
    })
  }

  ncomp_model <- model$ncomp[1]

  # -- Build consensus VIP table --
  rows <- list()
  for (i in seq_along(block_names)) {
    b <- block_names[i]

    # Positional weight extraction — avoids dots-vs-spaces rowname mismatch
    w_b <- tryCatch({
      wmat <- model$weights
      if (is.null(wmat) || nrow(wmat) < i || ncol(wmat) < 1) NA_real_
      else as.numeric(wmat[i, 1])
    }, error = function(e) NA_real_)

    if (!is.finite(w_b) || w_b <= 0) {
      log_fn("WARNING: block '", b, "' weight is NA/non-positive for comp 1 (w_b=",
             w_b, "); skipping VIP")
      next
    }

    vip_vec <- .compute_block_vip(b, ncomp_model)
    if (is.null(vip_vec) || length(vip_vec) == 0) {
      log_fn("WARNING: no VIP values for block '", b, "'; skipping")
      next
    }

    # Only include features with non-zero loading (selected by sPLS-DA)
    lv <- tryCatch({
      lmat <- model$loadings[[b]]
      if (is.null(lmat)) return(NULL)
      if (!is.matrix(lmat)) lmat <- as.matrix(lmat)
      if (ncol(lmat) < 1) return(NULL)
      lmat[, 1, drop = TRUE]
    }, error = function(e) NULL)

    if (is.null(lv)) {
      log_fn("WARNING: loadings not found for block '", b, "'; including all VIP features")
      selected_feats <- names(vip_vec)
    } else {
      selected_feats <- names(lv[lv != 0])
    }
    if (length(selected_feats) == 0) {
      log_fn("WARNING: no selected features for block '", b, "'; skipping")
      next
    }

    for (feat in selected_feats) {
      vip_val <- vip_vec[feat]
      if (is.null(vip_val) || is.na(vip_val)) next
      rows[[length(rows) + 1]] <- data.frame(
        feature       = feat,
        block         = b,
        vip_comp1     = round(as.numeric(vip_val), 4),
        block_weight  = round(w_b, 4),
        consensus_vip = round(w_b * as.numeric(vip_val), 4),
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(rows) == 0) {
    log_fn("consensus VIP: no features found across all blocks — skipping artifact generation")
    return(invisible(result))
  }

  tbl <- do.call(rbind, rows)
  tbl <- tbl[order(tbl$consensus_vip, decreasing = TRUE), ]

  # Save CSV
  tryCatch({
    csv_path <- file.path(out_dir, "consensus_vip.csv")
    write.csv(tbl, csv_path, row.names = FALSE)
    result$csv_path <- csv_path
    log_fn("saved consensus_vip CSV (", nrow(tbl), " features)")
  }, error = function(e) {
    log_fn("WARNING: consensus VIP CSV failed: ", conditionMessage(e))
  })

  # Plot: horizontal bar chart of top features by consensus_vip
  n_show         <- min(nrow(tbl), 40L)
  tbl_plot       <- tbl[seq_len(n_show), ]
  tbl_plot       <- tbl_plot[rev(seq_len(nrow(tbl_plot))), ]  # bottom-to-top
  plot_height_px <- max(700L, 28L * n_show)
  plot_height_in <- max(5.0, 0.22 * n_show)

  plot_result <- tryCatch({
    # Compute xlim from actual bar values so no bar is clipped.
    # barplot() with axes=TRUE ignores xlim for tick placement — it uses pretty()
    # on the data range. Fix: suppress auto x-axis (xaxt="n") and draw explicitly.
    vip_vals_plot <- tbl_plot$consensus_vip
    if (!all(is.finite(vip_vals_plot))) {
      stop("Non-finite consensus_vip values in plot data: ",
           paste(vip_vals_plot[!is.finite(vip_vals_plot)], collapse = ", "))
    }
    vip_max <- max(vip_vals_plot)
    vip_min <- min(vip_vals_plot)
    x_lo    <- min(0, vip_min)
    # Round up to next 0.5 increment beyond the maximum bar value
    x_hi_raw   <- ceiling(vip_max / 0.5) * 0.5
    if (x_hi_raw <= vip_max) x_hi_raw <- x_hi_raw + 0.5  # ensure strictly beyond max
    x_hi <- x_hi_raw
    if (x_hi <= x_lo) x_hi <- x_lo + 1  # degenerate guard

    # Explicit tick positions: every 0.5, labelled every 1.0
    tick_at    <- seq(x_lo, x_hi, by = 0.5)
    tick_label <- ifelse(tick_at == round(tick_at), as.character(round(tick_at)), "")

    .save_plot(out_dir, "consensus_vip",
      {
        op <- par(mar = c(4, max(8, max(nchar(tbl_plot$feature)) * 0.55), 3, 2))
        on.exit(par(op), add = TRUE)
        barplot(tbl_plot$consensus_vip,
                horiz     = TRUE,
                col       = "#7b1c2e",
                border    = NA,
                names.arg = tbl_plot$feature,
                las       = 1,
                cex.names = 0.72,
                xlim      = c(x_lo, x_hi),
                xaxt      = "n",
                xlab      = "Consensus VIP (block weight × VIP comp 1)",
                main      = "Consensus VIP - Multi-Block sPLS-DA")
        axis(1, at = tick_at, labels = tick_label, las = 1, cex.axis = 0.85)
        abline(v = 0, col = "grey40", lwd = 1)
      },
      width_px = 1600, height_px = plot_height_px,
      width_in = 10,   height_in = plot_height_in)
  }, error = function(e) {
    log_fn("consensus VIP plot failed: ", conditionMessage(e))
    NULL
  })

  if (!is.null(plot_result)) {
    result$png_path <- plot_result$png_path
    result$pdf_path <- plot_result$pdf_path
    log_fn("saved consensus_vip plot (", n_show, " features shown)")
  } else {
    log_fn("WARNING: consensus VIP plot was not saved — check errors above")
  }

  invisible(result)
}

# ---------------------------------------------------------------------------
# plot_diablo_circos
# Uses compute_diablo_edges with a high cutoff (0.7, fallback 0.3) to show
# only strong correlations. Circos becomes unreadable with too many edges, so
# a higher cutoff is intentional. The network plot uses cutoff=0 by design.
# See compute_diablo_edges header for full design rationale.
# ---------------------------------------------------------------------------
plot_diablo_circos <- function(model, out_dir, log_fn = message) {
  if (length(names(model$X)) < 2) {
    log_fn("skipped circos plot: fewer than 2 blocks")
    return(invisible(list()))
  }

  result      <- list()
  cutoff_used <- 0.7

  plot_result <- tryCatch({
    .save_plot(out_dir, "circos",
      mixOmics::circosPlot(model, cutoff = 0.7, size.variables = 0.4, size.labels = 0.7),
      width_px = 1600, height_px = 1600, width_in = 11, height_in = 11)
  }, error = function(e) {
    log_fn("circos at cutoff=0.7 failed (", conditionMessage(e), "), retrying cutoff=0.3")
    cutoff_used <<- 0.3
    tryCatch(
      .save_plot(out_dir, "circos",
        mixOmics::circosPlot(model, cutoff = 0.3, size.variables = 0.4, size.labels = 0.7),
        width_px = 1600, height_px = 1600, width_in = 11, height_in = 11),
      error = function(e2) { log_fn("circos plot failed entirely: ", conditionMessage(e2)); NULL }
    )
  })
  if (!is.null(plot_result)) {
    result$png_path    <- plot_result$png_path
    result$pdf_path    <- plot_result$pdf_path
    result$cutoff_used <- cutoff_used
  }

  tryCatch({
    edge_df <- compute_diablo_edges(model, cutoff = cutoff_used, log_fn = log_fn)
    if (!is.null(edge_df) && nrow(edge_df) > 0) {
      edge_csv <- file.path(out_dir, "circos_edges.csv")
      write.csv(edge_df[, c("feature_1","block_1","feature_2","block_2",
                             "component","correlation_r","abs_correlation","p_value","within_block")],
                edge_csv, row.names = FALSE)
      result$edge_csv <- edge_csv
      log_fn("saved circos edge CSV (", nrow(edge_df), " edges, cutoff=", cutoff_used, ")")
    }
  }, error = function(e) log_fn("WARNING: circos edge CSV failed: ", conditionMessage(e)))

  log_fn("circos artifacts complete")
  invisible(result)
}
