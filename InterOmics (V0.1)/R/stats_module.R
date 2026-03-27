# stats_module.R — Feature-wise statistical modeling
# Tasks: 2.7.1, 2.7.2
# Entry point: run_stats(session_id, params)

library(lme4)
library(car)
library(ggplot2)
library(pheatmap)
library(jsonlite)

source("R/utils.R")

# ---------------------------------------------------------------------------
# run_stats — task 2.7.1
# ---------------------------------------------------------------------------
#
# params:
#   predictors          — character vector of predictor column names in metadata
#   covariates          — character vector of covariate column names (may be empty)
#   interactions        — character vector of interaction terms, e.g. "group:time"
#   repeated_measures_id — column name for random effect subject ID, or NULL
#
# Outputs (written to storage/{session_id}/results/stats/):
#   feature_results.csv          — per-feature, per-term results
#   main_effects_summary.csv     — aggregated main effects
#   interaction_effects_summary.csv — aggregated interaction effects
#
run_stats <- function(session_id, params) {
  if (!check_prerequisite(session_id, "preprocess")) {
    return(error_response("PREREQUISITE_NOT_MET",
                          "Preprocessing must be complete before statistical modeling.",
                          "stats", recoverable = TRUE))
  }

  # ---- Load data ----
  preprocessed_dir <- get_session_path(session_id, "preprocessed")
  matrix_files     <- list.files(preprocessed_dir, pattern = "\\.csv$",
                                 full.names = TRUE)
  matrix_files     <- matrix_files[!grepl("metadata", basename(matrix_files),
                                          ignore.case = TRUE)]
  if (length(matrix_files) == 0) {
    return(error_response("NO_FILES_FOUND",
                          "No preprocessed matrices found.",
                          "stats", recoverable = TRUE))
  }

  # Load metadata
  raw_dir       <- get_session_path(session_id, "raw")
  all_raw_files <- list.files(raw_dir, pattern = "\\.csv$", full.names = TRUE)
  meta_files    <- all_raw_files[grepl("metadata", basename(all_raw_files), ignore.case = TRUE)]
  if (length(meta_files) == 0) {
    return(error_response("NO_FILES_FOUND",
                          "Metadata file not found.",
                          "stats", recoverable = TRUE))
  }
  meta <- read.csv(meta_files[1], stringsAsFactors = FALSE)

  predictors   <- params$predictors   %||% character(0)
  covariates   <- params$covariates   %||% character(0)
  interactions <- params$interactions %||% character(0)
  rm_id        <- params$repeated_measures_id  # NULL or column name

  if (length(predictors) == 0) {
    return(error_response("ANALYSIS_RUNTIME_ERROR",
                          "At least one predictor must be specified.",
                          "stats", recoverable = TRUE))
  }

  # Build fixed-effects formula string
  fixed_terms <- c(predictors, covariates, interactions)
  fixed_rhs   <- paste(fixed_terms, collapse = " + ")

  # ---- Process each dataset ----
  all_feature_rows <- list()
  convergence_warnings <- character(0)

  for (mf in matrix_files) {
    mat <- read.csv(mf, row.names = 1, check.names = FALSE)
    mat <- as.data.frame(mat)

    # Merge with metadata on SampleID
    mat$SampleID <- rownames(mat)
    merged       <- merge(mat, meta, by = "SampleID", all = FALSE)
    if (nrow(merged) == 0) next

    feature_cols <- setdiff(colnames(mat), "SampleID")

    for (feat in feature_cols) {
      row_result <- .fit_feature(
        feat         = feat,
        merged       = merged,
        fixed_rhs    = fixed_rhs,
        rm_id        = rm_id,
        predictors   = predictors,
        covariates   = covariates,
        interactions = interactions
      )

      if (!is.null(row_result$convergence_warning)) {
        convergence_warnings <- c(convergence_warnings,
                                  paste0(feat, ": ", row_result$convergence_warning))
      }
      all_feature_rows <- c(all_feature_rows, row_result$rows)
    }
  }

  if (length(all_feature_rows) == 0) {
    return(error_response("ANALYSIS_RUNTIME_ERROR",
                          "No features could be modeled. Check predictors and data.",
                          "stats", recoverable = TRUE))
  }

  feature_df <- do.call(rbind, all_feature_rows)

  # ---- BH FDR per term ----
  terms <- unique(feature_df$term)
  for (trm in terms) {
    idx <- feature_df$term == trm
    feature_df$fdr[idx] <- p.adjust(feature_df$p_value[idx], method = "BH")
  }

  feature_df <- feature_df[order(feature_df$term, feature_df$p_value), ]

  # ---- Write outputs ----
  out_dir <- get_session_path(session_id, "results", "stats")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  write.csv(feature_df,
            file.path(out_dir, "feature_results.csv"),
            row.names = FALSE)

  # Main effects summary
  main_terms <- setdiff(terms, interactions)
  main_df    <- feature_df[feature_df$term %in% main_terms, ]
  main_summary <- .summarise_effects(main_df)
  write.csv(main_summary,
            file.path(out_dir, "main_effects_summary.csv"),
            row.names = FALSE)

  # Interaction effects summary
  if (length(interactions) > 0) {
    int_df      <- feature_df[feature_df$term %in% interactions, ]
    int_summary <- .summarise_effects(int_df)
    write.csv(int_summary,
              file.path(out_dir, "interaction_effects_summary.csv"),
              row.names = FALSE)
  } else {
    write.csv(data.frame(), file.path(out_dir, "interaction_effects_summary.csv"),
              row.names = FALSE)
  }

  # ---- Plots ----
  for (trm in terms) {
    term_df <- feature_df[feature_df$term == trm, ]
    plot_stats_volcano(term_df, trm, out_dir)
  }

  # Heatmap: top 50 significant features across all terms
  sig_features <- unique(feature_df$feature[!is.na(feature_df$fdr) &
                                               feature_df$fdr < 0.05])
  top_features <- head(sig_features, 50)
  if (length(top_features) > 1) {
    # Reload first matrix for heatmap
    mat_hm <- read.csv(matrix_files[1], row.names = 1, check.names = FALSE)
    plot_stats_heatmap(mat_hm, top_features, out_dir)
  }

  # Update pipeline state
  state <- read_pipeline_state(session_id)
  state$steps$stats <- "complete"
  write_pipeline_state(session_id, state)

  ok_response(list(
    n_features           = length(unique(feature_df$feature)),
    n_terms              = length(terms),
    n_significant        = length(sig_features),
    convergence_warnings = convergence_warnings,
    output               = "results/stats/"
  ))
}

# ---------------------------------------------------------------------------
# Internal: fit one feature and return per-term result rows
# ---------------------------------------------------------------------------
.fit_feature <- function(feat, merged, fixed_rhs, rm_id,
                         predictors, covariates, interactions) {
  response <- merged[[feat]]
  if (all(is.na(response)) || var(response, na.rm = TRUE) == 0) {
    return(list(rows = NULL, convergence_warning = NULL))
  }

  formula_str <- paste0("`", feat, "` ~ ", fixed_rhs)
  conv_warn   <- NULL

  fit <- tryCatch({
    if (is.null(rm_id)) {
      lm(as.formula(formula_str), data = merged)
    } else {
      random_term  <- paste0("(1|", rm_id, ")")
      lmer_formula <- paste0(formula_str, " + ", random_term)
      withCallingHandlers(
        lme4::lmer(as.formula(lmer_formula), data = merged, REML = FALSE),
        warning = function(w) {
          if (grepl("convergence|failed to converge", conditionMessage(w),
                    ignore.case = TRUE)) {
            conv_warn <<- conditionMessage(w)
            invokeRestart("muffleWarning")
          }
        }
      )
    }
  }, error = function(e) {
    conv_warn <<- paste0("MODEL_CONVERGENCE_FAILED: ", conditionMessage(e))
    NULL
  })

  if (is.null(fit)) {
    return(list(rows = NULL, convergence_warning = conv_warn))
  }

  # Extract coefficients and raw p-values
  coef_tbl <- tryCatch(
    as.data.frame(summary(fit)$coefficients),
    error = function(e) NULL
  )
  if (is.null(coef_tbl)) {
    return(list(rows = NULL, convergence_warning = conv_warn))
  }

  # Partial eta-squared via car::Anova type III
  anova_tbl <- tryCatch(
    as.data.frame(car::Anova(fit, type = 3)),
    error = function(e) NULL
  )

  sd_response <- sd(response, na.rm = TRUE)

  rows <- lapply(rownames(coef_tbl), function(trm) {
    if (trm == "(Intercept)") return(NULL)

    estimate <- coef_tbl[trm, 1]
    p_col    <- grep("Pr\\(|p.value|p value", colnames(coef_tbl),
                     ignore.case = TRUE, value = TRUE)[1]
    p_val    <- if (!is.na(p_col)) coef_tbl[trm, p_col] else NA_real_

    # Standardized beta: coef * (sd_predictor / sd_response)
    pred_col <- trm  # term name matches predictor column for simple terms
    sd_pred  <- tryCatch(sd(merged[[pred_col]], na.rm = TRUE), error = function(e) NA_real_)
    std_beta <- if (!is.na(sd_pred) && sd_response > 0 && !is.na(sd_pred)) {
      estimate * (sd_pred / sd_response)
    } else NA_real_

    # Partial eta-squared from ANOVA table
    peta_sq <- NA_real_
    if (!is.null(anova_tbl)) {
      # Match term name (car::Anova uses predictor names, not coefficient names)
      anova_term <- trm
      if (anova_term %in% rownames(anova_tbl)) {
        ss_effect <- anova_tbl[anova_term, grep("^Sum Sq|^SS", colnames(anova_tbl),
                                                 ignore.case = TRUE)[1]]
        ss_resid  <- tryCatch(anova_tbl["Residuals", grep("^Sum Sq|^SS",
                                                           colnames(anova_tbl),
                                                           ignore.case = TRUE)[1]],
                              error = function(e) NA_real_)
        if (!is.na(ss_effect) && !is.na(ss_resid) && (ss_effect + ss_resid) > 0) {
          peta_sq <- ss_effect / (ss_effect + ss_resid)
        }
      }
    }

    data.frame(
      feature        = feat,
      term           = trm,
      estimate       = estimate,
      std_beta       = std_beta,
      partial_eta_sq = peta_sq,
      p_value        = p_val,
      fdr            = NA_real_,   # filled in after all features processed
      stringsAsFactors = FALSE
    )
  })

  rows <- Filter(Negate(is.null), rows)
  list(rows = rows, convergence_warning = conv_warn)
}

# ---------------------------------------------------------------------------
# Internal: summarise effects across features
# ---------------------------------------------------------------------------
.summarise_effects <- function(df) {
  if (nrow(df) == 0) return(df)
  terms <- unique(df$term)
  do.call(rbind, lapply(terms, function(trm) {
    sub_df <- df[df$term == trm, ]
    data.frame(
      term              = trm,
      n_features        = nrow(sub_df),
      n_significant_fdr = sum(!is.na(sub_df$fdr) & sub_df$fdr < 0.05),
      mean_std_beta     = mean(sub_df$std_beta, na.rm = TRUE),
      stringsAsFactors  = FALSE
    )
  }))
}

# ---------------------------------------------------------------------------
# Plot functions — task 2.7.2
# ---------------------------------------------------------------------------

#' Volcano plot: x = std_beta, y = -log10(p_value), color = FDR < 0.05
plot_stats_volcano <- function(results_df, term, out_dir) {
  df <- results_df[!is.na(results_df$std_beta) & !is.na(results_df$p_value), ]
  if (nrow(df) == 0) return(invisible(NULL))

  df$neg_log10_p  <- -log10(pmax(df$p_value, 1e-300))
  df$significant  <- !is.na(df$fdr) & df$fdr < 0.05

  p <- ggplot2::ggplot(df,
         ggplot2::aes(x = std_beta, y = neg_log10_p, color = significant)) +
    ggplot2::geom_point(alpha = 0.6, size = 1.5) +
    ggplot2::scale_color_manual(values = c("FALSE" = "grey60", "TRUE" = "red"),
                                 name = "FDR < 0.05") +
    ggplot2::geom_hline(yintercept = -log10(0.05), linetype = "dashed",
                         color = "black", linewidth = 0.4) +
    ggplot2::labs(x = "Standardized Beta",
                  y = "-log10(p-value)",
                  title = paste("Volcano Plot —", term)) +
    ggplot2::theme_minimal()

  safe_term <- gsub("[^A-Za-z0-9_]", "_", term)
  ggplot2::ggsave(file.path(out_dir, paste0("volcano_", safe_term, ".png")),
                  plot = p, width = 8, height = 6, dpi = 150)
  ggplot2::ggsave(file.path(out_dir, paste0("volcano_", safe_term, ".pdf")),
                  plot = p, width = 8, height = 6)
  invisible(NULL)
}

#' Heatmap of top significant features using pheatmap
plot_stats_heatmap <- function(mat, top_features, out_dir) {
  # mat: samples × features matrix (data.frame or matrix, row.names = SampleID)
  feat_present <- intersect(top_features, colnames(mat))
  if (length(feat_present) < 2) return(invisible(NULL))

  sub_mat <- t(as.matrix(mat[, feat_present, drop = FALSE]))

  png_path <- file.path(out_dir, "heatmap_top_features.png")
  pdf_path <- file.path(out_dir, "heatmap_top_features.pdf")

  png(png_path, width = 1200, height = 900, res = 150)
  tryCatch(
    pheatmap::pheatmap(sub_mat,
                       clustering_distance_rows = "euclidean",
                       clustering_distance_cols = "euclidean",
                       show_colnames = ncol(sub_mat) <= 50,
                       main = "Top Significant Features"),
    finally = dev.off()
  )

  pdf(pdf_path, width = 10, height = 8)
  tryCatch(
    pheatmap::pheatmap(sub_mat,
                       clustering_distance_rows = "euclidean",
                       clustering_distance_cols = "euclidean",
                       show_colnames = ncol(sub_mat) <= 50,
                       main = "Top Significant Features"),
    finally = dev.off()
  )

  invisible(NULL)
}
