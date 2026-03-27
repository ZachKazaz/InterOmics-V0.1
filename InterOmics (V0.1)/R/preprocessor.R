# preprocessor.R
# Entry point: run_preprocessing(session_id, params)
# Fixed order: filtering → imputation → normalization → transformation → scaling

source("R/utils.R")

# ===========================================================================
# 1. FILTERING
# ===========================================================================

filter_missingness <- function(mat, threshold = 0.5) {
  mv_pct   <- colMeans(is.na(mat))
  keep     <- mv_pct <= threshold
  removed  <- sum(!keep)
  list(mat = mat[, keep, drop = FALSE], removed = removed)
}

filter_variance <- function(mat, method = "IQR", percentile_cutoff = 0.1) {
  scores <- switch(method,
    IQR = apply(mat, 2, IQR,    na.rm = TRUE),
    SD  = apply(mat, 2, sd,     na.rm = TRUE),
    MAD = apply(mat, 2, mad,    na.rm = TRUE),
    stop("Unknown variance method: ", method)
  )
  cutoff  <- quantile(scores, probs = percentile_cutoff, na.rm = TRUE)
  keep    <- scores > cutoff
  removed <- sum(!keep)
  list(mat = mat[, keep, drop = FALSE], removed = removed)
}

filter_low_abundance <- function(mat, min_abundance) {
  if (is.null(min_abundance) || is.na(min_abundance)) return(list(mat = mat, removed = 0))
  medians <- apply(mat, 2, median, na.rm = TRUE)
  keep    <- medians >= min_abundance
  removed <- sum(!keep)
  list(mat = mat[, keep, drop = FALSE], removed = removed)
}

# ===========================================================================
# 2. IMPUTATION
# ===========================================================================

impute_half_min <- function(mat) {
  imputed <- 0L
  for (j in seq_len(ncol(mat))) {
    na_idx <- is.na(mat[, j])
    if (any(na_idx)) {
      mat[na_idx, j] <- min(mat[, j], na.rm = TRUE) / 2
      imputed <- imputed + sum(na_idx)
    }
  }
  list(mat = mat, imputed = imputed)
}

impute_mean <- function(mat) {
  imputed <- 0L
  for (j in seq_len(ncol(mat))) {
    na_idx <- is.na(mat[, j])
    if (any(na_idx)) {
      mat[na_idx, j] <- mean(mat[, j], na.rm = TRUE)
      imputed <- imputed + sum(na_idx)
    }
  }
  list(mat = mat, imputed = imputed)
}

impute_median <- function(mat) {
  imputed <- 0L
  for (j in seq_len(ncol(mat))) {
    na_idx <- is.na(mat[, j])
    if (any(na_idx)) {
      mat[na_idx, j] <- median(mat[, j], na.rm = TRUE)
      imputed <- imputed + sum(na_idx)
    }
  }
  list(mat = mat, imputed = imputed)
}

impute_knn <- function(mat, k = 10) {
  if (!requireNamespace("impute", quietly = TRUE))
    stop("Bioconductor package 'impute' is required for KNN imputation.")
  n_before <- sum(is.na(mat))
  result   <- impute::impute.knn(t(mat), k = k)
  mat_out  <- t(result$data)
  colnames(mat_out) <- colnames(mat)
  rownames(mat_out) <- rownames(mat)
  list(mat = mat_out, imputed = n_before)
}

# ===========================================================================
# 3. NORMALIZATION
# ===========================================================================

normalize_total_sum <- function(mat) {
  row_sums <- rowSums(mat, na.rm = TRUE)
  row_sums[row_sums == 0] <- 1  # guard against zero-sum rows
  mat / row_sums
}

normalize_median <- function(mat) {
  row_meds <- apply(mat, 1, median, na.rm = TRUE)
  row_meds[row_meds == 0] <- 1
  mat / row_meds
}

normalize_pqn <- function(mat) {
  ref     <- apply(mat, 2, median, na.rm = TRUE)
  ref[ref == 0] <- NA
  quotients <- sweep(mat, 2, ref, "/")
  sample_factors <- apply(quotients, 1, median, na.rm = TRUE)
  sample_factors[sample_factors == 0] <- 1
  mat / sample_factors
}

normalize_reference <- function(mat, ref_sample_id, sample_ids) {
  ref_idx <- which(sample_ids == ref_sample_id)
  if (length(ref_idx) == 0) stop("Reference sample '", ref_sample_id, "' not found.")
  ref_row <- as.numeric(mat[ref_idx, ])
  ref_row[ref_row == 0] <- NA
  sweep(mat, 2, ref_row, "/")
}

# ===========================================================================
# 4. TRANSFORMATION
# ===========================================================================

transform_log2     <- function(mat) log2(mat + 1)
transform_log10    <- function(mat) log10(mat + 1)
transform_sqrt     <- function(mat) sqrt(mat)
transform_cuberoot <- function(mat) mat^(1/3)

# ===========================================================================
# 5. SCALING
# ===========================================================================

scale_mean_center <- function(mat) {
  sweep(mat, 2, colMeans(mat, na.rm = TRUE), "-")
}

scale_auto <- function(mat) {
  means <- colMeans(mat, na.rm = TRUE)
  sds   <- apply(mat, 2, sd, na.rm = TRUE)
  sds[sds == 0] <- 1
  sweep(sweep(mat, 2, means, "-"), 2, sds, "/")
}

scale_pareto <- function(mat) {
  means <- colMeans(mat, na.rm = TRUE)
  sds   <- apply(mat, 2, sd, na.rm = TRUE)
  sds[sds == 0] <- 1
  sweep(sweep(mat, 2, means, "-"), 2, sqrt(sds), "/")
}

scale_range <- function(mat) {
  mins   <- apply(mat, 2, min, na.rm = TRUE)
  ranges <- apply(mat, 2, max, na.rm = TRUE) - mins
  ranges[ranges == 0] <- 1
  sweep(sweep(mat, 2, mins, "-"), 2, ranges, "/")
}

# ===========================================================================
# 6. ORCHESTRATOR
# ===========================================================================

run_preprocessing <- function(session_id, params) {
  raw_dir  <- get_session_path(session_id, "raw")
  out_dir  <- get_session_path(session_id, "preprocessed")
  all_csvs <- list.files(raw_dir, pattern = "\\.csv$", full.names = TRUE)
  matrix_files <- all_csvs[!grepl("metadata", basename(all_csvs), ignore.case = TRUE)]

  if (length(matrix_files) == 0) {
    return(error_response("NO_FILES_FOUND", "No feature matrix files found for preprocessing.",
                          "preprocessor", recoverable = TRUE))
  }

  summary_list   <- list()
  total_removed  <- 0L
  total_retained <- 0L
  total_imputed  <- 0L

  for (f in matrix_files) {
    dname <- tools::file_path_sans_ext(basename(f))
    raw   <- read.csv(f, stringsAsFactors = FALSE, check.names = FALSE)

    sample_ids  <- raw$SampleID
    feat_cols   <- setdiff(colnames(raw), "SampleID")
    mat         <- as.matrix(raw[, feat_cols, drop = FALSE])
    class(mat)  <- "numeric"
    n_start     <- ncol(mat)
    removed     <- 0L
    imputed_cnt <- 0L

    # --- Filtering ---
    filt_params <- params$filtering

    if (!is.null(filt_params$missingness_threshold)) {
      res <- filter_missingness(mat, as.numeric(filt_params$missingness_threshold))
      removed <- removed + res$removed
      mat     <- res$mat
    }

    if (!is.null(filt_params$variance_method) &&
        tolower(filt_params$variance_method) != "none") {
      cutoff <- if (!is.null(filt_params$variance_percentile_cutoff))
                  as.numeric(filt_params$variance_percentile_cutoff) else 0.1
      res <- filter_variance(mat, filt_params$variance_method, cutoff)
      removed <- removed + res$removed
      mat     <- res$mat
    }

    if (!is.null(filt_params$low_abundance_min)) {
      res <- filter_low_abundance(mat, as.numeric(filt_params$low_abundance_min))
      removed <- removed + res$removed
      mat     <- res$mat
    }

    # Post-filter feature limit check
    if (ncol(mat) > 5000) {
      return(error_response("FEATURE_LIMIT_EXCEEDED",
                            paste0("'", dname, "' has ", ncol(mat),
                                   " features after filtering. Maximum allowed is 5000."),
                            "preprocessor", recoverable = TRUE))
    }

    # --- Imputation ---
    imp_method <- tolower(params$imputation$method %||% "none")
    if (imp_method != "none") {
      res <- switch(imp_method,
        half_min = impute_half_min(mat),
        mean     = impute_mean(mat),
        median   = impute_median(mat),
        knn      = impute_knn(mat, k = 10),
        stop("Unknown imputation method: ", imp_method)
      )
      mat         <- res$mat
      imputed_cnt <- res$imputed
    }

    # --- Normalization ---
    norm_method <- tolower(params$normalization$method %||% "none")
    if (norm_method != "none") {
      mat <- switch(norm_method,
        total_sum = normalize_total_sum(mat),
        median    = normalize_median(mat),
        pqn       = normalize_pqn(mat),
        reference = normalize_reference(mat,
                      params$normalization$ref_sample_id, sample_ids),
        stop("Unknown normalization method: ", norm_method)
      )
    }

    # --- Transformation ---
    trans_method <- tolower(params$transformation$method %||% "none")
    if (trans_method != "none") {
      mat <- switch(trans_method,
        log2      = transform_log2(mat),
        log10     = transform_log10(mat),
        sqrt      = transform_sqrt(mat),
        cuberoot  = transform_cuberoot(mat),
        stop("Unknown transformation method: ", trans_method)
      )
    }

    # --- Scaling ---
    scale_method <- tolower(params$scaling$method %||% "none")
    if (scale_method != "none") {
      mat <- switch(scale_method,
        mean_center = scale_mean_center(mat),
        auto        = scale_auto(mat),
        pareto      = scale_pareto(mat),
        range       = scale_range(mat),
        stop("Unknown scaling method: ", scale_method)
      )
    }

    # Write preprocessed matrix
    out_df <- data.frame(SampleID = sample_ids, mat, check.names = FALSE)
    write.csv(out_df, file.path(out_dir, paste0(dname, ".csv")), row.names = FALSE)

    n_retained      <- ncol(mat)
    total_removed   <- total_removed  + removed
    total_retained  <- total_retained + n_retained
    total_imputed   <- total_imputed  + imputed_cnt

    summary_list[[dname]] <- list(
      features_start    = n_start,
      features_removed  = removed,
      features_retained = n_retained,
      values_imputed    = imputed_cnt,
      scaling_method    = scale_method
    )
  }

  # Build and write summary
  steps_applied <- character(0)
  if (!is.null(params$filtering$missingness_threshold))
    steps_applied <- c(steps_applied, "filtering")
  imp_m <- tolower(params$imputation$method %||% "none")
  if (imp_m != "none") steps_applied <- c(steps_applied, "imputation")
  norm_m <- tolower(params$normalization$method %||% "none")
  if (norm_m != "none") steps_applied <- c(steps_applied, "normalization")
  trans_m <- tolower(params$transformation$method %||% "none")
  if (trans_m != "none") steps_applied <- c(steps_applied, "transformation")
  scale_m <- tolower(params$scaling$method %||% "none")
  if (scale_m != "none") steps_applied <- c(steps_applied, "scaling")

  preprocessing_summary <- list(
    steps_applied     = steps_applied,
    parameters        = params,
    features_removed  = total_removed,
    features_retained = total_retained,
    values_imputed    = total_imputed,
    per_dataset       = summary_list
  )

  summary_path <- get_session_path(session_id, "preprocessing_summary.json")
  jsonlite::write_json(preprocessing_summary, summary_path,
                       auto_unbox = TRUE, pretty = TRUE)

  state <- read_pipeline_state(session_id)
  state$steps$preprocess <- "complete"
  write_pipeline_state(session_id, state)

  ok_response(list(summary = preprocessing_summary))
}

# ---------------------------------------------------------------------------
# Null-coalescing helper (base R doesn't have %||%)
# ---------------------------------------------------------------------------
`%||%` <- function(a, b) if (!is.null(a)) a else b
