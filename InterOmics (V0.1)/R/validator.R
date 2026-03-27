# validator.R
# Entry point: run_validation(session_id)
# Reads raw CSVs, runs all validation checks, writes validation_report.json

source("R/utils.R")

# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

.read_csv_safe <- function(path) {
  tryCatch(
    read.csv(path, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) NULL
  )
}

.is_numeric_col <- function(x) {
  suppressWarnings(!any(is.na(as.numeric(x[!is.na(x)]))))
}

# ---------------------------------------------------------------------------
# run_validation
# ---------------------------------------------------------------------------

run_validation <- function(session_id) {
  raw_dir  <- get_session_path(session_id, "raw")
  all_files <- list.files(raw_dir, pattern = "\\.csv$", full.names = TRUE)

  if (length(all_files) == 0) {
    return(error_response("NO_FILES_FOUND", "No CSV files found in upload directory.",
                          "validator", recoverable = TRUE))
  }

  # Identify metadata vs feature matrix files by filename convention
  meta_files   <- all_files[grepl("metadata", basename(all_files), ignore.case = TRUE)]
  matrix_files <- setdiff(all_files, meta_files)

  if (length(meta_files) != 1) {
    return(error_response("MISSING_REQUIRED_COLUMN",
                          "Exactly one metadata file (filename must contain 'metadata') is required.",
                          "validator", recoverable = TRUE))
  }

  # ---- Dataset count check ------------------------------------------------
  if (length(matrix_files) > 4) {
    return(error_response("DATASET_LIMIT_EXCEEDED",
                          paste0("A maximum of 4 feature matrix datasets are allowed. ",
                                 length(matrix_files), " were uploaded."),
                          "validator", recoverable = TRUE))
  }
  if (length(matrix_files) < 1) {
    return(error_response("NO_FILES_FOUND",
                          "At least one feature matrix CSV is required.",
                          "validator", recoverable = TRUE))
  }

  errors   <- list()
  warnings <- list()

  # ---- Read and clean metadata --------------------------------------------
  meta <- .read_csv_safe(meta_files[[1]])
  if (is.null(meta)) {
    return(error_response("MISSING_REQUIRED_COLUMN",
                          "Metadata file could not be read.",
                          "validator", recoverable = TRUE))
  }

  # Trim whitespace from column names
  colnames(meta) <- trimws(colnames(meta))

  if (!"SampleID" %in% colnames(meta)) {
    errors <- c(errors, list(list(
      error_code = "MISSING_REQUIRED_COLUMN",
      detail     = "Metadata file is missing the required 'SampleID' column."
    )))
  }

  if (ncol(meta) < 2) {
    errors <- c(errors, list(list(
      error_code = "MISSING_REQUIRED_COLUMN",
      detail     = "Metadata file must contain at least one grouping variable column in addition to SampleID."
    )))
  }

  # ---- Clean metadata before any ID checks --------------------------------
  n_raw_rows <- nrow(meta)

  if ("SampleID" %in% colnames(meta)) {
    # Trim whitespace in SampleID values
    meta$SampleID <- trimws(as.character(meta$SampleID))

    # Drop fully blank rows: rows where every column is NA or empty string
    is_blank_row <- apply(meta, 1, function(r) all(is.na(r) | trimws(as.character(r)) == ""))
    n_blank <- sum(is_blank_row)
    if (n_blank > 0) {
      meta <- meta[!is_blank_row, , drop = FALSE]
      warnings <- c(warnings, list(list(
        type   = "BLANK_ROWS_REMOVED",
        detail = paste0(n_blank, " fully blank row(s) were automatically removed from metadata.")
      )))
    }

    # Rows with missing or blank SampleID (after blank-row removal)
    missing_sid <- is.na(meta$SampleID) | meta$SampleID == ""
    n_missing_sid <- sum(missing_sid)
    if (n_missing_sid > 0) {
      errors <- c(errors, list(list(
        error_code = "MISSING_REQUIRED_COLUMN",
        detail     = paste0(n_missing_sid, " metadata row(s) have a missing or blank SampleID ",
                            "after removing fully blank rows.")
      )))
    }

    # Duplicate check only among non-missing, non-blank SampleIDs
    valid_ids <- meta$SampleID[!missing_sid]
    dup_meta  <- valid_ids[duplicated(valid_ids)]
    if (length(dup_meta) > 0) {
      errors <- c(errors, list(list(
        error_code = "DUPLICATE_IDS",
        detail     = paste0("Duplicate non-empty SampleIDs in metadata: ",
                            paste(unique(dup_meta), collapse = ", "))
      )))
    }
  }

  # Duplicate column names in metadata
  dup_meta_cols <- colnames(meta)[duplicated(colnames(meta))]
  if (length(dup_meta_cols) > 0) {
    errors <- c(errors, list(list(
      error_code = "DUPLICATE_COLUMNS",
      detail     = paste0("Duplicate column names in metadata: ", paste(dup_meta_cols, collapse = ", "))
    )))
  }

  # Use only rows with valid SampleIDs for downstream cross-checks
  meta_sample_ids <- if ("SampleID" %in% colnames(meta)) {
    valid_mask <- !is.na(meta$SampleID) & meta$SampleID != ""
    meta$SampleID[valid_mask]
  } else {
    character(0)
  }

  # ---- Read and validate each feature matrix --------------------------------
  matrices          <- list()
  missing_value_pct <- list()

  for (f in matrix_files) {
    dname <- tools::file_path_sans_ext(basename(f))
    mat   <- .read_csv_safe(f)

    if (is.null(mat)) {
      errors <- c(errors, list(list(
        error_code = "MISSING_REQUIRED_COLUMN",
        detail     = paste0("Feature matrix '", basename(f), "' could not be read.")
      )))
      next
    }

    # First column must be SampleID
    if (colnames(mat)[1] != "SampleID") {
      errors <- c(errors, list(list(
        error_code = "MISSING_REQUIRED_COLUMN",
        detail     = paste0("First column of '", basename(f), "' must be named 'SampleID'.")
      )))
      next
    }

    # No empty column names
    empty_cols <- which(colnames(mat) == "" | is.na(colnames(mat)))
    if (length(empty_cols) > 0) {
      errors <- c(errors, list(list(
        error_code = "MISSING_REQUIRED_COLUMN",
        detail     = paste0("'", basename(f), "' has empty column names at positions: ",
                            paste(empty_cols, collapse = ", "))
      )))
    }

    # Duplicate row IDs — trim whitespace first
    mat$SampleID <- trimws(as.character(mat$SampleID))
    dup_rows <- mat$SampleID[duplicated(mat$SampleID)]
    if (length(dup_rows) > 0) {
      errors <- c(errors, list(list(
        error_code = "DUPLICATE_IDS",
        detail     = paste0("Duplicate SampleIDs in '", basename(f), "': ",
                            paste(unique(dup_rows), collapse = ", "))
      )))
    }

    # Duplicate column names
    dup_cols <- colnames(mat)[duplicated(colnames(mat))]
    if (length(dup_cols) > 0) {
      errors <- c(errors, list(list(
        error_code = "DUPLICATE_COLUMNS",
        detail     = paste0("Duplicate column names in '", basename(f), "': ",
                            paste(dup_cols, collapse = ", "))
      )))
    }

    # All non-SampleID columns must be numeric
    feature_cols <- colnames(mat)[colnames(mat) != "SampleID"]
    non_numeric  <- feature_cols[!sapply(feature_cols, function(col) .is_numeric_col(mat[[col]]))]
    if (length(non_numeric) > 0) {
      errors <- c(errors, list(list(
        error_code = "NON_NUMERIC_VALUES",
        detail     = paste0("Non-numeric values in '", basename(f), "' columns: ",
                            paste(non_numeric, collapse = ", "))
      )))
    }

    # Sample count limit
    n_samples <- nrow(mat)
    if (n_samples > 500) {
      errors <- c(errors, list(list(
        error_code = "SAMPLE_LIMIT_EXCEEDED",
        detail     = paste0("'", basename(f), "' has ", n_samples,
                            " samples. Maximum allowed is 500.")
      )))
    }

    # Feature count warning (raw, before preprocessing)
    n_features <- length(feature_cols)
    if (n_features > 5000) {
      warnings <- c(warnings, list(list(
        type   = "FEATURE_COUNT_HIGH",
        detail = paste0("'", basename(f), "' has ", n_features,
                        " features. Maximum recommended is 5000.")
      )))
    }

    # Missing value percentages — summarised per dataset (overall %)
    feat_mat     <- mat[, feature_cols, drop = FALSE]
    feat_mat_num <- as.data.frame(lapply(feat_mat, as.numeric))
    if (nrow(feat_mat_num) > 0 && ncol(feat_mat_num) > 0) {
      per_feature_pct <- round(colMeans(is.na(feat_mat_num)) * 100, 2)
      overall_pct     <- round(mean(is.na(feat_mat_num)) * 100, 2)
      n_features_with_mv <- sum(per_feature_pct > 0)
      missing_value_pct[[dname]] <- list(
        overall_pct         = overall_pct,
        n_features_with_mv  = n_features_with_mv,
        n_features_total    = length(feature_cols),
        n_samples           = nrow(mat)
      )
    } else {
      missing_value_pct[[dname]] <- list(
        overall_pct = 0, n_features_with_mv = 0,
        n_features_total = length(feature_cols), n_samples = nrow(mat)
      )
    }

    matrices[[dname]] <- mat
  }

  # ---- SampleID cross-file consistency ------------------------------------
  # Rule: all matrix SampleIDs must exist in metadata (metadata may have extras).
  # Matrix files must share the same SampleID set with each other.
  if (length(matrices) > 0 && length(meta_sample_ids) > 0) {
    for (dname in names(matrices)) {
      mat_ids <- trimws(as.character(matrices[[dname]]$SampleID))
      missing_in_meta <- setdiff(mat_ids, meta_sample_ids)
      if (length(missing_in_meta) > 0) {
        errors <- c(errors, list(list(
          error_code = "SAMPLEID_MISMATCH",
          detail     = paste0("SampleIDs in '", dname,
                              "' not found in metadata: ",
                              paste(head(missing_in_meta, 10), collapse = ", "),
                              if (length(missing_in_meta) > 10) "…" else "")
        )))
      }
    }
    if (length(matrices) > 1) {
      id_sets <- lapply(matrices, function(m) trimws(as.character(m$SampleID)))
      ref     <- id_sets[[1]]
      for (i in seq_along(id_sets)[-1]) {
        if (!setequal(ref, id_sets[[i]])) {
          errors <- c(errors, list(list(
            error_code = "SAMPLEID_MISMATCH",
            detail     = paste0("SampleIDs in '", names(matrices)[i],
                                "' do not match '", names(matrices)[1], "'.")
          )))
        }
      }
    }
  }

  # ---- Class imbalance warning — first grouping column only ---------------
  # Only warn on the first non-SampleID column (the primary grouping variable).
  # Downstream DIABLO configuration lets the user pick the grouping column;
  # we don't know which column they intend to use yet, so we check only the
  # first candidate to avoid spurious warnings on unrelated metadata columns.
  sample_counts_per_group <- list()
  if ("SampleID" %in% colnames(meta) && ncol(meta) >= 2) {
    gcol <- setdiff(colnames(meta), "SampleID")[1]
    if (!is.na(gcol)) {
      counts <- table(meta[[gcol]])
      sample_counts_per_group[[gcol]] <- as.list(counts)
      small_groups <- names(counts)[counts < 3]
      if (length(small_groups) > 0) {
        warnings <- c(warnings, list(list(
          type   = "CLASS_IMBALANCE",
          detail = paste0("Groups with fewer than 3 samples in '", gcol, "': ",
                          paste(small_groups, collapse = ", "),
                          ". DIABLO requires at least 3 samples per group for cross-validation.")
        )))
      }
    }
  }

  # ---- Write report and update state --------------------------------------
  has_critical <- length(errors) > 0
  report <- list(
    status                  = if (has_critical) "error" else "ok",
    errors                  = errors,
    warnings                = warnings,
    missing_value_pct       = missing_value_pct,
    sample_counts_per_group = sample_counts_per_group
  )

  report_path <- get_session_path(session_id, "validation_report.json")
  jsonlite::write_json(report, report_path, auto_unbox = TRUE, pretty = TRUE)

  step_status <- if (has_critical) "error" else "complete"
  state <- read_pipeline_state(session_id)
  state$steps$validate <- step_status
  write_pipeline_state(session_id, state)

  if (has_critical) {
    first_err <- errors[[1]]
    return(error_response(first_err$error_code, first_err$detail, "validator",
                          recoverable = TRUE))
  }

  ok_response(list(
    status                  = "ok",
    warnings                = warnings,
    missing_value_pct       = missing_value_pct,
    sample_counts_per_group = sample_counts_per_group
  ))
}
