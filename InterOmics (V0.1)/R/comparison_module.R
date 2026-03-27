# comparison_module.R — DIABLO comparison definition
# Provides:
#   get_group_table(session_id)            — build group summary table from metadata
#   save_comparison_params(session_id, params) — persist selected groups

source("R/utils.R")

# ---------------------------------------------------------------------------
# .load_session_files  (internal)
# Shared helper: reads metadata + matrix files for a session.
# Returns list(meta, matrix_files, shared_ids) or an error_response list.
# ---------------------------------------------------------------------------
.load_session_files <- function(session_id) {
  raw_dir       <- get_session_path(session_id, "raw")
  all_raw_files <- list.files(raw_dir, pattern = "\\.csv$", full.names = TRUE)

  # ---- Metadata ------------------------------------------------------------
  meta_files <- all_raw_files[grepl("metadata", basename(all_raw_files), ignore.case = TRUE)]
  if (length(meta_files) == 0) {
    return(error_response("NO_FILES_FOUND",
                          "Metadata file not found. Please upload data first.",
                          "comparison", recoverable = TRUE))
  }
  if (length(meta_files) > 1) {
    return(error_response("AMBIGUOUS_METADATA",
                          paste0("Multiple metadata files found: ",
                                 paste(basename(meta_files), collapse = ", "),
                                 ". Please upload exactly one metadata file."),
                          "comparison", recoverable = TRUE))
  }

  meta <- tryCatch(
    read.csv(meta_files[1], stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) NULL
  )
  if (is.null(meta)) {
    return(error_response("FILE_READ_ERROR",
                          "Could not read metadata file.",
                          "comparison", recoverable = TRUE))
  }
  colnames(meta) <- trimws(colnames(meta))
  if (!"SampleID" %in% colnames(meta)) {
    return(error_response("MISSING_REQUIRED_COLUMN",
                          "Metadata file is missing the 'SampleID' column.",
                          "comparison", recoverable = TRUE))
  }
  meta$SampleID <- trimws(as.character(meta$SampleID))

  # Drop fully blank rows
  is_blank <- apply(meta, 1, function(r) all(is.na(r) | trimws(as.character(r)) == ""))
  meta <- meta[!is_blank, , drop = FALSE]

  # ---- Matrix files --------------------------------------------------------
  matrix_files <- all_raw_files[!grepl("metadata", basename(all_raw_files), ignore.case = TRUE)]
  if (length(matrix_files) == 0) {
    return(error_response("NO_FILES_FOUND",
                          "No feature matrix files found.",
                          "comparison", recoverable = TRUE))
  }
  if (length(matrix_files) < 2 || length(matrix_files) > 4) {
    return(error_response("DATASET_COUNT_INVALID",
                          paste0("DIABLO requires 2–4 feature matrix datasets. Found: ",
                                 length(matrix_files), "."),
                          "comparison", recoverable = TRUE))
  }

  # Shared SampleIDs across all matrices
  sample_sets <- lapply(matrix_files, function(f) {
    m <- tryCatch(read.csv(f, stringsAsFactors = FALSE, check.names = FALSE),
                  error = function(e) NULL)
    if (is.null(m) || !"SampleID" %in% colnames(m)) return(character(0))
    trimws(as.character(m$SampleID))
  })
  shared_ids <- Reduce(intersect, sample_sets)
  if (length(shared_ids) == 0) {
    return(error_response("SAMPLEID_MISMATCH",
                          "No common SampleIDs found across all uploaded matrix files.",
                          "comparison", recoverable = TRUE))
  }

  list(meta = meta, matrix_files = matrix_files, shared_ids = shared_ids)
}

# ---------------------------------------------------------------------------
# get_group_table
# Builds a group summary table using the "Group" metadata column.
# Each row = one unique Group value among the shared SampleIDs.
# Descriptor columns = metadata columns that are constant within every group
# (excluding SampleID and Group itself).
# ---------------------------------------------------------------------------
get_group_table <- function(session_id) {
  loaded <- .load_session_files(session_id)
  if (!is.null(loaded$status) && loaded$status == "error") return(loaded)

  meta         <- loaded$meta
  matrix_files <- loaded$matrix_files
  shared_ids   <- loaded$shared_ids

  # Restrict metadata to shared SampleIDs
  meta_filtered <- meta[meta$SampleID %in% shared_ids, , drop = FALSE]
  if (nrow(meta_filtered) == 0) {
    return(error_response("SAMPLEID_MISMATCH",
                          "No metadata rows match the SampleIDs in the uploaded matrices.",
                          "comparison", recoverable = TRUE))
  }

  # Require "Group" column
  if (!"Group" %in% colnames(meta_filtered)) {
    return(error_response("MISSING_GROUP_COLUMN",
                          paste0("DIABLO Define Comparison requires a metadata column named 'Group'. ",
                                 "Available columns: ",
                                 paste(setdiff(colnames(meta_filtered), "SampleID"), collapse = ", "),
                                 "."),
                          "comparison", recoverable = TRUE))
  }

  meta_filtered$Group <- trimws(as.character(meta_filtered$Group))
  # Remove rows with blank Group
  meta_filtered <- meta_filtered[meta_filtered$Group != "" & !is.na(meta_filtered$Group), , drop = FALSE]

  group_values <- unique(meta_filtered$Group)
  if (length(group_values) < 2) {
    return(error_response("INSUFFICIENT_GROUPS",
                          paste0("At least 2 distinct Group values are required. Found: ",
                                 length(group_values), "."),
                          "comparison", recoverable = TRUE))
  }

  # ---- Identify descriptor columns ----------------------------------------
  # A descriptor column is one that has exactly one unique value within every
  # group (i.e. constant per group). Exclude SampleID and Group.
  other_cols <- setdiff(colnames(meta_filtered), c("SampleID", "Group"))
  descriptor_cols <- character(0)
  for (col in other_cols) {
    vals <- trimws(as.character(meta_filtered[[col]]))
    # Check: for each group, all values of this column are the same
    is_constant <- all(sapply(group_values, function(g) {
      grp_vals <- vals[meta_filtered$Group == g]
      grp_vals <- grp_vals[!is.na(grp_vals) & grp_vals != ""]
      length(unique(grp_vals)) <= 1
    }))
    if (is_constant) descriptor_cols <- c(descriptor_cols, col)
  }

  # ---- Build one row per group --------------------------------------------
  available_groups <- lapply(group_values, function(g) {
    grp_rows  <- meta_filtered[meta_filtered$Group == g, , drop = FALSE]
    n_samples <- nrow(grp_rows)
    selectable <- n_samples >= 3
    warning_msg <- if (!selectable) {
      paste0("Only ", n_samples, " sample", if (n_samples != 1) "s" else "",
             " — at least 3 required for DIABLO cross-validation.")
    } else NULL

    row <- list(
      group_id   = g,
      n_samples  = n_samples,
      selectable = selectable
    )
    if (!is.null(warning_msg)) row$warning <- warning_msg

    # Attach constant descriptor values
    for (col in descriptor_cols) {
      vals <- trimws(as.character(grp_rows[[col]]))
      vals <- vals[!is.na(vals) & vals != ""]
      row[[col]] <- if (length(vals) > 0) vals[1] else NA_character_
    }

    row
  })

  # Sort: selectable groups first, then by group name
  selectable_rows <- Filter(function(r) isTRUE(r$selectable), available_groups)
  invalid_rows    <- Filter(function(r) !isTRUE(r$selectable), available_groups)
  available_groups <- c(selectable_rows, invalid_rows)

  # Default selection = all selectable groups
  recommended_default <- sapply(
    Filter(function(r) isTRUE(r$selectable), available_groups),
    function(r) r$group_id
  )

  # ---- keepX recommendation (if preprocessing already done) ---------------
  # Uses the preprocessing summary (retained feature counts per block) to
  # generate a sensible grid. Also returns a pre-formatted comma-separated
  # string for direct use in the frontend text input.
  keepx_recommendation  <- NULL
  keepx_default_string  <- "5, 10, 15, 20"   # fallback if no summary yet
  summary_path <- get_session_path(session_id, "preprocessing_summary.json")
  if (file.exists(summary_path)) {
    tryCatch({
      ps <- jsonlite::fromJSON(summary_path, simplifyVector = TRUE)
      per_dataset <- ps$per_dataset
      if (!is.null(per_dataset) && length(per_dataset) > 0) {
        retained_counts <- sapply(per_dataset, function(d) {
          as.integer(d$features_retained %||% 0L)
        })
        pos_counts <- retained_counts[retained_counts > 0]
        if (length(pos_counts) > 0) {
          min_features <- min(pos_counts)
          rec <- recommend_keepx(min_features)
          keepx_recommendation <- rec
          keepx_default_string <- paste(unlist(rec), collapse = ", ")
        }
      }
    }, error = function(e) NULL)
  }

  ok_response(list(
    grouping_column          = "Group",
    shared_sample_count      = length(shared_ids),
    n_matrix_blocks          = length(matrix_files),
    matrix_block_names       = tools::file_path_sans_ext(basename(matrix_files)),
    descriptor_columns       = as.list(descriptor_cols),
    available_groups         = available_groups,
    recommended_default      = as.list(recommended_default),
    keepx_recommendation     = keepx_recommendation,
    keepx_default_string     = keepx_default_string
  ))
}

# ---------------------------------------------------------------------------
# recommend_keepx
# Generates a sensible keepX grid given the smallest block feature count.
# ---------------------------------------------------------------------------
recommend_keepx <- function(min_features) {
  fixed    <- c(5L, 10L, 15L, 20L)
  pct_vals <- unique(round(min_features * c(0.05, 0.10, 0.20, 0.30)))
  combined <- sort(unique(c(fixed, pct_vals)))
  combined <- combined[combined >= 1 & combined <= min_features]
  if (length(combined) > 8) combined <- combined[1:8]
  as.list(as.integer(combined))
}

# ---------------------------------------------------------------------------
# save_comparison_params
# Validates and persists the user's group selection.
# grouping_column is always "Group" — not user-selectable.
# Fields expected in params:
#   selected_groups — character vector (>= 2 groups, each >= 3 samples)
# ---------------------------------------------------------------------------
save_comparison_params <- function(session_id, params) {
  selected_groups <- params$selected_groups

  if (is.null(selected_groups) || length(selected_groups) < 2) {
    return(error_response("INSUFFICIENT_GROUPS",
                          "At least 2 groups must be selected for DIABLO.",
                          "comparison", recoverable = TRUE))
  }

  loaded <- .load_session_files(session_id)
  if (!is.null(loaded$status) && loaded$status == "error") return(loaded)

  meta       <- loaded$meta
  shared_ids <- loaded$shared_ids

  meta_filtered <- meta[meta$SampleID %in% shared_ids, , drop = FALSE]

  if (!"Group" %in% colnames(meta_filtered)) {
    return(error_response("MISSING_GROUP_COLUMN",
                          "Metadata is missing the required 'Group' column.",
                          "comparison", recoverable = TRUE))
  }

  meta_filtered$Group <- trimws(as.character(meta_filtered$Group))

  # Filter to selected groups
  meta_selected <- meta_filtered[meta_filtered$Group %in% selected_groups, , drop = FALSE]

  if (nrow(meta_selected) == 0) {
    return(error_response("INSUFFICIENT_GROUPS",
                          "No samples found for the selected groups.",
                          "comparison", recoverable = TRUE))
  }

  # Validate minimum samples per group
  counts <- table(meta_selected$Group)
  small  <- names(counts)[counts < 3]
  if (length(small) > 0) {
    return(error_response("INSUFFICIENT_SAMPLES",
                          paste0("Groups with fewer than 3 samples: ",
                                 paste(small, collapse = ", "),
                                 ". DIABLO requires at least 3 samples per group for cross-validation."),
                          "comparison", recoverable = TRUE))
  }

  sample_counts <- as.list(counts[names(counts) %in% selected_groups])
  retained_ids  <- meta_selected$SampleID

  comparison_params <- list(
    grouping_column         = "Group",
    selected_groups         = as.list(selected_groups),
    sample_ids_retained     = as.list(retained_ids),
    sample_counts_per_group = sample_counts,
    defined_at              = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )

  params_dir <- get_session_path(session_id, "params")
  dir.create(params_dir, recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(comparison_params,
                       file.path(params_dir, "comparison_params.json"),
                       auto_unbox = TRUE, pretty = TRUE)

  state <- read_pipeline_state(session_id)
  state$steps$comparison <- "complete"
  write_pipeline_state(session_id, state)

  ok_response(list(
    grouping_column         = "Group",
    selected_groups         = as.list(selected_groups),
    sample_counts_per_group = sample_counts,
    n_samples_retained      = length(retained_ids)
  ))
}
