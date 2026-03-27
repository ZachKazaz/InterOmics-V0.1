# plumber.R — Multi-Omics Platform API
# All routes prefixed /api/v1
# Tasks: 3.1 – 3.12

library(plumber)
library(jsonlite)

source("R/utils.R")
source("R/validator.R")
source("R/preprocessor.R")
source("R/id_mapper.R")
source("R/comparison_module.R")
source("R/diablo_module.R")
source("R/diablo_worker_helpers.R")
source("R/diablo_plot_regen.R")
source("R/correlation_module.R")
source("R/enrichment_module.R")
source("R/stats_module.R")
source("R/exporter.R")

# ---------------------------------------------------------------------------
# Helper: parse JSON body from request
# ---------------------------------------------------------------------------
.parse_body <- function(req) {
  body <- req$postBody
  if (is.null(body) || nchar(body) == 0) return(list())
  tryCatch(fromJSON(body, simplifyVector = TRUE), error = function(e) list())
}

# ---------------------------------------------------------------------------
# 3.1  Session endpoints
# ---------------------------------------------------------------------------

#* @post /api/v1/session
#* @serializer unboxedJSON
function(req, res) {
  result <- create_session()
  list(session_id = result)
}

#* @get /api/v1/<session_id>/state
#* @serializer unboxedJSON
function(session_id, req, res) {
  state_path <- get_session_path(session_id, "pipeline_state.json")
  if (!file.exists(state_path)) {
    res$status <- 404
    return(error_response("SESSION_NOT_FOUND",
                          paste0("Session '", session_id, "' not found."),
                          "api", recoverable = FALSE))
  }
  read_pipeline_state(session_id)
}

# ---------------------------------------------------------------------------
# 3.2  Upload endpoint
# ---------------------------------------------------------------------------

#* @post /api/v1/<session_id>/upload
#* @serializer unboxedJSON
function(session_id, req, res) {
  tryCatch(
    .do_upload(session_id, req, res),
    error = function(e) {
      message("[upload] UNCAUGHT ERROR: ", conditionMessage(e))
      res$status <- 500
      error_response("UPLOAD_PARSE_ERROR",
                     paste0("Upload failed due to an internal error: ", conditionMessage(e)),
                     "upload", recoverable = TRUE)
    }
  )
}

# Separated so tryCatch above can catch any runtime error and return JSON.
.do_upload <- function(session_id, req, res) {
  body <- req$body

  # ---- Debug: log body keys and top-level structure ------------------------
  message("[upload] req$body keys: ", paste(names(body), collapse = ", "))
  for (k in names(body)) {
    v <- body[[k]]
    sub_keys <- if (is.list(v)) paste(names(v), collapse = ", ") else "(not a list)"
    message("[upload]   key='", k, "'  class=", paste(class(v), collapse = "/"),
            "  is.list=", is.list(v), "  sub-keys=[", sub_keys, "]")
  }

  # ---- Normalization layer --------------------------------------------------
  # plumber/webutils delivers multipart entries in one of two shapes:
  #
  #   Shape A — disk-backed (older plumber or large files):
  #     list(datapath = "/tmp/Rtmp.../...", filename = "foo.csv", ...)
  #
  #   Shape B — in-memory (webutils default for small files):
  #     list(value = <raw>, parsed = "col1,col2\n...", filename = "foo.csv",
  #          name = "matrices[0]", content_type = "text/csv",
  #          content_disposition = "...")
  #
  # normalize_entry() returns list(datapath, filename, via) or NULL.
  # 'via' records how the file was materialized (for debug logging).

  normalize_entry <- function(x, field_key) {
    if (is.null(x)) return(NULL)

    # Unwrap single-element wrapper (some plumber versions nest the object)
    if (is.list(x) && length(x) == 1 && is.list(x[[1]])) x <- x[[1]]
    if (!is.list(x)) return(NULL)

    # Derive original filename from filename or name fields
    fn <- NULL
    for (fk in c("filename", "name")) {
      cand <- x[[fk]]
      if (!is.null(cand) && length(cand) == 1 && nchar(trimws(as.character(cand))) > 0) {
        fn <- trimws(as.character(cand))
        # 'name' is the form field key (e.g. "matrices[0]"), not the filename —
        # skip it if it looks like a field key rather than a filename
        if (fk == "name" && !grepl("\\.", fn)) fn <- NULL else break
      }
    }
    fn <- fn %||% paste0(gsub("[^A-Za-z0-9]", "_", field_key), ".csv")

    # ---- Shape A: datapath already on disk ----------------------------------
    dp_cand <- x[["datapath"]]
    if (!is.null(dp_cand) && length(dp_cand) == 1) {
      dp <- trimws(as.character(dp_cand))
      if (nchar(dp) > 0 && file.exists(dp)) {
        message("[upload]   '", field_key, "' resolved via datapath: ", dp)
        return(list(datapath = dp, filename = fn, via = "datapath"))
      }
    }

    # ---- Shape B-1: parsed character content --------------------------------
    parsed_cand <- x[["parsed"]]
    if (!is.null(parsed_cand) && is.character(parsed_cand) && length(parsed_cand) >= 1) {
      txt <- paste(parsed_cand, collapse = "\n")
      if (nchar(trimws(txt)) > 0) {
        tmp <- tempfile(fileext = ".csv")
        writeLines(txt, tmp)
        message("[upload]   '", field_key, "' resolved via parsed text -> ", tmp)
        return(list(datapath = tmp, filename = fn, via = "parsed"))
      }
    }

    # ---- Shape B-2: raw bytes -----------------------------------------------
    raw_cand <- x[["value"]]
    if (!is.null(raw_cand) && is.raw(raw_cand) && length(raw_cand) > 0) {
      tmp <- tempfile(fileext = ".csv")
      writeBin(raw_cand, tmp)
      message("[upload]   '", field_key, "' resolved via raw bytes -> ", tmp)
      return(list(datapath = tmp, filename = fn, via = "raw"))
    }

    message("[upload]   '", field_key, "' could not be resolved (no datapath/parsed/raw)")
    NULL
  }

  is_resolved <- function(x) !is.null(x) && nchar(x[["datapath"]]) > 0

  # ---- Extract matrix files -------------------------------------------------
  # Frontend sends matrices[0], matrices[1], … (indexed keys)
  # Also accept plain "matrices" or "matrices[]" for robustness
  matrix_keys <- grep("^matrices", names(body), value = TRUE)
  matrix_files <- Filter(is_resolved,
    lapply(matrix_keys, function(k) normalize_entry(body[[k]], k))
  )

  # ---- Extract metadata file ------------------------------------------------
  metadata_file <- normalize_entry(body[["metadata"]], "metadata")

  n_matrices <- length(matrix_files)
  message("[upload] matrix files resolved: ", n_matrices,
          " | metadata resolved: ", is_resolved(metadata_file))
  for (i in seq_along(matrix_files)) {
    mf <- matrix_files[[i]]
    message("[upload]   matrix[", i, "] filename=", mf$filename,
            "  via=", mf$via, "  datapath=", mf$datapath)
  }
  if (is_resolved(metadata_file)) {
    message("[upload]   metadata filename=", metadata_file$filename,
            "  via=", metadata_file$via, "  datapath=", metadata_file$datapath)
  }

  # ---- Validate -------------------------------------------------------------
  if (n_matrices == 0) {
    res$status <- 400
    return(error_response("NO_FILES_FOUND",
                          paste0("No feature matrix files received. ",
                                 "Body keys: [", paste(names(body), collapse = ", "), "]. ",
                                 "Expected keys like matrices[0], matrices[1], etc."),
                          "upload", recoverable = TRUE))
  }
  if (n_matrices > 4) {
    res$status <- 400
    return(error_response("DATASET_LIMIT_EXCEEDED",
                          paste0("Maximum 4 feature matrix files allowed. ",
                                 "Received ", n_matrices, ". Metadata is excluded from this count."),
                          "upload", recoverable = TRUE))
  }
  if (!is_resolved(metadata_file)) {
    res$status <- 400
    return(error_response("NO_FILES_FOUND",
                          "A metadata file is required (field name: 'metadata').",
                          "upload", recoverable = TRUE))
  }

  # ---- Clear previous raw files so re-uploads don't accumulate --------------
  raw_dir <- get_session_path(session_id, "raw")
  if (dir.exists(raw_dir)) {
    old_files <- list.files(raw_dir, full.names = TRUE)
    file.remove(old_files[file.exists(old_files)])
  }
  dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)

  received <- character(0)

  for (i in seq_along(matrix_files)) {
    mf    <- matrix_files[[i]]
    fname <- if (nchar(mf[["filename"]]) > 0) mf[["filename"]] else paste0("matrix_", i, ".csv")
    file.copy(mf[["datapath"]], file.path(raw_dir, fname), overwrite = TRUE)
    received <- c(received, fname)
  }

  meta_fname <- if (nchar(metadata_file[["filename"]]) > 0) metadata_file[["filename"]] else "metadata.csv"
  file.copy(metadata_file[["datapath"]], file.path(raw_dir, meta_fname), overwrite = TRUE)
  received <- c(received, meta_fname)

  state <- read_pipeline_state(session_id)
  state$steps$upload <- "complete"
  # Uploading new data invalidates all downstream steps
  state$steps$validate   <- "pending"
  state$steps$comparison <- "pending"
  state$steps$preprocess <- "pending"
  state$steps$diablo     <- "pending"
  write_pipeline_state(session_id, state)

  # Clear any stale DIABLO job/results so the old run is not reused
  tryCatch(reset_diablo_job(session_id), error = function(e)
    message("[upload] WARNING: DIABLO reset failed: ", conditionMessage(e)))

  list(
    session_id     = session_id,
    files_received = as.list(received),
    status         = "uploaded"
  )
}

# ---------------------------------------------------------------------------
# 3.3  Validation endpoints
# ---------------------------------------------------------------------------

#* @post /api/v1/<session_id>/validate
#* @serializer unboxedJSON
function(session_id, req, res) {
  if (!check_prerequisite(session_id, "upload")) {
    res$status <- 409
    return(error_response("PREREQUISITE_NOT_MET",
                          "Upload must be complete before validation.",
                          "api", recoverable = TRUE))
  }
  run_validation(session_id)
}

#* @get /api/v1/<session_id>/validate
#* @serializer unboxedJSON
function(session_id, req, res) {
  report_path <- get_session_path(session_id, "validation_report.json")
  if (!file.exists(report_path)) {
    res$status <- 404
    return(error_response("NO_FILES_FOUND",
                          "Validation report not found. Run validation first.",
                          "api", recoverable = TRUE))
  }
  fromJSON(report_path, simplifyVector = FALSE)
}

# ---------------------------------------------------------------------------
# 3.3b  Comparison definition endpoints
# ---------------------------------------------------------------------------

#* @get /api/v1/<session_id>/comparison/candidates
#* @serializer unboxedJSON
function(session_id, req, res) {
  if (!check_prerequisite(session_id, "validate")) {
    res$status <- 409
    return(error_response("PREREQUISITE_NOT_MET",
                          "Validation must be complete before defining a comparison.",
                          "api", recoverable = TRUE))
  }
  get_group_table(session_id)
}

#* @post /api/v1/<session_id>/comparison/define
#* @serializer unboxedJSON
function(session_id, req, res) {
  if (!check_prerequisite(session_id, "validate")) {
    res$status <- 409
    return(error_response("PREREQUISITE_NOT_MET",
                          "Validation must be complete before defining a comparison.",
                          "api", recoverable = TRUE))
  }
  params <- .parse_body(req)
  save_comparison_params(session_id, params)
}

# ---------------------------------------------------------------------------
# 3.4  Preprocessing endpoints
# ---------------------------------------------------------------------------

#* @post /api/v1/<session_id>/preprocess
#* @serializer unboxedJSON
function(session_id, req, res) {
  if (!check_prerequisite(session_id, "validate")) {
    res$status <- 409
    return(error_response("PREREQUISITE_NOT_MET",
                          "Validation must be complete before preprocessing.",
                          "api", recoverable = TRUE))
  }
  # Also require comparison to be defined
  comp_path <- get_session_path(session_id, "params", "comparison_params.json")
  if (!file.exists(comp_path)) {
    res$status <- 409
    return(error_response("PREREQUISITE_NOT_MET",
                          "Comparison must be defined before preprocessing.",
                          "api", recoverable = TRUE))
  }
  params <- .parse_body(req)
  run_preprocessing(session_id, params)
}

#* @get /api/v1/<session_id>/preprocess/summary
#* @serializer unboxedJSON
function(session_id, req, res) {
  summary_path <- get_session_path(session_id, "preprocessing_summary.json")
  if (!file.exists(summary_path)) {
    res$status <- 404
    return(error_response("NO_FILES_FOUND",
                          "Preprocessing summary not found. Run preprocessing first.",
                          "api", recoverable = TRUE))
  }
  fromJSON(summary_path, simplifyVector = FALSE)
}

# ---------------------------------------------------------------------------
# 3.5  Feature ID mapping endpoints
# ---------------------------------------------------------------------------

#* @post /api/v1/<session_id>/map-ids
#* @serializer unboxedJSON
function(session_id, req, res) {
  if (!check_prerequisite(session_id, "preprocess")) {
    res$status <- 409
    return(error_response("PREREQUISITE_NOT_MET",
                          "Preprocessing must be complete before ID mapping.",
                          "api", recoverable = TRUE))
  }
  run_id_mapping(session_id)
}

#* @get /api/v1/<session_id>/map-ids
#* @serializer unboxedJSON
function(session_id, req, res) {
  mapping_path <- get_session_path(session_id, "id_mapping.json")
  if (!file.exists(mapping_path)) {
    res$status <- 404
    return(error_response("NO_FILES_FOUND",
                          "ID mapping results not found. Run ID mapping first.",
                          "api", recoverable = TRUE))
  }
  fromJSON(mapping_path, simplifyVector = FALSE)
}

# ---------------------------------------------------------------------------
# 3.6  DIABLO endpoints
# ---------------------------------------------------------------------------

#* @post /api/v1/<session_id>/analysis/diablo
#* @serializer unboxedJSON
function(session_id, req, res) {
  message("[diablo:route] POST /analysis/diablo — session: ", session_id)
  if (!check_prerequisite(session_id, "preprocess")) {
    message("[diablo:route] BLOCKED — preprocess prerequisite not met")
    res$status <- 409
    return(error_response("PREREQUISITE_NOT_MET",
                          "Preprocessing must be complete before running DIABLO.",
                          "api", recoverable = TRUE))
  }
  params <- .parse_body(req)
  message("[diablo:route] body parsed — keys: ", paste(names(params), collapse = ", "))
  submit_diablo_job(session_id, params)
}

#* @get /api/v1/<session_id>/analysis/diablo/status
#* @serializer unboxedJSON
function(session_id, req, res) {
  job_path <- get_session_path(session_id, "jobs", "diablo_job.json")
  if (!file.exists(job_path)) {
    res$status <- 404
    return(error_response("NO_FILES_FOUND",
                          "No DIABLO job found for this session.",
                          "diablo", recoverable = TRUE))
  }
  fromJSON(job_path, simplifyVector = FALSE)
}

#* @post /api/v1/<session_id>/analysis/diablo/cancel
#* @serializer unboxedJSON
function(session_id, req, res) {
  cancel_diablo_job(session_id)
}

#* @post /api/v1/<session_id>/analysis/diablo/restart
#* @serializer unboxedJSON
function(session_id, req, res) {
  if (!check_prerequisite(session_id, "preprocess")) {
    res$status <- 409
    return(error_response("PREREQUISITE_NOT_MET",
                          "Preprocessing must be complete before running DIABLO.",
                          "api", recoverable = TRUE))
  }
  params <- .parse_body(req)
  restart_diablo_job(session_id, params)
}

#* @post /api/v1/<session_id>/analysis/diablo/reset
#* @serializer unboxedJSON
function(session_id, req, res) {
  reset_diablo_job(session_id)
}

# ---------------------------------------------------------------------------
# 3.6b  DIABLO plot-regeneration endpoints (no model rerun)
# ---------------------------------------------------------------------------

#* @post /api/v1/<session_id>/analysis/diablo/regen/network
#* @serializer unboxedJSON
function(session_id, req, res) {
  params <- .parse_body(req)
  regen_network(session_id, params)
}

#* @post /api/v1/<session_id>/analysis/diablo/regen/circos
#* @serializer unboxedJSON
function(session_id, req, res) {
  params <- .parse_body(req)
  regen_circos(session_id, params)
}

#* @post /api/v1/<session_id>/analysis/diablo/regen/scores
#* @serializer unboxedJSON
function(session_id, req, res) {
  params <- .parse_body(req)
  regen_scores_plot(session_id, params)
}

#* @post /api/v1/<session_id>/analysis/diablo/regen/loadings
#* @serializer unboxedJSON
function(session_id, req, res) {
  params <- .parse_body(req)
  regen_loadings_plot(session_id, params)
}

#* @post /api/v1/<session_id>/analysis/diablo/regen/consensus-loadings
#* @serializer unboxedJSON
function(session_id, req, res) {
  params <- .parse_body(req)
  regen_consensus_loadings(session_id, params)
}

#* @post /api/v1/<session_id>/analysis/diablo/regen/consensus-vip
#* @serializer unboxedJSON
function(session_id, req, res) {
  params <- .parse_body(req)
  regen_consensus_vip(session_id, params)
}

#* @get /api/v1/<session_id>/analysis/diablo/results
#* @serializer unboxedJSON
function(session_id, req, res) {
  job_path <- get_session_path(session_id, "jobs", "diablo_job.json")
  if (!file.exists(job_path)) {
    res$status <- 404
    return(error_response("NO_FILES_FOUND",
                          "No DIABLO job found for this session.",
                          "diablo", recoverable = TRUE))
  }
  job <- fromJSON(job_path, simplifyVector = FALSE)
  if (job$status != "complete") {
    res$status <- 409
    return(error_response("PREREQUISITE_NOT_MET",
                          paste0("DIABLO job is not complete. Current status: ", job$status),
                          "diablo", recoverable = TRUE))
  }

  results_dir  <- get_session_path(session_id, "results", "diablo")
  summary_path <- file.path(results_dir, "diablo_results_summary.json")

  # Prefer the structured summary written by the worker
  if (file.exists(summary_path)) {
    summary <- tryCatch(
      fromJSON(summary_path, simplifyVector = FALSE),
      error = function(e) NULL
    )
    if (!is.null(summary)) {
      summary$status <- "complete"
      return(summary)
    }
  }

  # Fallback: reconstruct from raw files (handles older runs without summary)
  perf_path <- file.path(results_dir, "performance.json")
  perf      <- if (file.exists(perf_path))
                 tryCatch(fromJSON(perf_path, simplifyVector = FALSE), error = function(e) NULL)
               else NULL

  png_files <- list.files(results_dir, pattern = "\\.png$", full.names = FALSE)
  plots     <- setNames(
    lapply(png_files, function(f)
      paste0("/api/v1/", session_id, "/plots/diablo/", f)),
    tools::file_path_sans_ext(png_files)
  )

  list(
    status      = "complete",
    seed_used   = job$seed_used,
    performance = perf,
    plots       = plots,
    tables = list(
      selected_features = paste0("/api/v1/", session_id, "/results/diablo/selected_features.csv"),
      ranked_features   = paste0("/api/v1/", session_id, "/results/diablo/ranked_features.csv")
    ),
    warnings = list()
  )
}

# ---------------------------------------------------------------------------
# 3.7  Single-dataset correlation endpoints
# ---------------------------------------------------------------------------

#* @post /api/v1/<session_id>/analysis/correlation/single
#* @serializer unboxedJSON
function(session_id, req, res) {
  if (!check_prerequisite(session_id, "preprocess")) {
    res$status <- 409
    return(error_response("PREREQUISITE_NOT_MET",
                          "Preprocessing must be complete before correlation analysis.",
                          "api", recoverable = TRUE))
  }
  params <- .parse_body(req)
  run_correlation_single(session_id, params)
}

#* @get /api/v1/<session_id>/analysis/correlation/single/results
#* @serializer unboxedJSON
function(session_id, req, res) {
  results_dir <- get_session_path(session_id, "results", "correlation")
  edge_path   <- file.path(results_dir, "edge_list.csv")
  if (!file.exists(edge_path)) {
    res$status <- 404
    return(error_response("NO_FILES_FOUND",
                          "Correlation results not found. Run correlation analysis first.",
                          "correlation", recoverable = TRUE))
  }
  plot_files <- list.files(results_dir, pattern = "\\.png$", full.names = FALSE)
  list(
    edge_list    = paste0("/api/v1/", session_id, "/results/correlation/edge_list.csv"),
    node_metrics = paste0("/api/v1/", session_id, "/results/correlation/node_metrics.csv"),
    plots        = lapply(plot_files, function(f)
      paste0("/api/v1/", session_id, "/plots/correlation/", f))
  )
}

# ---------------------------------------------------------------------------
# 3.8  Cross-dataset correlation endpoints
# ---------------------------------------------------------------------------

#* @post /api/v1/<session_id>/analysis/correlation/cross
#* @serializer unboxedJSON
function(session_id, req, res) {
  if (!check_prerequisite(session_id, "preprocess")) {
    res$status <- 409
    return(error_response("PREREQUISITE_NOT_MET",
                          "Preprocessing must be complete before cross-dataset correlation.",
                          "api", recoverable = TRUE))
  }
  params <- .parse_body(req)
  run_correlation_cross(session_id, params)
}

#* @get /api/v1/<session_id>/analysis/correlation/cross/results
#* @serializer unboxedJSON
function(session_id, req, res) {
  results_dir <- get_session_path(session_id, "results", "correlation", "cross")
  edge_path   <- file.path(results_dir, "edge_list.csv")
  if (!file.exists(edge_path)) {
    res$status <- 404
    return(error_response("NO_FILES_FOUND",
                          "Cross-dataset correlation results not found.",
                          "correlation", recoverable = TRUE))
  }
  plot_files <- list.files(results_dir, pattern = "\\.png$", full.names = FALSE)
  list(
    edge_list    = paste0("/api/v1/", session_id, "/results/correlation/cross/edge_list.csv"),
    node_metrics = paste0("/api/v1/", session_id, "/results/correlation/cross/node_metrics.csv"),
    plots        = lapply(plot_files, function(f)
      paste0("/api/v1/", session_id, "/plots/correlation/cross/", f))
  )
}

# ---------------------------------------------------------------------------
# 3.9  Enrichment endpoints
# ---------------------------------------------------------------------------

#* @post /api/v1/<session_id>/analysis/enrichment/ora
#* @serializer unboxedJSON
function(session_id, req, res) {
  if (!check_prerequisite(session_id, "map_ids")) {
    res$status <- 409
    return(error_response("PREREQUISITE_NOT_MET",
                          "ID mapping must be complete before ORA.",
                          "api", recoverable = TRUE))
  }
  params <- .parse_body(req)
  run_ora(session_id, params)
}

#* @get /api/v1/<session_id>/analysis/enrichment/ora/results
#* @serializer unboxedJSON
function(session_id, req, res) {
  results_dir <- get_session_path(session_id, "results", "enrichment")
  ora_path    <- file.path(results_dir, "ora_results.csv")
  if (!file.exists(ora_path)) {
    res$status <- 404
    return(error_response("NO_FILES_FOUND",
                          "ORA results not found. Run ORA first.",
                          "enrichment", recoverable = TRUE))
  }
  plot_files <- list.files(results_dir, pattern = "^ora.*\\.png$", full.names = FALSE)
  list(
    ora_results = paste0("/api/v1/", session_id, "/results/enrichment/ora_results.csv"),
    plots       = lapply(plot_files, function(f)
      paste0("/api/v1/", session_id, "/plots/enrichment/", f))
  )
}

#* @post /api/v1/<session_id>/analysis/enrichment/topology
#* @serializer unboxedJSON
function(session_id, req, res) {
  ora_path <- get_session_path(session_id, "results", "enrichment", "ora_results.csv")
  if (!file.exists(ora_path)) {
    res$status <- 409
    return(error_response("PREREQUISITE_NOT_MET",
                          "ORA must be run before pathway topology analysis.",
                          "api", recoverable = TRUE))
  }
  params <- .parse_body(req)
  run_topology(session_id, params)
}

#* @get /api/v1/<session_id>/analysis/enrichment/topology/results
#* @serializer unboxedJSON
function(session_id, req, res) {
  results_dir <- get_session_path(session_id, "results", "enrichment")
  topo_path   <- file.path(results_dir, "topology_results.csv")
  if (!file.exists(topo_path)) {
    res$status <- 404
    return(error_response("NO_FILES_FOUND",
                          "Topology results not found. Run topology analysis first.",
                          "enrichment", recoverable = TRUE))
  }
  list(
    topology_results = paste0("/api/v1/", session_id, "/results/enrichment/topology_results.csv"),
    plots = list(
      topology_bubble = paste0("/api/v1/", session_id, "/plots/enrichment/topology_bubble.png")
    )
  )
}

#* @post /api/v1/<session_id>/analysis/enrichment/lipid-class
#* @serializer unboxedJSON
function(session_id, req, res) {
  if (!check_prerequisite(session_id, "map_ids")) {
    res$status <- 409
    return(error_response("PREREQUISITE_NOT_MET",
                          "ID mapping must be complete before lipid class enrichment.",
                          "api", recoverable = TRUE))
  }
  params <- .parse_body(req)
  run_lipid_enrichment(session_id, params)
}

#* @get /api/v1/<session_id>/analysis/enrichment/lipid-class/results
#* @serializer unboxedJSON
function(session_id, req, res) {
  results_dir <- get_session_path(session_id, "results", "enrichment")
  lipid_path  <- file.path(results_dir, "lipid_enrichment_results.csv")
  if (!file.exists(lipid_path)) {
    res$status <- 404
    return(error_response("NO_FILES_FOUND",
                          "Lipid enrichment results not found.",
                          "enrichment", recoverable = TRUE))
  }
  list(
    lipid_results = paste0("/api/v1/", session_id, "/results/enrichment/lipid_enrichment_results.csv"),
    plots = list(
      lipid_bar    = paste0("/api/v1/", session_id, "/plots/enrichment/lipid_bar.png"),
      lipid_bubble = paste0("/api/v1/", session_id, "/plots/enrichment/lipid_bubble.png")
    )
  )
}

# ---------------------------------------------------------------------------
# 3.10  Statistical modeling endpoints
# ---------------------------------------------------------------------------

#* @post /api/v1/<session_id>/analysis/stats
#* @serializer unboxedJSON
function(session_id, req, res) {
  if (!check_prerequisite(session_id, "preprocess")) {
    res$status <- 409
    return(error_response("PREREQUISITE_NOT_MET",
                          "Preprocessing must be complete before statistical modeling.",
                          "api", recoverable = TRUE))
  }
  params <- .parse_body(req)
  run_stats(session_id, params)
}

#* @get /api/v1/<session_id>/analysis/stats/results
#* @serializer unboxedJSON
function(session_id, req, res) {
  results_dir <- get_session_path(session_id, "results", "stats")
  feat_path   <- file.path(results_dir, "feature_results.csv")
  if (!file.exists(feat_path)) {
    res$status <- 404
    return(error_response("NO_FILES_FOUND",
                          "Stats results not found. Run statistical modeling first.",
                          "stats", recoverable = TRUE))
  }
  plot_files <- list.files(results_dir, pattern = "\\.png$", full.names = FALSE)
  list(
    feature_results          = paste0("/api/v1/", session_id, "/results/stats/feature_results.csv"),
    main_effects_summary     = paste0("/api/v1/", session_id, "/results/stats/main_effects_summary.csv"),
    interaction_effects_summary = paste0("/api/v1/", session_id, "/results/stats/interaction_effects_summary.csv"),
    plots = lapply(plot_files, function(f)
      paste0("/api/v1/", session_id, "/plots/stats/", f))
  )
}

# ---------------------------------------------------------------------------
# 3.11  Plot retrieval endpoint
# ---------------------------------------------------------------------------

#* @get /api/v1/<session_id>/plots/<module>/<filename>
#* @serializer contentType list(type="image/png")
function(session_id, module, filename, req, res) {
  # Sanitise filename to prevent path traversal
  filename <- basename(filename)
  ext      <- tolower(tools::file_ext(filename))

  file_path <- get_session_path(session_id, "results", module, filename)

  # Also check cross/ subdirectory for correlation cross plots
  if (!file.exists(file_path)) {
    file_path <- get_session_path(session_id, "results", module, "cross", filename)
  }

  if (!file.exists(file_path)) {
    res$status <- 404
    res$setHeader("Content-Type", "application/json")
    return(error_response("NO_FILES_FOUND",
                          paste0("Plot file '", filename, "' not found for module '", module, "'."),
                          "api", recoverable = TRUE))
  }

  content_type <- switch(ext,
    png = "image/png",
    pdf = "application/pdf",
    "application/octet-stream"
  )
  res$setHeader("Content-Type", content_type)
  readBin(file_path, "raw", file.info(file_path)$size)
}

# ---------------------------------------------------------------------------
# 3.11b  Result file download endpoint (CSV, JSON, PDF)
# ---------------------------------------------------------------------------

#* @get /api/v1/<session_id>/results/<module>/<filename>
#* @serializer contentType list(type="application/octet-stream")
function(session_id, module, filename, req, res) {
  filename  <- basename(filename)
  ext       <- tolower(tools::file_ext(filename))
  file_path <- get_session_path(session_id, "results", module, filename)

  if (!file.exists(file_path)) {
    res$status <- 404
    res$setHeader("Content-Type", "application/json")
    return(error_response("NO_FILES_FOUND",
                          paste0("Result file '", filename, "' not found for module '", module, "'."),
                          "api", recoverable = TRUE))
  }

  content_type <- switch(ext,
    csv  = "text/csv",
    json = "application/json",
    pdf  = "application/pdf",
    png  = "image/png",
    "application/octet-stream"
  )
  res$setHeader("Content-Type", content_type)
  res$setHeader("Content-Disposition",
                paste0("attachment; filename=\"", filename, "\""))
  readBin(file_path, "raw", file.info(file_path)$size)
}

# ---------------------------------------------------------------------------
# 3.12  Export endpoint
# ---------------------------------------------------------------------------

#* @post /api/v1/<session_id>/export
#* @serializer contentType list(type="application/zip")
function(session_id, req, res) {
  params  <- .parse_body(req)
  modules <- params$modules %||% c("diablo", "correlation", "enrichment", "stats")
  formats <- params$formats %||% c("csv", "png", "pdf")

  result <- run_export(session_id, modules, formats)

  if (!is.null(result$status) && result$status == "error") {
    res$status <- 500
    res$setHeader("Content-Type", "application/json")
    return(result)
  }

  zip_path  <- result$data$zip_path
  omissions <- result$data$omissions

  # Surface omissions in response header as JSON
  if (length(omissions) > 0) {
    res$setHeader("X-Export-Omissions", toJSON(omissions, auto_unbox = TRUE))
  }

  res$setHeader("Content-Disposition",
                paste0("attachment; filename=\"export_", session_id, ".zip\""))
  readBin(zip_path, "raw", file.info(zip_path)$size)
}
