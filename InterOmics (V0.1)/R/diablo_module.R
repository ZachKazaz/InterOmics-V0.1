# diablo_module.R - DIABLO multi-omics integration via mixOmics
# Tasks: 2.4.1, 2.4.2, 2.4.3

library(mixOmics)
library(callr)
library(jsonlite)
library(uuid)
# .now_utc() lives in utils.R - sourced by both main process and worker subprocess

# ---------------------------------------------------------------------------
# submit_diablo_job - task 2.4.1
# ---------------------------------------------------------------------------
submit_diablo_job <- function(session_id, params) {
  message("[diablo:submit] â”€â”€ ROUTE ENTERED â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€")
  message("[diablo:submit] session_id:  ", session_id)
  message("[diablo:submit] raw params:  ", paste(names(params), collapse = ", "))
  message("[diablo:submit] ncomp_max:   ", params$ncomp_max)
  message("[diablo:submit] keepX_grid:  ", paste(params$keepX_grid, collapse = ", "))

  job_path <- get_session_path(session_id, "jobs", "diablo_job.json")

  # ---- Validate session path ----
  session_dir <- get_session_path(session_id)
  if (!dir.exists(session_dir)) {
    message("[diablo:submit] ERROR - session directory not found: ", session_dir)
    return(error_response("SESSION_NOT_FOUND",
                          paste0("Session directory not found: ", session_dir),
                          "diablo", recoverable = FALSE))
  }

  # ---- Check comparison params ----
  params_path <- get_session_path(session_id, "params", "comparison_params.json")
  message("[diablo:submit] comparison params path:   ", params_path)
  message("[diablo:submit] comparison params exists: ", file.exists(params_path))
  if (!file.exists(params_path)) {
    message("[diablo:submit] ERROR - comparison params missing, returning early")
    return(error_response("MISSING_COMPARISON_PARAMS",
                          "Comparison parameters not found. Complete the Define Comparison step first.",
                          "diablo", recoverable = TRUE))
  }

  # ---- Check preprocessed matrices ----
  preprocessed_dir <- get_session_path(session_id, "preprocessed")
  matrix_files_check <- list.files(preprocessed_dir, pattern = "\\.csv$", full.names = TRUE)
  matrix_files_check <- matrix_files_check[!grepl("metadata", basename(matrix_files_check), ignore.case = TRUE)]
  message("[diablo:submit] preprocessed dir:     ", preprocessed_dir)
  message("[diablo:submit] preprocessed matrices: ", paste(basename(matrix_files_check), collapse = ", "))
  if (length(matrix_files_check) < 2) {
    message("[diablo:submit] ERROR - insufficient preprocessed matrices: ", length(matrix_files_check))
    return(error_response("INSUFFICIENT_PREPROCESSED_DATA",
                          paste0("Expected at least 2 preprocessed matrices, found: ",
                                 length(matrix_files_check), "."),
                          "diablo", recoverable = TRUE))
  }

  # ---- Validate params ----
  ncomp_val <- suppressWarnings(as.integer(params$ncomp_max))
  if (is.na(ncomp_val) || ncomp_val < 1L) {
    message("[diablo:submit] ERROR - invalid ncomp_max: ", params$ncomp_max)
    return(error_response("INVALID_PARAM",
                          paste0("ncomp_max must be a positive integer. Got: ", params$ncomp_max),
                          "diablo", recoverable = TRUE))
  }
  keepX_val <- suppressWarnings(as.integer(params$keepX_grid))
  if (length(keepX_val) == 0 || any(is.na(keepX_val)) || any(keepX_val < 1L)) {
    message("[diablo:submit] ERROR - invalid keepX_grid: ", paste(params$keepX_grid, collapse = ", "))
    return(error_response("INVALID_PARAM",
                          paste0("keepX_grid must be a vector of positive integers. Got: ",
                                 paste(params$keepX_grid, collapse = ", ")),
                          "diablo", recoverable = TRUE))
  }

  # ---- Block only when a job is actively running ----
  if (file.exists(job_path)) {
    existing <- tryCatch(fromJSON(job_path), error = function(e) list(status = "unknown"))
    message("[diablo:submit] existing job status: ", existing$status)
    if (!is.null(existing$status) && existing$status == "running") {
      message("[diablo:submit] ERROR - job already running, returning JOB_ALREADY_RUNNING")
      return(error_response(
        error_code  = "JOB_ALREADY_RUNNING",
        detail      = "A DIABLO job is already running for this session.",
        module      = "diablo",
        recoverable = TRUE
      ))
    }
  }

  job_id <- UUIDgenerate()
  seed   <- sample.int(1e6, 1)
  now    <- .now_utc()

  # Write "starting" state synchronously before worker launch
  job_state <- list(
    status           = "starting",
    job_id           = job_id,
    seed_used        = seed,
    session_id       = session_id,
    ncomp_max        = ncomp_val,
    keepX_grid       = as.list(keepX_val),
    submitted_at     = now,
    started_at       = now,
    updated_at       = now,
    progress_percent = 0L,
    stage_label      = "Submitting DIABLO job",
    worker_pid       = NA_integer_
  )
  dir.create(dirname(job_path), recursive = TRUE, showWarnings = FALSE)
  write_json(job_state, job_path, auto_unbox = TRUE)
  message("[diablo:submit] job file written (status=starting): ", job_path)

  # ---- Launch background worker ----
  bg <- tryCatch(
    callr::r_bg(
      func = run_diablo_worker,
      args = list(session_id = session_id, params = params, seed = seed,
                  job_id = job_id, wd = getwd()),
      package = FALSE
    ),
    error = function(e) {
      message("[diablo:submit] ERROR - worker launch failed: ", conditionMessage(e))
      write_json(modifyList(job_state, list(
        status      = "error",
        error_code  = "WORKER_START_FAILED",
        detail      = conditionMessage(e),
        updated_at  = .now_utc(),
        stage_label = "Worker failed to start"
      )), job_path, auto_unbox = TRUE)
      NULL
    }
  )

  if (is.null(bg)) {
    return(error_response("WORKER_START_FAILED",
                          "Failed to launch DIABLO background worker.",
                          "diablo", recoverable = TRUE))
  }

  # Keep status as "starting" -- the worker itself upgrades to "running" once
  # it has successfully bootstrapped (setwd + source calls completed).
  # Only store the PID so the status route can check liveness.
  pid <- tryCatch(bg$get_pid(), error = function(e) NA_integer_)
  job_state$worker_pid <- pid
  job_state$updated_at <- .now_utc()
  write_json(job_state, job_path, auto_unbox = TRUE)
  message("[diablo:submit] worker launched - PID: ", pid, " | job_id: ", job_id)
  message("[diablo:submit] job file written (status=starting, worker owns upgrade to running)")

  ok_response(list(status = "starting", job_id = job_id, worker_pid = pid))
}

# ---------------------------------------------------------------------------
# cancel_diablo_job
# ---------------------------------------------------------------------------
cancel_diablo_job <- function(session_id) {
  job_path <- get_session_path(session_id, "jobs", "diablo_job.json")

  if (!file.exists(job_path)) {
    return(error_response(
      error_code  = "NO_JOB_FOUND",
      detail      = "No DIABLO job found for this session.",
      module      = "diablo",
      recoverable = TRUE
    ))
  }

  job <- fromJSON(job_path)

  if (!is.null(job$status) && !job$status %in% c("running", "starting")) {
    return(error_response(
      error_code  = "JOB_NOT_RUNNING",
      detail      = paste0("Cannot cancel a job with status '", job$status, "'."),
      module      = "diablo",
      recoverable = TRUE
    ))
  }

  # Attempt to terminate the worker process
  pid <- job$worker_pid
  if (!is.null(pid) && !is.na(pid) && is.numeric(pid) && pid > 0) {
    tryCatch(
      tools::pskill(as.integer(pid), signal = tools::SIGTERM),
      error = function(e) message("[diablo] pskill failed: ", conditionMessage(e))
    )
  }

  now <- .now_utc()
  write_json(list(
    status           = "cancelled",
    job_id           = job$job_id,
    seed_used        = job$seed_used,
    started_at       = job$started_at,
    updated_at       = now,
    cancelled_at     = now,
    progress_percent = job$progress_percent,
    stage_label      = "Cancelled by user",
    worker_pid       = pid
  ), job_path, auto_unbox = TRUE)

  ok_response(list(status = "cancelled", message = "Job cancelled."))
}

# ---------------------------------------------------------------------------
# reset_diablo_job - clears all DIABLO state so the user can start fresh.
# Called when: (a) new data is uploaded, (b) user clicks "Reset DIABLO Run".
# Does NOT delete raw or preprocessed data.
# ---------------------------------------------------------------------------
reset_diablo_job <- function(session_id) {
  message("[diablo:reset] session_id: ", session_id)

  job_path    <- get_session_path(session_id, "jobs", "diablo_job.json")
  results_dir <- get_session_path(session_id, "results", "diablo")

  # â”€â”€ Kill any running worker â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
  if (file.exists(job_path)) {
    job <- tryCatch(fromJSON(job_path), error = function(e) list())
    pid <- job$worker_pid
    if (!is.null(pid) && !is.na(pid) && is.numeric(pid) && pid > 0 &&
        isTRUE(job$status %in% c("running", "starting"))) {
      tryCatch(
        tools::pskill(as.integer(pid), signal = tools::SIGTERM),
        error = function(e) message("[diablo:reset] pskill failed: ", conditionMessage(e))
      )
    }
    file.remove(job_path)
    message("[diablo:reset] job file removed")
  }

  # â”€â”€ Remove result artifacts â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
  result_files <- c(
    "diablo_results_summary.json",
    "results_manifest.json",
    "performance.json",
    "selected_features.csv",
    "ranked_features.csv"
  )
  for (f in result_files) {
    p <- file.path(results_dir, f)
    if (file.exists(p)) { file.remove(p); message("[diablo:reset] removed: ", f) }
  }
  # Remove all PNGs and PDFs in results/diablo
  plot_files <- list.files(results_dir, pattern = "\\.(png|pdf)$", full.names = TRUE)
  if (length(plot_files) > 0) {
    file.remove(plot_files)
    message("[diablo:reset] removed ", length(plot_files), " plot file(s)")
  }

  # â”€â”€ Reset pipeline state â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
  tryCatch({
    state <- read_pipeline_state(session_id)
    state$steps$diablo <- "pending"
    write_pipeline_state(session_id, state)
    message("[diablo:reset] pipeline_state.steps.diablo reset to 'pending'")
  }, error = function(e) {
    message("[diablo:reset] WARNING: could not reset pipeline state: ", conditionMessage(e))
  })

  ok_response(list(status = "reset", message = "DIABLO state cleared."))
}

# ---------------------------------------------------------------------------
# restart_diablo_job
# ---------------------------------------------------------------------------
restart_diablo_job <- function(session_id, params) {
  job_path <- get_session_path(session_id, "jobs", "diablo_job.json")

  # Cancel any running job first
  if (file.exists(job_path)) {
    existing <- fromJSON(job_path)
    if (!is.null(existing$status) && existing$status == "running") {
      cancel_diablo_job(session_id)
    }
    # Remove old job file so submit starts clean
    file.remove(job_path)
  }

  # Reset pipeline state for diablo step
  tryCatch({
    state <- read_pipeline_state(session_id)
    state$steps$diablo <- "pending"
    write_pipeline_state(session_id, state)
  }, error = function(e) {
    message("[diablo] restart: could not reset pipeline state: ", conditionMessage(e))
  })

  # Submit fresh job
  submit_diablo_job(session_id, params)
}

# ---------------------------------------------------------------------------
# run_diablo_worker - task 2.4.2
# Runs in a background callr process; sources utils to access helpers.
# job_id is passed in so the worker never needs to re-read the job file
# to preserve it - all writes are self-contained.
# ---------------------------------------------------------------------------
run_diablo_worker <- function(session_id, params, seed, job_id, wd) {
  # ---------------------------------------------------------------------------
  # BOOTSTRAP PHASE - uses base R only, NO source() calls yet.
  # Any failure here writes status="error" to the job file so the frontend
  # never gets stuck at "starting".
  # ---------------------------------------------------------------------------

  jobs_dir <- file.path(wd, "storage", session_id, "jobs")
  log_path <- file.path(jobs_dir, "diablo_worker.log")
  job_path <- file.path(jobs_dir, "diablo_job.json")

  # Minimal bootstrap logger - base R only, no dependencies
  boot_log <- function(...) {
    line <- paste0("[", format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), "] ",
                   paste(..., sep = ""))
    tryCatch({
      dir.create(jobs_dir, recursive = TRUE, showWarnings = FALSE)
      cat(line, "\n", file = log_path, append = TRUE, sep = "")
    }, error = function(e) NULL)
    message(line)
  }

  # Minimal job writer - base R only, no jsonlite yet
  boot_write_job <- function(status, stage, pct, error_code = NULL, detail = NULL) {
    tryCatch({
      now <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
      ec_field  <- if (!is.null(error_code)) paste0('"error_code":"', error_code, '",') else ""
      det_field <- if (!is.null(detail))
        paste0('"detail":"', gsub('"', '\\"', detail), '",') else ""
      json <- paste0(
        '{"status":"', status, '",',
        '"job_id":"', job_id, '",',
        '"session_id":"', session_id, '",',
        '"stage_label":"', stage, '",',
        '"progress_percent":', pct, ',',
        ec_field, det_field,
        '"worker_pid":', Sys.getpid(), ',',
        '"updated_at":"', now, '"}'
      )
      writeLines(json, job_path)
    }, error = function(e) boot_log("boot_write_job failed: ", conditionMessage(e)))
  }

  # First log line - BEFORE any source() call
  boot_log("WORKER_BOOTSTRAP_START | session=", session_id,
           " | job_id=", job_id, " | pid=", Sys.getpid(), " | wd=", wd)
  boot_write_job("starting", "Worker bootstrap starting", 1L)

  # -- Bootstrap stage 1: setwd --
  tryCatch(
    setwd(wd),
    error = function(e) {
      boot_log("BOOTSTRAP_FAILED at setwd: ", conditionMessage(e))
      boot_write_job("error", "setwd failed", 1L,
                     error_code = "WORKER_BOOTSTRAP_ERROR",
                     detail = paste0("setwd failed: ", conditionMessage(e)))
      stop(e)
    }
  )
  boot_log("BOOTSTRAP setwd OK: ", wd)

  # -- Bootstrap stage 2: source utils.R --
  tryCatch(
    source(file.path(wd, "R", "utils.R"), local = FALSE),
    error = function(e) {
      boot_log("BOOTSTRAP_FAILED at source(utils.R): ", conditionMessage(e))
      boot_write_job("error", "source utils.R failed", 2L,
                     error_code = "WORKER_BOOTSTRAP_ERROR",
                     detail = paste0("source(utils.R) failed: ", conditionMessage(e)))
      stop(e)
    }
  )
  boot_log("BOOTSTRAP source(utils.R) OK")

  # -- Bootstrap stage 3: source diablo_worker_helpers.R --
  # Sources only the plot helpers the worker needs, NOT the full diablo_module.R.
  # This avoids re-executing submit/cancel/reset logic and reduces bootstrap risk.
  tryCatch(
    source(file.path(wd, "R", "diablo_worker_helpers.R"), local = FALSE),
    error = function(e) {
      boot_log("BOOTSTRAP_FAILED at source(diablo_worker_helpers.R): ", conditionMessage(e))
      boot_write_job("error", "source diablo_worker_helpers.R failed", 3L,
                     error_code = "WORKER_BOOTSTRAP_ERROR",
                     detail = paste0("source(diablo_worker_helpers.R) failed: ",
                                     conditionMessage(e)))
      stop(e)
    }
  )
  boot_log("BOOTSTRAP source(diablo_worker_helpers.R) OK")

  # ---------------------------------------------------------------------------
  # RUNTIME PHASE - utils.R and helpers are now available.
  # ---------------------------------------------------------------------------

  log_path <- get_session_path(session_id, "jobs", "diablo_worker.log")
  dir.create(dirname(log_path), recursive = TRUE, showWarnings = FALSE)

  append_worker_log <- function(...) {
    line <- paste0("[", .now_utc(), "] ", paste(..., sep = ""))
    tryCatch(
      cat(line, "\n", file = log_path, append = TRUE, sep = ""),
      error = function(e) NULL
    )
    message(line)
  }

  job_path    <- get_session_path(session_id, "jobs", "diablo_job.json")
  results_dir <- get_session_path(session_id, "results", "diablo")
  dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

  js <- new.env(parent = emptyenv())
  js$status           <- "running"
  js$job_id           <- job_id
  js$seed_used        <- seed
  js$session_id       <- session_id
  js$started_at       <- .now_utc()
  js$progress_percent <- 5L
  js$stage_label      <- "Worker ready"
  js$worker_pid       <- Sys.getpid()

  flush_job <- function(pct, label, status = "running") {
    js$progress_percent <- as.integer(pct)
    js$stage_label      <- label
    js$status           <- status
    js$updated_at       <- .now_utc()
    tryCatch(
      write_json(as.list(js), job_path, auto_unbox = TRUE),
      error = function(e) append_worker_log("flush_job write failed: ", conditionMessage(e))
    )
    append_worker_log("[", pct, "%] ", label)
  }

  fail_job <- function(code, detail, label) {
    js$status      <- "error"
    js$error_code  <- code
    js$detail      <- detail
    js$stage_label <- label
    js$updated_at  <- .now_utc()
    tryCatch(
      write_json(as.list(js), job_path, auto_unbox = TRUE),
      error = function(e) append_worker_log("fail_job write failed: ", conditionMessage(e))
    )
    append_worker_log("FAILED [", code, "] at stage '", label, "': ", detail)
  }

  # Upgrades status from "starting" to "running"
  flush_job(5L, "Worker ready")

  tryCatch({
    append_worker_log("---- START ----")
    append_worker_log("session_id:  ", session_id)
    append_worker_log("job_id:      ", job_id)
    append_worker_log("pid:         ", Sys.getpid())
    append_worker_log("wd:          ", wd)
    append_worker_log("ncomp_max:   ", params$ncomp_max)
    append_worker_log("keepX_grid:  ", paste(params$keepX_grid, collapse = ", "))

    set.seed(seed)

    # -- Stage 1: Load comparison params --
    flush_job(10L, "Loading comparison parameters")
    params_path <- get_session_path(session_id, "params", "comparison_params.json")
    append_worker_log("comparison params: ", params_path,
                      " | exists: ", file.exists(params_path))

    if (!file.exists(params_path)) {
      fail_job("MISSING_COMPARISON_PARAMS",
               "comparison_params.json not found.",
               "Loading comparison parameters")
      return(invisible(NULL))
    }

    comp_params     <- fromJSON(params_path, simplifyVector = TRUE)
    grouping_column <- comp_params$grouping_column
    selected_groups <- as.character(comp_params$selected_groups)
    retained_ids    <- as.character(comp_params$sample_ids_retained)

    append_worker_log("grouping_column:  ", grouping_column)
    append_worker_log("selected_groups:  ", paste(selected_groups, collapse = ", "))
    append_worker_log("retained_ids (n): ", length(retained_ids))

    if (is.null(grouping_column) || length(selected_groups) < 2 || length(retained_ids) == 0) {
      fail_job("INVALID_COMPARISON_PARAMS",
               "Comparison params incomplete: need grouping_column, >=2 groups, >=1 retained ID.",
               "Loading comparison parameters")
      return(invisible(NULL))
    }

    # -- Stage 2: Load preprocessed matrices --
    flush_job(20L, "Loading preprocessed matrices")
    preprocessed_dir <- get_session_path(session_id, "preprocessed")
    matrix_files     <- list.files(preprocessed_dir, pattern = "\\.csv$", full.names = TRUE)
    matrix_files     <- matrix_files[!grepl("metadata", basename(matrix_files), ignore.case = TRUE)]

    append_worker_log("preprocessed dir:      ", preprocessed_dir)
    append_worker_log("preprocessed matrices: ", paste(basename(matrix_files), collapse = ", "))

    if (length(matrix_files) < 2 || length(matrix_files) > 4) {
      fail_job("DATASET_COUNT_INVALID",
               paste0("DIABLO requires 2-4 datasets. Found: ", length(matrix_files), "."),
               "Loading preprocessed matrices")
      return(invisible(NULL))
    }

    blocks <- lapply(matrix_files, function(f) {
      m <- read.csv(f, row.names = 1, check.names = FALSE)
      as.matrix(m)
    })
    names(blocks) <- tools::file_path_sans_ext(basename(matrix_files))
    append_worker_log("blocks loaded: ", paste(names(blocks), collapse = ", "))
    append_worker_log("block dims: ",
            paste(sapply(blocks, function(b) paste0(nrow(b), "x", ncol(b))), collapse = " | "))

    # -- Stage 3: Load metadata --
    flush_job(25L, "Loading metadata")
    raw_dir       <- get_session_path(session_id, "raw")
    all_raw_files <- list.files(raw_dir, pattern = "\\.csv$", full.names = TRUE)
    meta_files    <- all_raw_files[grepl("metadata", basename(all_raw_files), ignore.case = TRUE)]

    append_worker_log("raw dir:        ", raw_dir)
    append_worker_log("all raw CSVs:   ", paste(basename(all_raw_files), collapse = ", "))
    append_worker_log("metadata files: ", paste(basename(meta_files), collapse = ", "))

    if (length(meta_files) == 0) {
      fail_job("METADATA_NOT_FOUND",
               "No metadata file found in raw directory.",
               "Loading metadata")
      return(invisible(NULL))
    }

    meta <- read.csv(meta_files[1], stringsAsFactors = FALSE, check.names = FALSE)
    colnames(meta) <- trimws(colnames(meta))
    meta$SampleID  <- trimws(as.character(meta$SampleID))
    is_blank <- apply(meta, 1, function(r) all(is.na(r) | trimws(as.character(r)) == ""))
    meta <- meta[!is_blank, , drop = FALSE]
    rownames(meta) <- meta$SampleID
    append_worker_log("metadata rows (after blank drop): ", nrow(meta))

    # -- Stage 4: Match samples --
    flush_job(30L, "Matching selected groups and samples")
    available_in_blocks <- Reduce(intersect, lapply(blocks, rownames))
    use_ids <- intersect(retained_ids, available_in_blocks)

    append_worker_log("available in all blocks: ", length(available_in_blocks))
    append_worker_log("use_ids after intersect: ", length(use_ids))

    if (length(use_ids) < 6) {
      fail_job("INSUFFICIENT_SAMPLES",
               paste0("Only ", length(use_ids),
                      " samples remain after filtering. DIABLO requires >=6."),
               "Matching selected groups and samples")
      return(invisible(NULL))
    }

    missing_meta <- setdiff(use_ids, rownames(meta))
    if (length(missing_meta) > 0) {
      fail_job("SAMPLEID_MISMATCH",
               paste0("Sample IDs not found in metadata: ",
                      paste(head(missing_meta, 5), collapse = ", ")),
               "Matching selected groups and samples")
      return(invisible(NULL))
    }

    blocks <- lapply(blocks, function(m) m[use_ids, , drop = FALSE])
    meta   <- meta[use_ids, , drop = FALSE]

    # -- Stage 5: Build outcome vector Y --
    flush_job(35L, "Building DIABLO design and outcome vector")
    if (!grouping_column %in% colnames(meta)) {
      fail_job("MISSING_REQUIRED_COLUMN",
               paste0("Grouping column '", grouping_column, "' not found in metadata."),
               "Building DIABLO design and outcome vector")
      return(invisible(NULL))
    }

    Y <- factor(trimws(as.character(meta[[grouping_column]])), levels = selected_groups)
    present_levels <- levels(droplevels(Y))
    append_worker_log("Y levels present: ", paste(present_levels, collapse = ", "))
    append_worker_log("Y table: ",
            paste(names(table(Y)), as.integer(table(Y)), sep = "=", collapse = " | "))

    if (length(present_levels) < 2) {
      fail_job("INSUFFICIENT_GROUPS",
               paste0("Only ", length(present_levels), " group(s) remain after filtering."),
               "Building DIABLO design and outcome vector")
      return(invisible(NULL))
    }

    # -- Stage 6: Validate dimensions --
    flush_job(40L, "Validating input dimensions")
    n_blocks     <- length(blocks)
    min_features <- min(sapply(blocks, ncol))
    ncomp_max    <- as.integer(if (is.null(params$ncomp_max))  2L            else params$ncomp_max)
    keepX_grid   <- as.integer(if (is.null(params$keepX_grid)) c(5, 10, 20) else params$keepX_grid)
    keepX_grid   <- keepX_grid[keepX_grid <= min_features]
    if (length(keepX_grid) == 0) keepX_grid <- as.integer(min(5L, min_features))

    append_worker_log("n_blocks:     ", n_blocks)
    append_worker_log("min_features: ", min_features)
    append_worker_log("ncomp_max:    ", ncomp_max)
    append_worker_log("keepX_grid:   ", paste(keepX_grid, collapse = ", "))

    design <- matrix(0.1, nrow = n_blocks, ncol = n_blocks,
                     dimnames = list(names(blocks), names(blocks)))
    diag(design) <- 1

    # -- Stage 7: Tune --
    flush_job(50L, "Starting cross-validation tuning")
    append_worker_log("---- calling tune.block.splsda ----")

    tune_res <- mixOmics::tune.block.splsda(
      X           = blocks,
      Y           = Y,
      ncomp       = ncomp_max,
      test.keepX  = lapply(blocks, function(b) keepX_grid),
      design      = design,
      validation  = "Mfold",
      folds       = as.integer(if (is.null(params$cv_folds))   5L  else params$cv_folds),
      nrepeat     = as.integer(if (is.null(params$cv_repeats)) 10L else params$cv_repeats),
      dist        = "centroids.dist",
      progressBar = FALSE
    )

    ncomp_opt <- tune_res$choice.ncomp$ncomp
    keepX_opt <- tune_res$choice.keepX
    append_worker_log("tuning complete - ncomp_opt: ", ncomp_opt)
    append_worker_log("keepX_opt: ",
            paste(names(keepX_opt), unlist(keepX_opt), sep = "=", collapse = " | "))

    # -- Stage 8: Fit final model --
    flush_job(70L, "Tuning complete - fitting final model")
    append_worker_log("---- calling block.splsda ----")

    model <- mixOmics::block.splsda(
      X      = blocks,
      Y      = Y,
      ncomp  = ncomp_opt,
      keepX  = keepX_opt,
      design = design
    )
    append_worker_log("model fitted")

    # -- Stage 9: Performance --
    flush_job(80L, "Evaluating model performance")
    append_worker_log("---- calling perf ----")

    perf_res <- mixOmics::perf(
      object      = model,
      validation  = "Mfold",
      folds       = as.integer(if (is.null(params$cv_folds))   5L  else params$cv_folds),
      nrepeat     = as.integer(if (is.null(params$cv_repeats)) 10L else params$cv_repeats),
      dist        = "centroids.dist",
      progressBar = FALSE
    )
    append_worker_log("performance evaluation complete")

    # -- Stage 10: Save essential results --
    flush_job(90L, "Saving results")

    # manifest tracks every artifact: status = success / skipped / failed
    manifest <- list()

    # record_artifact - extended schema for manifest-driven frontend rendering.
    # page:         top-level results page  (hub/consensus/components/circos/network/performance/export)
    # tab_id:       tab within that page    (e.g. "consensus_plot", "comp1", "loadings_comp1")
    # artifact_type: plot / table / json
    # block:        omics block name or NA
    # component:    component number (integer) or NA
    # render_order: integer sort key within its page/tab
    record_artifact <- function(key, display_name, status,
                                page = "hub", tab_id = key,
                                artifact_type = "plot",
                                block = NA_character_, component = NA_integer_,
                                png_path = NULL, pdf_path = NULL, csv_path = NULL,
                                file_paths = NULL,
                                render_order = 99L,
                                message = NULL, warning = NULL) {
      if (is.null(png_path) && !is.null(file_paths)) {
        pngs <- file_paths[grepl("\\.png$", file_paths, ignore.case = TRUE)]
        if (length(pngs) > 0) png_path <- pngs[1]
      }
      if (is.null(pdf_path) && !is.null(file_paths)) {
        pdfs <- file_paths[grepl("\\.pdf$", file_paths, ignore.case = TRUE)]
        if (length(pdfs) > 0) pdf_path <- pdfs[1]
      }
      if (is.null(csv_path) && !is.null(file_paths)) {
        csvs <- file_paths[grepl("\\.csv$", file_paths, ignore.case = TRUE)]
        if (length(csvs) > 0) csv_path <- csvs[1]
      }

      make_url <- function(p) {
        if (is.null(p) || is.na(p) || !nzchar(p)) return(NULL)
        fname <- basename(p)
        paste0("/api/v1/", session_id, "/results/diablo/", fname)
      }

      manifest[[key]] <<- list(
        artifact_key  = key,
        display_name  = display_name,
        status        = status,
        page          = page,
        tab_id        = tab_id,
        artifact_type = artifact_type,
        block         = block %||% NA_character_,
        component     = if (is.na(component %||% NA)) NA_integer_ else as.integer(component),
        png_url       = make_url(png_path),
        pdf_url       = make_url(pdf_path),
        csv_url       = make_url(csv_path),
        file_paths    = if (is.null(file_paths)) list() else as.list(file_paths),
        render_order  = as.integer(render_order),
        message       = message %||% "",
        warning       = warning %||% ""
      )
    }

    # -- Essential: selected / ranked features --
    selected_rows <- list()
    ranked_rows   <- list()
    for (b in names(blocks)) {
      for (comp in seq_len(ncomp_opt)) {
        comp_name    <- paste0("comp", comp)
        loadings_vec <- model$loadings[[b]][, comp]
        selected     <- names(loadings_vec[loadings_vec != 0])
        for (feat in selected) {
          selected_rows[[length(selected_rows) + 1]] <- data.frame(
            block = b, component = comp_name, feature = feat,
            loading = loadings_vec[feat], stringsAsFactors = FALSE
          )
        }
        ranked_order <- order(abs(loadings_vec), decreasing = TRUE)
        for (rank_i in seq_along(ranked_order)) {
          feat <- names(loadings_vec)[ranked_order[rank_i]]
          ranked_rows[[length(ranked_rows) + 1]] <- data.frame(
            block = b, component = comp_name, rank = rank_i,
            feature = feat, loading = loadings_vec[feat],
            stringsAsFactors = FALSE
          )
        }
      }
    }

    sel_path  <- file.path(results_dir, "selected_features.csv")
    rank_path <- file.path(results_dir, "ranked_features.csv")
    tryCatch({
      selected_df <- if (length(selected_rows) > 0) do.call(rbind, selected_rows) else
        data.frame(block=character(), component=character(), feature=character(),
                   loading=numeric(), stringsAsFactors=FALSE)
      ranked_df <- if (length(ranked_rows) > 0) do.call(rbind, ranked_rows) else
        data.frame(block=character(), component=character(), rank=integer(),
                   feature=character(), loading=numeric(), stringsAsFactors=FALSE)
      write.csv(selected_df, sel_path,  row.names = FALSE)
      write.csv(ranked_df,   rank_path, row.names = FALSE)
      record_artifact("selected_features", "Selected Features Table", "success",
                      page = "export", tab_id = "selected_features",
                      artifact_type = "table",
                      csv_path = sel_path, render_order = 1L)
      record_artifact("ranked_features", "Ranked Features Table", "success",
                      page = "export", tab_id = "ranked_features",
                      artifact_type = "table",
                      csv_path = rank_path, render_order = 2L)
      append_worker_log("selected/ranked features written")
    }, error = function(e) {
      record_artifact("selected_features", "Selected Features Table", "failed",
                      page = "export", tab_id = "selected_features",
                      artifact_type = "table",
                      message = conditionMessage(e))
      append_worker_log("WARNING: selected features write failed: ", conditionMessage(e))
    })

    # -- Essential: performance summary --
    perf_path <- file.path(results_dir, "performance.json")
    tryCatch({
      err_rate_raw <- tryCatch(
        perf_res$error.rate$overall[, "centroids.dist"],
        error = function(e) {
          tryCatch(perf_res$error.rate[, "centroids.dist"],
                   error = function(e2) NULL)
        }
      )
      ber_raw <- tryCatch(
        perf_res$error.rate.class$centroids.dist,
        error = function(e) {
          tryCatch(perf_res$error.rate.class, error = function(e2) NULL)
        }
      )

      perf_list <- list(
        error_rate       = if (!is.null(err_rate_raw)) as.list(err_rate_raw) else list(),
        ber              = if (!is.null(ber_raw)) ber_raw else list(),
        confusion_matrix = tryCatch(perf_res$confusion, error = function(e) list())
      )
      write_json(perf_list, perf_path, auto_unbox = TRUE)
      append_worker_log("performance.json written")

      if (!is.null(err_rate_raw) && length(err_rate_raw) > 0) {
        err_df <- data.frame(
          component  = names(err_rate_raw),
          error_rate = as.numeric(err_rate_raw),
          error_pct  = paste0(round(as.numeric(err_rate_raw) * 100, 1), "%"),
          stringsAsFactors = FALSE
        )
        err_csv <- file.path(results_dir, "performance_error_rate.csv")
        write.csv(err_df, err_csv, row.names = FALSE)
        record_artifact("perf_error_rate", "Overall Error Rate", "success",
                        page = "performance", tab_id = "error_rate",
                        artifact_type = "table",
                        csv_path = err_csv, render_order = 1L)
        append_worker_log("performance_error_rate.csv written (", nrow(err_df), " rows)")
      } else {
        record_artifact("perf_error_rate", "Overall Error Rate", "skipped",
                        page = "performance", tab_id = "error_rate",
                        artifact_type = "table",
                        message = "No error rate data available")
      }

      ber_rows <- list()
      if (!is.null(ber_raw)) {
        if (is.matrix(ber_raw)) {
          for (comp_i in seq_len(nrow(ber_raw))) {
            for (cls in colnames(ber_raw)) {
              ber_rows[[length(ber_rows) + 1]] <- data.frame(
                component = rownames(ber_raw)[comp_i],
                class     = cls,
                ber       = round(ber_raw[comp_i, cls], 4),
                stringsAsFactors = FALSE
              )
            }
          }
        } else if (is.list(ber_raw)) {
          for (comp_nm in names(ber_raw)) {
            cls_vals <- ber_raw[[comp_nm]]
            if (is.numeric(cls_vals)) {
              for (cls in names(cls_vals)) {
                ber_rows[[length(ber_rows) + 1]] <- data.frame(
                  component = comp_nm, class = cls,
                  ber = round(cls_vals[[cls]], 4),
                  stringsAsFactors = FALSE
                )
              }
            }
          }
        }
      }
      if (length(ber_rows) > 0) {
        ber_df  <- do.call(rbind, ber_rows)
        ber_csv <- file.path(results_dir, "performance_ber.csv")
        write.csv(ber_df, ber_csv, row.names = FALSE)
        record_artifact("perf_ber", "Balanced Error Rate (BER)", "success",
                        page = "performance", tab_id = "ber",
                        artifact_type = "table",
                        csv_path = ber_csv, render_order = 2L)
        append_worker_log("performance_ber.csv written (", nrow(ber_df), " rows)")
      } else {
        record_artifact("perf_ber", "Balanced Error Rate (BER)", "skipped",
                        page = "performance", tab_id = "ber",
                        artifact_type = "table",
                        message = "No BER data available")
      }

      append_worker_log("performance artifacts written")
    }, error = function(e) {
      record_artifact("perf_error_rate", "Overall Error Rate", "failed",
                      page = "performance", tab_id = "error_rate",
                      artifact_type = "table",
                      message = conditionMessage(e))
      record_artifact("perf_ber", "Balanced Error Rate (BER)", "failed",
                      page = "performance", tab_id = "ber",
                      artifact_type = "table",
                      message = conditionMessage(e))
      append_worker_log("WARNING: performance write failed: ", conditionMessage(e))
    })

    append_worker_log("essential results written to: ", results_dir)

    # -- Stage 11: Plots (optional - failures do not kill the job) --
    flush_job(93L, "Generating plots")

    required_plot_fns <- c(
      "compute_diablo_edges",
      "plot_diablo_consensus", "plot_diablo_consensus_loadings",
      "plot_diablo_consensus_vip",
      "plot_diablo_blocks", "plot_diablo_loadings",
      "plot_diablo_network", "plot_diablo_circos", ".save_plot"
    )
    missing_fns <- required_plot_fns[
      !sapply(required_plot_fns, exists, mode = "function", inherits = TRUE)
    ]
    if (length(missing_fns) > 0) {
      fail_job("MISSING_PLOT_HELPERS",
               paste0("Plot helper functions not found in worker environment: ",
                      paste(missing_fns, collapse = ", ")),
               "Generating plots")
      return(invisible(NULL))
    }
    append_worker_log("plot helpers verified: ", paste(required_plot_fns, collapse = ", "))

    # -- Multiblock 2D scores plot (weighted.average / average block) --
    tryCatch({
      paths <- plot_diablo_consensus(model, results_dir, append_worker_log)
      record_artifact("plot_consensus", "Multi-Block 2D Scores Plot", "success",
                      page = "consensus", tab_id = "multiblock_scores",
                      artifact_type = "plot",
                      file_paths = if (is.list(paths)) c(paths$png_path, paths$pdf_path) else paths,
                      render_order = 1L)
    }, error = function(e) {
      record_artifact("plot_consensus", "Multi-Block 2D Scores Plot", "failed",
                      page = "consensus", tab_id = "multiblock_scores",
                      artifact_type = "plot",
                      message = conditionMessage(e))
      append_worker_log("WARNING: multiblock scores plot failed: ", conditionMessage(e))
    })

    # -- Sample scores: no distinct artifact beyond multiblock 2D scores --
    # In block.splsda, model$variates$WeightedAverage IS the multiblock sample
    # scores. There is no mathematically distinct separate view. The "Sample
    # Scores" tab has been removed from the frontend for this reason.

    # -- Consensus loadings: use Y-block loadings from block.splsda --
    # model$loadings$Y contains the outcome-block loadings which represent the
    # multiblock consensus direction. This is a valid multiblock loadings view.
    tryCatch({
      cl_result <- plot_diablo_consensus_loadings(model, results_dir, append_worker_log)
      if (!is.null(cl_result$png_path)) {
        record_artifact("plot_consensus_loadings", "Loadings Plot - Multi-Block sPLS-DA", "success",
                        page = "consensus", tab_id = "consensus_loadings",
                        artifact_type = "plot",
                        png_path = cl_result$png_path,
                        pdf_path = cl_result$pdf_path,
                        render_order = 3L)
      }
      if (!is.null(cl_result$csv_path)) {
        record_artifact("table_consensus_loadings", "Consensus Loadings Table", "success",
                        page = "consensus", tab_id = "consensus_loadings_table",
                        artifact_type = "table",
                        csv_path = cl_result$csv_path,
                        render_order = 4L)
      }
    }, error = function(e) {
      record_artifact("plot_consensus_loadings", "Loadings Plot - Multi-Block sPLS-DA", "skipped",
                      page = "consensus", tab_id = "consensus_loadings",
                      artifact_type = "plot",
                      message = conditionMessage(e))
      append_worker_log("WARNING: consensus loadings failed: ", conditionMessage(e))
    })

    # -- Consensus VIP --
    tryCatch({
      vip_result <- plot_diablo_consensus_vip(model, results_dir, append_worker_log)
      append_worker_log("consensus VIP result: png=", !is.null(vip_result$png_path),
                        " csv=", !is.null(vip_result$csv_path))
      if (!is.null(vip_result$png_path)) {
        record_artifact("plot_consensus_vip", "Consensus VIP Plot", "success",
                        page = "consensus", tab_id = "consensus_vip",
                        artifact_type = "plot",
                        png_path = vip_result$png_path,
                        pdf_path = vip_result$pdf_path,
                        render_order = 5L)
      } else {
        record_artifact("plot_consensus_vip", "Consensus VIP Plot", "failed",
                        page = "consensus", tab_id = "consensus_vip",
                        artifact_type = "plot",
                        message = "VIP plot not generated — no selected features found or plot rendering failed. Check worker log for details.")
        append_worker_log("WARNING: consensus VIP plot was not generated — check log above for block-level warnings")
      }
      if (!is.null(vip_result$csv_path)) {
        record_artifact("table_consensus_vip", "Consensus VIP Table", "success",
                        page = "consensus", tab_id = "consensus_vip_table",
                        artifact_type = "table",
                        csv_path = vip_result$csv_path,
                        render_order = 6L)
      } else {
        record_artifact("table_consensus_vip", "Consensus VIP Table", "failed",
                        page = "consensus", tab_id = "consensus_vip_table",
                        artifact_type = "table",
                        message = "VIP table not generated — no selected features found. Check worker log for details.")
      }
    }, error = function(e) {
      record_artifact("plot_consensus_vip", "Consensus VIP Plot", "failed",
                      page = "consensus", tab_id = "consensus_vip",
                      artifact_type = "plot",
                      message = conditionMessage(e))
      record_artifact("table_consensus_vip", "Consensus VIP Table", "failed",
                      page = "consensus", tab_id = "consensus_vip_table",
                      artifact_type = "table",
                      message = conditionMessage(e))
      append_worker_log("WARNING: consensus VIP failed with error: ", conditionMessage(e))
    })

    tryCatch({
      paths_list <- plot_diablo_blocks(model, results_dir, append_worker_log)
      for (b in names(model$X)) {
        safe_b <- gsub("[^A-Za-z0-9_]", "_", b)
        # Only comp 1 is the user-facing block-specific 2D scores tab.
        # Higher components are stored internally but not surfaced as separate tabs.
        bpaths <- paths_list[[paste0(b, "_comp1")]]
        if (!is.null(bpaths) && length(bpaths) > 0) {
          record_artifact(paste0("plot_block_", safe_b, "_scores"),
                          paste0("2D Scores Plot - ", b), "success",
                          page = "components", tab_id = paste0("block_", safe_b, "_scores"),
                          artifact_type = "plot", block = b, component = 1L,
                          file_paths = bpaths, render_order = 10L)
        }
      }
    }, error = function(e) {
      record_artifact("plot_blocks", "Per-Block Score Plots", "failed",
                      page = "components", tab_id = "block_scores",
                      artifact_type = "plot",
                      message = conditionMessage(e))
      append_worker_log("WARNING: block plots failed: ", conditionMessage(e))
    })

    tryCatch({
      loadings_results <- plot_diablo_loadings(model, results_dir, append_worker_log)
      for (b in names(model$X)) {
        safe_b  <- gsub("[^A-Za-z0-9_]", "_", b)
        tab_id  <- paste0("block_", safe_b, "_loadings")
        # Use comp 1 as the user-facing block-specific loadings tab.
        lpaths  <- loadings_results[[paste0(b, "_comp1")]]
        csv_p   <- loadings_results[[paste0(b, "_comp1_csv")]]
        if (!is.null(lpaths) && length(lpaths) > 0) {
          record_artifact(paste0("plot_loadings_", safe_b),
                          paste0("Loadings Plot - ", b), "success",
                          page = "components", tab_id = tab_id,
                          artifact_type = "plot", block = b, component = 1L,
                          file_paths = lpaths, render_order = 20L)
        }
        if (!is.null(csv_p) && nzchar(csv_p)) {
          record_artifact(paste0("table_loadings_", safe_b),
                          paste0("Loadings Table - ", b), "success",
                          page = "components", tab_id = tab_id,
                          artifact_type = "table", block = b, component = 1L,
                          csv_path = csv_p, render_order = 21L)
        }
      }
    }, error = function(e) {
      record_artifact("plot_loadings", "Loadings Plots", "failed",
                      page = "components", tab_id = "block_loadings",
                      artifact_type = "plot",
                      message = conditionMessage(e))
      append_worker_log("WARNING: loadings plots failed: ", conditionMessage(e))
    })

    tryCatch({
      net_result <- plot_diablo_network(model, results_dir, append_worker_log)
      if (!is.null(net_result$png_path)) {
        record_artifact("plot_network", "Network Plot", "success",
                        page = "network", tab_id = "network_plot",
                        artifact_type = "plot",
                        png_path = net_result$png_path,
                        pdf_path = net_result$pdf_path,
                        render_order = 1L)
      }
      if (!is.null(net_result$node_csv)) {
        record_artifact("table_network_nodes", "Network Node Metrics", "success",
                        page = "network", tab_id = "node_metrics",
                        artifact_type = "table",
                        csv_path = net_result$node_csv, render_order = 2L)
      }
      if (!is.null(net_result$edge_csv)) {
        record_artifact("table_network_edges", "Network Edge Table", "success",
                        page = "network", tab_id = "edge_table",
                        artifact_type = "table",
                        csv_path = net_result$edge_csv, render_order = 3L)
      }
    }, error = function(e) {
      record_artifact("plot_network", "Network Plot", "failed",
                      page = "network", tab_id = "network_plot",
                      artifact_type = "plot",
                      message = conditionMessage(e))
      append_worker_log("WARNING: network plot failed: ", conditionMessage(e))
    })

    tryCatch({
      circ_result <- plot_diablo_circos(model, results_dir, append_worker_log)
      if (!is.null(circ_result$png_path)) {
        record_artifact("plot_circos", "Circos Plot", "success",
                        page = "circos", tab_id = "circos_plot",
                        artifact_type = "plot",
                        png_path = circ_result$png_path,
                        pdf_path = circ_result$pdf_path,
                        render_order = 1L)
      }
      if (!is.null(circ_result$edge_csv)) {
        record_artifact("table_circos_edges", "Circos Edge Table", "success",
                        page = "circos", tab_id = "circos_table",
                        artifact_type = "table",
                        csv_path = circ_result$edge_csv, render_order = 2L)
      }
    }, error = function(e) {
      record_artifact("plot_circos", "Circos Plot", "failed",
                      page = "circos", tab_id = "circos_plot",
                      artifact_type = "plot",
                      message = conditionMessage(e))
      append_worker_log("WARNING: circos plot failed: ", conditionMessage(e))
    })

    append_worker_log("plot generation complete")

    # -- Essential: results manifest --
    manifest_path <- file.path(results_dir, "results_manifest.json")
    tryCatch(
      write_json(manifest, manifest_path, auto_unbox = TRUE, pretty = TRUE),
      error = function(e) append_worker_log("WARNING: manifest write failed: ", conditionMessage(e))
    )
    append_worker_log("results manifest written")

    # -- Essential: diablo_results_summary.json --
    artifact_warnings <- Filter(function(a) nchar(a$warning) > 0, manifest)
    warning_msgs <- sapply(artifact_warnings, function(a)
      paste0(a$artifact_key, ": ", a$warning))

    png_files <- list.files(results_dir, pattern = "\\.png$", full.names = FALSE)
    plot_urls <- setNames(
      lapply(png_files, function(f)
        paste0("/api/v1/", session_id, "/plots/diablo/", f)),
      tools::file_path_sans_ext(png_files)
    )

    feature_counts <- lapply(names(blocks), function(b) {
      if (!is.null(model$loadings[[b]])) ncol(model$loadings[[b]]) else 0L
    })
    names(feature_counts) <- names(blocks)

    comp_params_for_summary <- tryCatch(
      fromJSON(params_path, simplifyVector = TRUE),
      error = function(e) list()
    )

    summary_obj <- list(
      session_id       = session_id,
      status           = "complete",
      selected_groups  = as.list(selected_groups),
      ncomp_used       = ncomp_opt,
      keepX_grid_used  = as.list(keepX_grid),
      keepX_chosen     = keepX_opt,
      block_names      = as.list(names(blocks)),
      feature_counts   = feature_counts,
      performance      = if (file.exists(perf_path))
                           tryCatch(fromJSON(perf_path, simplifyVector = FALSE),
                                    error = function(e) NULL)
                         else NULL,
      plots            = plot_urls,
      tables = list(
        selected_features = paste0("/api/v1/", session_id,
                                   "/results/diablo/selected_features.csv"),
        ranked_features   = paste0("/api/v1/", session_id,
                                   "/results/diablo/ranked_features.csv")
      ),
      warnings         = as.list(warning_msgs),
      completed_at     = .now_utc()
    )

    summary_path <- file.path(results_dir, "diablo_results_summary.json")
    tryCatch(
      write_json(summary_obj, summary_path, auto_unbox = TRUE, pretty = TRUE),
      error = function(e) append_worker_log("WARNING: summary write failed: ", conditionMessage(e))
    )
    append_worker_log("diablo_results_summary.json written")

    # -- Complete --
    js$completed_at <- .now_utc()
    flush_job(100L, "Complete", status = "complete")

    pipeline_state <- read_pipeline_state(session_id)
    pipeline_state$steps$diablo <- "complete"
    write_pipeline_state(session_id, pipeline_state)

    append_worker_log("---- COMPLETE ----")

  }, error = function(e) {
    msg <- conditionMessage(e)
    append_worker_log("---- UNCAUGHT ERROR ----")
    append_worker_log(msg)
    fail_job("ANALYSIS_RUNTIME_ERROR", msg, js$stage_label)
  })

  invisible(NULL)
}


# ---------------------------------------------------------------------------
# DIABLO plot / result helper functions - task 2.4.3
#
# Design rules:
#   - All helpers accept an optional log_fn argument for worker logging.
#   - plot helpers return a named list with png_path, pdf_path, and optional
#     csv paths for associated tables.
#   - All helpers validate their inputs before calling mixOmics.
#   - Callers wrap each helper in tryCatch - failures are recorded in the
#     manifest but do NOT crash the worker.
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

# Choose the best available "consensus-style" block target for plotIndiv.
# VERIFIED: mixOmics 6.26.0 block.splsda does NOT store "weighted.average" in
# model$variates. plotIndiv computes it internally as a virtual block name.
# This helper is kept for reference but plot_diablo_consensus calls plotIndiv
# with blocks="weighted.average" directly.
.choose_score_block <- function(model) {
  # "weighted.average" and "average" are virtual names accepted by plotIndiv
  # — they are computed internally, not stored in model$variates.
  "weighted.average"
}

# ---------------------------------------------------------------------------
# compute_diablo_edges - shared edge computation for network AND circos
# Returns data.frame with columns:
#   feature_1, block_1, feature_2, block_2, component,
#   correlation_r, abs_correlation, p_value, within_block
# Includes BOTH within-block and between-block pairs for all components.
# ---------------------------------------------------------------------------
# compute_diablo_edges -- shared edge computation for network AND circos.
#
# DESIGN NOTE on cutoffs:
#   - Network plot uses cutoff = 0 (all edges) to show the full correlation
#     structure. The network visualization handles density via node/edge
#     aesthetics (size, width, colour).
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
# ── Weighted-average score computation ───────────────────────────────────────
# model$weights is a DATA FRAME — rownames have spaces converted to dots.
# names(model$X) preserves original names with spaces.
# ROOT CAUSE: model$weights["DIABLO Serum Lipids", 1] returns NA because
# the rowname is "DIABLO.Serum.Lipids". Fix: use positional indexing [i, comp].
#
# Formula: wa[sample,comp] = sum_b(w_b * variates[[b]][sample,comp]) / sum_b(w_b)
# over X blocks only. Works for any ncomp >= 1.
.compute_weighted_average_scores <- function(model) {
  block_names <- names(model$X)
  ncomp       <- model$ncomp[1]

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
    w_total   <- 0.0
    contrib   <- numeric(n_samples)
    n_contrib <- 0L
    for (i in seq_along(block_names)) {
      b     <- block_names[i]
      w     <- tryCatch(as.numeric(model$weights[i, comp]), error = function(e) NA_real_)
      if (!is.finite(w) || w < 0) next
      v     <- tryCatch(model$variates[[b]], error = function(e) NULL)
      if (is.null(v)) next
      if (!is.matrix(v)) v <- as.matrix(v)
      if (ncol(v) < comp) next
      score <- as.numeric(v[, comp, drop = FALSE])
      if (length(score) != n_samples || !all(is.finite(score))) next
      contrib   <- contrib + w * score
      w_total   <- w_total + w
      n_contrib <- n_contrib + 1L
    }
    if (n_contrib == 0L) stop("No valid X-block contributions for component ", comp)
    if (is.finite(w_total) && w_total > 0) wa[, comp] <- contrib / w_total
    else wa[, comp] <- contrib
  }
  wa
}

# ── Consensus plot (manually computed weighted-average scores) ────────────────
plot_diablo_consensus <- function(model, out_dir, log_fn = message) {
  log_fn("multiblock scores plot: computing weighted-average coordinates manually")
  block_names  <- names(model$X)
  ncomp        <- model$ncomp[1]
  Y            <- model$Y
  groups       <- as.character(Y)
  group_levels <- levels(Y)

  wa <- tryCatch(.compute_weighted_average_scores(model),
                 error = function(e) stop("WA scores failed: ", conditionMessage(e)))

  x_coords <- wa[, 1]
  y_coords <- if (ncomp >= 2) wa[, 2] else rep(0.0, nrow(wa))

  .wa_expl_var <- function(comp_idx) {
    tryCatch({
      vals  <- sapply(block_names, function(b) { ev <- model$prop_expl_var[[b]]; if (length(ev) >= comp_idx) ev[comp_idx] else NA_real_ })
      w_col <- model$weights[block_names, min(comp_idx, ncol(model$weights))]
      round(weighted.mean(vals, w_col, na.rm = TRUE) * 100, 1)
    }, error = function(e) NA_real_)
  }
  ev1  <- .wa_expl_var(1)
  ev2  <- if (ncomp >= 2) .wa_expl_var(2) else NA_real_
  xlab <- if (!is.na(ev1)) paste0("Component 1 (", ev1, "% expl. var)") else "Component 1"
  ylab <- if (ncomp >= 2 && !is.na(ev2)) paste0("Component 2 (", ev2, "% expl. var)") else "Component 2"

  palette_cols  <- c("#388ECC","#F68B33","#C2C2C2","#009E73","#CC79A7","#56B4E9","#E69F00","#D55E00","#0072B2","#F0E442")
  group_col_map <- setNames(palette_cols[seq_along(group_levels) %% length(palette_cols) + 1], group_levels)
  point_cols    <- group_col_map[groups]

  .render_wa_plot <- function(with_ellipse) {
    op <- par(mar = c(5, 5, 4, 8), xpd = FALSE); on.exit(par(op), add = TRUE)
    plot(x_coords, y_coords, pch = 16, cex = 2.0, col = point_cols,
         xlab = xlab, ylab = ylab, main = "2D Scores Plot - Multi-Block sPLS-DA", bty = "l", las = 1)
    abline(h = 0, v = 0, col = "grey80", lty = 2, lwd = 0.8)
    if (with_ellipse) {
      for (grp in group_levels) {
        idx <- which(groups == grp)
        if (length(idx) >= 3) {
          tryCatch({
            ep <- mixOmics:::.ellipse(cov(cbind(x_coords[idx], y_coords[idx])),
                                      centre = c(mean(x_coords[idx]), mean(y_coords[idx])), level = 0.95)
            lines(ep, col = group_col_map[grp], lwd = 1.5)
          }, error = function(e) NULL)
        }
      }
    }
    par(xpd = TRUE)
    legend("topright", inset = c(-0.18, 0), legend = group_levels,
           col = group_col_map[group_levels], pch = 16, pt.cex = 1.4, cex = 0.85, bty = "n", title = "Group")
  }

  result <- tryCatch(
    .save_plot(out_dir, "multiblock_scores", .render_wa_plot(TRUE),
               width_px = 1400, height_px = 1000, res = 150, width_in = 9, height_in = 7),
    error = function(e) {
      log_fn("with ellipse failed (", conditionMessage(e), "), retrying without")
      .save_plot(out_dir, "multiblock_scores", .render_wa_plot(FALSE),
                 width_px = 1400, height_px = 1000, res = 150, width_in = 9, height_in = 7)
    }
  )
  log_fn("saved multiblock_scores plot (ncomp=", ncomp, ")")

  # Save score coordinates CSV for plot regeneration
  tryCatch({
    coords_df <- data.frame(
      sample_id = names(x_coords),
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

# ── Sample scores plot (kept for backward compat, not called by worker) ───────
# In block.splsda, model$variates$WeightedAverage IS the multiblock sample
# scores — there is no mathematically distinct separate view.
plot_diablo_sample_scores <- function(model, out_dir, log_fn = message) {
  log_fn("plot_diablo_sample_scores: no distinct sample-scores artifact for block.splsda; skipped.")
  invisible(list())
}

# ── Consensus loadings (weighted block contribution per feature, comp 1) ──────
# DIAGNOSIS: plotLoadings(model, comp=1) only concatenates per-block comp-1
# loadings side by side — it is NOT a true consensus metric.
#
# CORRECT FORMULA:
#   consensus_contribution[feature] = w_b * loading_b[feature, comp1]
#   where w_b = model$weights[b, 1] (block weight for comp 1)
#
# This gives each selected feature a single consensus contribution score
# reflecting its contribution to the multiblock consensus direction,
# weighted by how much that block contributes to the overall model.
# Sign is preserved from the original loading.
plot_diablo_consensus_loadings <- function(model, out_dir, log_fn = message) {
  result      <- list()
  block_names <- names(model$X)

  rows <- list()
  for (i in seq_along(block_names)) {
    b   <- block_names[i]
    lv  <- tryCatch(model$loadings[[b]][, 1], error = function(e) NULL)
    if (is.null(lv)) next
    # Positional weight extraction — model$weights rownames use dots, names(model$X) use spaces
    w_b <- tryCatch(as.numeric(model$weights[i, 1]), error = function(e) NA_real_)
    if (!is.finite(w_b) || w_b <= 0) {
      log_fn("WARNING: block '", b, "' weight is NA/non-positive for comp 1; skipping")
      next
    }
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
    log_fn("consensus loadings: no selected features for comp 1")
    return(invisible(result))
  }

  tbl      <- do.call(rbind, rows)
  tbl      <- tbl[order(tbl$abs_consensus, decreasing = TRUE), ]
  tbl$rank <- seq_len(nrow(tbl))

  tryCatch({
    csv_path <- file.path(out_dir, "consensus_loadings.csv")
    write.csv(tbl, csv_path, row.names = FALSE)
    result$csv_path <- csv_path
    log_fn("saved consensus_loadings CSV (", nrow(tbl), " selected features)")
  }, error = function(e) log_fn("WARNING: consensus loadings CSV failed: ", conditionMessage(e)))

  n_show         <- min(nrow(tbl), 40L)
  tbl_plot       <- tbl[rev(seq_len(n_show)), ]
  bar_cols       <- ifelse(tbl_plot$sign == "+", "#388ECC", "#F68B33")
  plot_height_px <- max(700L, 28L * n_show)
  plot_height_in <- max(5.0, 0.22 * n_show)

  plot_result <- tryCatch({
    .save_plot(out_dir, "consensus_loadings",
      {
        op <- par(mar = c(4, max(8, max(nchar(tbl_plot$feature)) * 0.55), 3, 2))
        on.exit(par(op), add = TRUE)
        barplot(tbl_plot$consensus_contribution, horiz = TRUE, col = bar_cols, border = NA,
                names.arg = paste0(tbl_plot$feature, " [", tbl_plot$block, "]"),
                las = 1, cex.names = 0.72,
                xlab = "Consensus Contribution (block weight × loading, comp 1)",
                main = "Loadings Plot - Multi-Block sPLS-DA")
        abline(v = 0, col = "grey40", lwd = 1)
        legend("bottomright", legend = c("Positive","Negative"),
               fill = c("#388ECC","#F68B33"), border = NA, bty = "n", cex = 0.8)
      },
      width_px = 1600, height_px = plot_height_px,
      width_in = 10,   height_in = plot_height_in)
  }, error = function(e) { log_fn("consensus loadings plot failed: ", conditionMessage(e)); NULL })

  if (!is.null(plot_result)) {
    result$png_path <- plot_result$png_path
    result$pdf_path <- plot_result$pdf_path
    log_fn("saved consensus_loadings plot (", n_show, " features)")
  }
  invisible(result)
}

# â”€â”€ Per-block Ã— per-component sample plots â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
# Returns named list keyed by "<block>_comp<n>" â†’ c(png_path, pdf_path)
plot_diablo_blocks <- function(model, out_dir, log_fn = message) {
  block_names <- names(model$X)
  ncomp       <- model$ncomp[1]
  all_results <- list()

  for (b in block_names) {
    safe_b <- gsub("[^A-Za-z0-9_]", "_", b)
    for (comp in seq_len(ncomp)) {
      key      <- paste0(b, "_comp", comp)
      comp_arg <- if (ncomp >= 2) c(comp, min(comp + 1L, ncomp)) else comp
      result <- tryCatch({
        r <- tryCatch({
          .save_plot(out_dir, paste0("block_", safe_b, "_comp", comp),
            mixOmics::plotIndiv(model, blocks = b, comp = comp_arg,
                                ind.names = FALSE, pch = 16, cex = 2.0,
                                legend = TRUE, ellipse = TRUE,
                                title = paste0("2D Scores Plot - ", b)))
        }, error = function(e) {
          log_fn("block '", b, "' comp", comp, " with ellipse failed, retrying without")
          .save_plot(out_dir, paste0("block_", safe_b, "_comp", comp),
            mixOmics::plotIndiv(model, blocks = b, comp = comp_arg,
                                ind.names = FALSE, pch = 16, cex = 2.0,
                                legend = TRUE, ellipse = FALSE,
                                title = paste0("2D Scores Plot - ", b)))
        })
        log_fn("saved block score plot for '", b, "' comp", comp)
        r
      }, error = function(e) {
        log_fn("WARNING: block plot failed for '", b, "' comp", comp, ": ", conditionMessage(e))
        NULL
      })
      if (!is.null(result)) {
        all_results[[key]] <- c(result$png_path, result$pdf_path)

        # Save score coordinates CSV for regeneration (comp 1 only)
        if (comp == 1) {
          tryCatch({
            v <- model$variates[[b]]
            if (!is.matrix(v)) v <- as.matrix(v)
            x_c <- as.numeric(v[, 1, drop = TRUE])
            y_c <- if (ncol(v) >= 2) as.numeric(v[, 2, drop = TRUE]) else rep(0.0, nrow(v))
            coords_df <- data.frame(
              sample_id = rownames(v),
              group     = as.character(model$Y),
              x         = x_c,
              y         = y_c,
              xlab      = "Component 1",
              ylab      = if (ncol(v) >= 2) "Component 2" else "Component 1",
              stringsAsFactors = FALSE
            )
            write.csv(coords_df,
                      file.path(out_dir, paste0("block_", safe_b, "_scores_coords.csv")),
                      row.names = FALSE)
          }, error = function(e)
            log_fn("WARNING: block coords CSV failed for '", b, "': ", conditionMessage(e)))
        }
      }
    }
  }
  invisible(all_results)
}

# â”€â”€ Loadings plots + CSV tables â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
# Returns named list keyed by "<block>_comp<n>" â†’ c(png, pdf)
# and "<block>_comp<n>_csv" â†’ csv_path
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
                                 title = paste0("Loadings Plot - ", b)),
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

# -- Network plot + node/edge tables ------------------------------------------
# Custom igraph-based renderer. Uses compute_diablo_edges(cutoff=0) -- all
# edges, full network structure.
# (Circos uses a higher cutoff; see compute_diablo_edges for design rationale.)
#
# Visual encoding:
#   - Node size    : proportional to degree (number of edges)
#   - Node colour  : one colour per omics block
#   - Edge width   : proportional to abs(correlation)
#   - Edge colour  : red = positive correlation, blue = negative correlation
#   - Labels       : small (cex 0.55), do not inflate node size
#   - Layout       : Fruchterman-Reingold weighted by abs_correlation
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
# â”€â”€ Circos plot + edge table â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
# Uses compute_diablo_edges with a high cutoff (0.7, fallback 0.3) to show only
# strong correlations. Circos becomes unreadable with too many edges, so a higher
# cutoff is intentional. The network plot uses cutoff=0 (all edges) by design.
# See compute_diablo_edges header for full design rationale.
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
