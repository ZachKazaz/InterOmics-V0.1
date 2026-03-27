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

