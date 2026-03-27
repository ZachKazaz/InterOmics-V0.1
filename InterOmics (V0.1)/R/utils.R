# utils.R
# Session management, path helpers, state I/O, response helpers

library(jsonlite)
library(uuid)

STORAGE_ROOT <- "storage"

# ---------------------------------------------------------------------------
# Session management
# ---------------------------------------------------------------------------

create_session <- function() {
  session_id <- UUIDgenerate()
  base       <- file.path(STORAGE_ROOT, session_id)

  dirs <- c(
    base,
    file.path(base, "raw"),
    file.path(base, "preprocessed"),
    file.path(base, "params"),
    file.path(base, "jobs"),
    file.path(base, "results", "diablo"),
    file.path(base, "results", "correlation"),
    file.path(base, "results", "correlation", "cross"),
    file.path(base, "results", "enrichment"),
    file.path(base, "results", "stats")
  )
  for (d in dirs) dir.create(d, recursive = TRUE, showWarnings = FALSE)

  initial_state <- list(
    session_id = session_id,
    steps = list(
      upload      = "pending",
      validate    = "pending",
      preprocess  = "pending",
      map_ids     = "pending",
      diablo      = "pending",
      correlation = "pending",
      enrichment  = "pending",
      stats       = "pending"
    )
  )
  state_path <- file.path(base, "pipeline_state.json")
  write_json(initial_state, state_path, auto_unbox = TRUE, pretty = TRUE)

  session_id
}

get_session_path <- function(session_id, ...) {
  file.path(STORAGE_ROOT, session_id, ...)
}

read_pipeline_state <- function(session_id) {
  path <- get_session_path(session_id, "pipeline_state.json")
  if (!file.exists(path)) stop("pipeline_state.json not found for session: ", session_id)
  fromJSON(path, simplifyVector = FALSE)
}

write_pipeline_state <- function(session_id, state) {
  path <- get_session_path(session_id, "pipeline_state.json")
  write_json(state, path, auto_unbox = TRUE, pretty = TRUE)
}

check_prerequisite <- function(session_id, required_step) {
  state <- read_pipeline_state(session_id)
  isTRUE(state$steps[[required_step]] == "complete")
}

# ---------------------------------------------------------------------------
# Session TTL cleanup
# ---------------------------------------------------------------------------

cleanup_expired_sessions <- function(storage_root = STORAGE_ROOT, ttl_hours = 24) {
  if (!dir.exists(storage_root)) return(invisible(NULL))
  sessions <- list.dirs(storage_root, recursive = FALSE, full.names = TRUE)
  cutoff   <- Sys.time() - ttl_hours * 3600

  for (s in sessions) {
    state_file <- file.path(s, "pipeline_state.json")
    mtime      <- if (file.exists(state_file)) file.mtime(state_file) else file.mtime(s)
    if (mtime < cutoff) {
      unlink(s, recursive = TRUE)
    }
  }
  invisible(NULL)
}

# ---------------------------------------------------------------------------
# Utility operators
# ---------------------------------------------------------------------------

`%||%` <- function(a, b) if (!is.null(a)) a else b

# ---------------------------------------------------------------------------
# Timestamp helper — ISO-8601 UTC
# Available to both main process and callr worker subprocesses (via source)
# ---------------------------------------------------------------------------
.now_utc <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

# ---------------------------------------------------------------------------
# Response helpers
# ---------------------------------------------------------------------------

error_response <- function(error_code, detail, module, recoverable = TRUE) {
  list(
    status      = "error",
    error_code  = error_code,
    detail      = detail,
    module      = module,
    recoverable = recoverable
  )
}

ok_response <- function(data = list()) {
  if (!"status" %in% names(data)) data$status <- "ok"
  data
}
