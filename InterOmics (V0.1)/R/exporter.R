# exporter.R — Package analysis results into a ZIP archive
# Task: 2.8.1
# Entry point: run_export(session_id, modules, formats)

library(jsonlite)
library(zip)

source("R/utils.R")

# ---------------------------------------------------------------------------
# Expected output files per module
# ---------------------------------------------------------------------------
.expected_files <- function(module, formats) {
  csv_files <- switch(module,
    diablo = c(
      "results/diablo/selected_features.csv",
      "results/diablo/ranked_features.csv",
      "results/diablo/performance.json"
    ),
    correlation = c(
      "results/correlation/correlation_matrix.csv",
      "results/correlation/edge_list.csv",
      "results/correlation/node_metrics.csv",
      "results/correlation/cross/edge_list.csv",
      "results/correlation/cross/node_metrics.csv"
    ),
    enrichment = c(
      "results/enrichment/ora_results.csv",
      "results/enrichment/topology_results.csv",
      "results/enrichment/lipid_enrichment_results.csv"
    ),
    stats = c(
      "results/stats/feature_results.csv",
      "results/stats/main_effects_summary.csv",
      "results/stats/interaction_effects_summary.csv"
    ),
    character(0)
  )

  # Plot files (PNG and/or PDF) — discovered dynamically from results dir
  # (handled separately in run_export)
  if ("csv" %in% formats) csv_files else character(0)
}

# ---------------------------------------------------------------------------
# run_export — task 2.8.1
# ---------------------------------------------------------------------------
#
# modules  — character vector, e.g. c("diablo", "correlation", "enrichment", "stats")
# formats  — character vector, e.g. c("csv", "png", "pdf")
#
# Returns ok_response with:
#   zip_path   — absolute path to the created ZIP
#   omissions  — list of { path, reason } for missing files
#
run_export <- function(session_id, modules, formats) {
  session_dir <- get_session_path(session_id)
  timestamp   <- format(Sys.time(), "%Y%m%d_%H%M%S")
  zip_path    <- file.path(session_dir, paste0("export_", timestamp, ".zip"))

  included  <- character(0)
  omissions <- list()

  # Collect files to include
  files_to_zip <- character(0)

  for (mod in modules) {
    # CSV / JSON files
    if ("csv" %in% formats) {
      expected <- .expected_files(mod, formats)
      for (rel_path in expected) {
        abs_path <- file.path(session_dir, rel_path)
        if (file.exists(abs_path)) {
          files_to_zip <- c(files_to_zip, abs_path)
          included     <- c(included, rel_path)
        } else {
          omissions[[length(omissions) + 1]] <- list(
            path   = rel_path,
            reason = paste0("Module '", mod, "' output not found. ",
                            "Analysis may not have been run.")
          )
        }
      }
    }

    # Plot files (PNG and/or PDF) — scan results directory
    results_dir <- file.path(session_dir, "results", mod)
    if (dir.exists(results_dir)) {
      plot_exts <- character(0)
      if ("png" %in% formats) plot_exts <- c(plot_exts, "png")
      if ("pdf" %in% formats) plot_exts <- c(plot_exts, "pdf")

      if (length(plot_exts) > 0) {
        pattern   <- paste0("\\.(", paste(plot_exts, collapse = "|"), ")$")
        plot_files <- list.files(results_dir, pattern = pattern,
                                 full.names = TRUE, recursive = TRUE)
        for (pf in plot_files) {
          rel <- sub(paste0("^", session_dir, "/"), "", pf)
          files_to_zip <- c(files_to_zip, pf)
          included     <- c(included, rel)
        }
      }
    } else if (any(c("png", "pdf") %in% formats)) {
      omissions[[length(omissions) + 1]] <- list(
        path   = paste0("results/", mod, "/"),
        reason = paste0("No results directory for module '", mod, "'. ",
                        "Analysis may not have been run.")
      )
    }
  }

  if (length(files_to_zip) == 0) {
    return(error_response("NO_FILES_FOUND",
                          "No output files found for the requested modules and formats.",
                          "exporter", recoverable = TRUE))
  }

  # Write manifest.json to a temp file and include in ZIP
  manifest <- list(
    session_id  = session_id,
    created_at  = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    modules     = modules,
    formats     = formats,
    included    = as.list(included),
    omitted     = omissions
  )
  manifest_path <- tempfile(fileext = ".json")
  write_json(manifest, manifest_path, auto_unbox = TRUE, pretty = TRUE)
  files_to_zip <- c(files_to_zip, manifest_path)

  # Create ZIP archive
  # zip() expects paths relative to a root; use session_dir as root
  zip_files_rel <- c(
    sub(paste0("^", normalizePath(session_dir), .Platform$file.sep), "",
        normalizePath(files_to_zip[-length(files_to_zip)])),
    "manifest.json"
  )

  # Copy manifest to session dir temporarily so relative path works
  manifest_dest <- file.path(session_dir, "manifest.json")
  file.copy(manifest_path, manifest_dest, overwrite = TRUE)
  on.exit(unlink(manifest_dest), add = TRUE)
  on.exit(unlink(manifest_path), add = TRUE)

  zip::zip(
    zipfile = zip_path,
    files   = zip_files_rel,
    root    = session_dir
  )

  ok_response(list(
    zip_path  = zip_path,
    omissions = omissions
  ))
}
