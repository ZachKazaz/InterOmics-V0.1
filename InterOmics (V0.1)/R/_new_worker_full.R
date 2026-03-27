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
        record_artifact("plot_consensus_vip", "Consensus VIP Plot", "skipped",
                        page = "consensus", tab_id = "consensus_vip",
                        artifact_type = "plot",
                        message = "VIP plot not generated — check worker log for details")
      }
      if (!is.null(vip_result$csv_path)) {
        record_artifact("table_consensus_vip", "Consensus VIP Table", "success",
                        page = "consensus", tab_id = "consensus_vip_table",
                        artifact_type = "table",
                        csv_path = vip_result$csv_path,
                        render_order = 6L)
      }
    }, error = function(e) {
      record_artifact("plot_consensus_vip", "Consensus VIP Plot", "skipped",
                      page = "consensus", tab_id = "consensus_vip",
                      artifact_type = "plot",
                      message = conditionMessage(e))
      append_worker_log("WARNING: consensus VIP failed with error: ", conditionMessage(e))
    })

    tryCatch({
      paths_list <- plot_diablo_blocks(model, results_dir, append_worker_log)
      for (b in names(model$X)) {
        safe_b <- gsub("[^A-Za-z0-9_]", "_", b)
        # Only comp 1 is the user-facing block-specific 2D scores tab.
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
                      page = "components", tab_id = "loadings",
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

