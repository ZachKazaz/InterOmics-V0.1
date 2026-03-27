# id_mapper.R
# Entry point: run_id_mapping(session_id)
# Maps metabolite feature names to KEGG compound IDs and lipid feature names
# to lipid classes. Writes id_mapping.json to the session directory.

source("R/utils.R")

# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

# Recognised lipid class prefixes in standard lipid notation
.LIPID_CLASSES <- c(
  "PC", "PE", "PS", "PI", "PG", "PA",
  "SM", "TG", "DG", "MG", "CE", "Cer",
  "HexCer", "LPC", "LPE"
)

# Regex: KEGG compound ID  C followed by exactly 5 digits
.KEGG_ID_REGEX <- "^C\\d{5}$"

# Regex: HMDB identifier
.HMDB_REGEX <- "^HMDB\\d{5,7}$"

# ---------------------------------------------------------------------------
# 2.3.1  Metabolite → KEGG mapping
# ---------------------------------------------------------------------------

#' Map a single metabolite feature name to a KEGG compound ID.
#'
#' Priority order:
#'   1. Direct KEGG compound ID match (regex C\d{5})
#'   2. HMDB cross-reference via KEGGREST::keggFind
#'   3. Common name search via KEGGREST::keggFind
#'
#' Returns a named list:
#'   kegg_id      – character or NA
#'   mapping_type – "direct_kegg" | "hmdb" | "name" | NA
#'   ambiguous    – logical (TRUE when multiple candidates were found)
map_metabolite_to_kegg <- function(feature_name) {
  if (is.na(feature_name) || nchar(trimws(feature_name)) == 0) {
    return(list(kegg_id = NA_character_, mapping_type = NA_character_,
                ambiguous = FALSE))
  }

  name <- trimws(feature_name)

  # --- Priority 1: direct KEGG compound ID ---------------------------------
  if (grepl(.KEGG_ID_REGEX, name)) {
    return(list(kegg_id = name, mapping_type = "direct_kegg", ambiguous = FALSE))
  }

  # --- Priority 2: HMDB cross-reference ------------------------------------
  if (grepl(.HMDB_REGEX, name, ignore.case = TRUE)) {
    result <- .kegg_find_safe(name)
    if (!is.null(result) && length(result) > 0) {
      ids       <- names(result)
      kegg_ids  <- ids[grepl(.KEGG_ID_REGEX, ids)]
      if (length(kegg_ids) > 0) {
        return(list(
          kegg_id      = kegg_ids[[1]],
          mapping_type = "hmdb",
          ambiguous    = length(kegg_ids) > 1
        ))
      }
    }
  }

  # --- Priority 3: common name search via KEGGREST --------------------------
  result <- .kegg_find_safe(name)
  if (!is.null(result) && length(result) > 0) {
    ids      <- names(result)
    kegg_ids <- ids[grepl(.KEGG_ID_REGEX, ids)]
    if (length(kegg_ids) > 0) {
      return(list(
        kegg_id      = kegg_ids[[1]],
        mapping_type = "name",
        ambiguous    = length(kegg_ids) > 1
      ))
    }
  }

  # --- No match found -------------------------------------------------------
  list(kegg_id = NA_character_, mapping_type = NA_character_, ambiguous = FALSE)
}

# Safe wrapper around KEGGREST::keggFind — returns NULL on any error
.kegg_find_safe <- function(query) {
  if (!requireNamespace("KEGGREST", quietly = TRUE)) {
    warning("KEGGREST is not installed; metabolite name mapping will be skipped.")
    return(NULL)
  }
  tryCatch(
    KEGGREST::keggFind("compound", query),
    error   = function(e) NULL,
    warning = function(w) NULL
  )
}

# ---------------------------------------------------------------------------
# 2.3.2  Lipid class parsing
# ---------------------------------------------------------------------------

#' Parse the lipid class from a standard lipid notation feature name.
#'
#' Extracts the class prefix using the pattern ^([A-Z][A-Za-z]*)\(
#' Matches against the recognised class list.
#' Returns the class string or NA if unrecognised.
parse_lipid_class <- function(feature_name) {
  if (is.na(feature_name) || nchar(trimws(feature_name)) == 0) return(NA_character_)

  # Extract prefix before the first '('
  m <- regmatches(feature_name, regexpr("^([A-Z][A-Za-z]*)\\(", feature_name))
  if (length(m) == 0 || nchar(m) == 0) return(NA_character_)

  prefix <- sub("\\($", "", m)  # strip trailing '('

  if (prefix %in% .LIPID_CLASSES) prefix else NA_character_
}

# ---------------------------------------------------------------------------
# 2.3.3  Orchestrator: run_id_mapping(session_id)
# ---------------------------------------------------------------------------

#' Run feature ID mapping for all preprocessed datasets in a session.
#'
#' Writes id_mapping.json to storage/{session_id}/ and updates pipeline_state.
#' Returns an ok_response with mapping_success_rate_pct, warning (if < 60%),
#' and unmapped_features.
run_id_mapping <- function(session_id) {
  pre_dir <- get_session_path(session_id, "preprocessed")
  csvs    <- list.files(pre_dir, pattern = "\\.csv$", full.names = TRUE)

  if (length(csvs) == 0) {
    return(error_response("NO_FILES_FOUND",
                          "No preprocessed feature matrix files found.",
                          "id_mapper", recoverable = TRUE))
  }

  all_records <- list()

  for (f in csvs) {
    mat <- read.csv(f, stringsAsFactors = FALSE, check.names = FALSE)
    feature_names <- setdiff(colnames(mat), "SampleID")

    for (feat in feature_names) {
      record <- .map_single_feature(feat)
      all_records <- c(all_records, list(record))
    }
  }

  # Deduplicate by feature name (same feature may appear in multiple datasets)
  seen     <- character(0)
  unique_records <- list()
  for (r in all_records) {
    if (!r$feature %in% seen) {
      seen           <- c(seen, r$feature)
      unique_records <- c(unique_records, list(r))
    }
  }

  total   <- length(unique_records)
  mapped  <- sum(sapply(unique_records, function(r) {
    !is.na(r$kegg_id) || !is.na(r$lipid_class)
  }))

  success_rate <- if (total > 0) round(mapped / total * 100, 2) else 0

  unmapped <- sapply(unique_records, function(r) {
    if (is.na(r$kegg_id) && is.na(r$lipid_class)) r$feature else NA_character_
  })
  unmapped <- unmapped[!is.na(unmapped)]

  # Write id_mapping.json
  mapping_path <- get_session_path(session_id, "id_mapping.json")
  jsonlite::write_json(unique_records, mapping_path,
                       auto_unbox = TRUE, pretty = TRUE)

  # Update pipeline state
  state <- read_pipeline_state(session_id)
  state$steps$map_ids <- "complete"
  write_pipeline_state(session_id, state)

  # Build response
  warn_msg <- NULL
  if (success_rate < 60) {
    warn_msg <- paste0(
      "Mapping success rate is ", success_rate,
      "%. This is below the 60% threshold. ",
      "Enrichment results may be incomplete."
    )
  }

  ok_response(list(
    mapping_success_rate_pct = success_rate,
    warning                  = warn_msg,
    unmapped_features        = as.list(unmapped)
  ))
}

# ---------------------------------------------------------------------------
# Internal: classify and map a single feature name
# ---------------------------------------------------------------------------

.map_single_feature <- function(feat) {
  # Determine whether this looks like a lipid (has standard lipid notation)
  # or a metabolite, then apply the appropriate mapping.

  lipid_class <- parse_lipid_class(feat)

  if (!is.na(lipid_class)) {
    # Lipid: class parsed; no KEGG mapping attempted for lipids
    return(list(
      feature      = feat,
      kegg_id      = NA_character_,
      lipid_class  = lipid_class,
      mapping_type = "lipid_class",
      ambiguous    = FALSE
    ))
  }

  # Metabolite: attempt KEGG mapping
  kegg_result <- map_metabolite_to_kegg(feat)

  list(
    feature      = feat,
    kegg_id      = kegg_result$kegg_id,
    lipid_class  = NA_character_,
    mapping_type = kegg_result$mapping_type,
    ambiguous    = kegg_result$ambiguous
  )
}
