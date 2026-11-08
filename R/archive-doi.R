# Compact per-run DOI lookup indexes for scholarly research archives.
#
# The index contains DOI identities and article positions, not abstracts.
# Existing archives without this index remain readable via RDS fallback.

.archive_index_records <- function(snapshot) {
  .check_search_snapshot(snapshot)
  keys <- .snapshot_doi_index(snapshot)$keys
  selected <- which(!is.na(keys))
  provenance <- snapshot$result$provenance
  if (!all(c("source", "output_index") %in% names(provenance))) {
    stop("Snapshot provenance lacks source or output_index", call. = FALSE)
  }
  providers <- vapply(selected, function(index) {
    sources <- as.character(
      provenance$source[provenance$output_index == index]
    )
    paste(sort(unique(sources[!is.na(sources)])), collapse = ";")
  }, character(1L), USE.NAMES = FALSE)

  data.frame(
    doi = unname(keys[selected]),
    article_index = as.integer(selected),
    sources = providers,
    stringsAsFactors = FALSE
  )
}

.archive_decode_doi_index <- function(manifest) {
  records <- manifest$doi_index
  if (is.null(records)) {
    return(NULL) # A manifest created before DOI indexes existed.
  }
  if (!is.list(records) ||
      (length(records) > 0L && !is.null(names(records)))) {
    stop("Archive DOI index must be a JSON array", call. = FALSE)
  }
  empty <- data.frame(
    doi = character(), article_index = integer(),
    sources = character(), stringsAsFactors = FALSE
  )
  if (length(records) == 0L) return(empty)

  numeric_index <- function(value, max_index) {
    is.numeric(value) && length(value) == 1L &&
      !is.na(value) && is.finite(value) &&
      value >= 1 && value <= max_index && value == floor(value)
  }
  scalar_string <- function(value) {
    is.character(value) && length(value) == 1L && !is.na(value)
  }
  count <- manifest$counts$normalized_articles
  if (!is.numeric(count) || length(count) != 1L ||
      is.na(count) || !is.finite(count) ||
      count < 0 || count > .Machine$integer.max || count != floor(count)) {
    stop("Archive DOI index has an invalid article count", call. = FALSE)
  }

  rows <- lapply(records, function(record) {
    if (!is.list(record) ||
        !setequal(names(record), c("doi", "article_index", "sources")) ||
        !scalar_string(record$doi) ||
        !scalar_string(record$sources) ||
        !numeric_index(record$article_index, count)) {
      stop("Archive DOI index contains an invalid record", call. = FALSE)
    }
    canonical <- normalize_doi(record$doi)
    if (is.na(canonical[[1L]]) ||
        !identical(canonical[[1L]], record$doi)) {
      stop("Archive DOI index contains a noncanonical DOI", call. = FALSE)
    }
    data.frame(
      doi = record$doi,
      article_index = as.integer(record$article_index),
      sources = record$sources,
      stringsAsFactors = FALSE
    )
  })
  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  if (anyDuplicated(result$doi) ||
      anyDuplicated(result$article_index)) {
    stop("Archive DOI index contains duplicate identifiers", call. = FALSE)
  }
  result
}

#' Trace DOI observations across a research archive
#'
#' Locate normalized DOIs across all archived search runs using compact
#' JSON manifest indexes rather than loading every RDS snapshot. The result
#' records which run first and last observed each DOI, its position in that
#' run, and the contributing provider names. Dates are search-observation
#' timestamps, not publication, withdrawal or retraction dates.
#'
#' Manifests created before DOI indexes are supported: when on_unindexed is
#' "read" (default), only those older RDS snapshots are loaded and validated
#' to recover their DOI positions. Use "error" to forbid fallback reads.
#'
#' The archive inventory is checked for chronological consistency. By
#' default all archived RDS/manifest file checksums are verified without
#' deserializing indexed runs. Setting verify_integrity = FALSE skips those
#' checks for indexed runs and labels the returned result as unverified.
#' Legacy RDS fallback always performs its available integrity checks.
#'
#' No network queries are performed. Records without valid DOIs cannot be
#' traced using this function.
#'
#' @param directory Existing research archive directory.
#' @param dois Character vector of one or more valid DOI representations.
#'   Equivalent DOIs in the input are deduplicated after normalization.
#' @param on_unindexed Either "read" (default) or "error" for older manifests.
#' @param verify_integrity Logical. TRUE (default) verifies all stored file
#'   checksums; FALSE performs a faster metadata-only scan.
#'
#' @return An ArchiveDOITrace list with occurrences (one row per matching
#'   archive run), summary (one row per requested DOI, including absent
#'   identifiers), legacy_runs (runs requiring RDS fallback), and the
#'   integrity_checked flag.
#' @export
#'
#' @examples
#' plan <- new_search_plan("example", sources = "crossref")
#' mock <- function(url, headers, params, timeout) {
#'   list(message = list(items = list(
#'     list(title = list("Paper"), DOI = "10.1000/example")
#'   )))
#' }
#' snapshot <- run_search_plan(plan, requester = mock)
#' path <- tempfile("archive-")
#' create_search_archive(snapshot, path)
#' trace_archive_dois(path, "https://doi.org/10.1000/example")$summary
#' unlink(path, recursive = TRUE)
trace_archive_dois <- function(
    directory, dois, on_unindexed = c("read", "error"),
    verify_integrity = TRUE) {
  on_unindexed <- match.arg(on_unindexed)
  if (!is.character(dois) || length(dois) == 0L ||
      anyNA(dois)) {
    stop("dois must be a nonempty character vector", call. = FALSE)
  }
  normalized <- normalize_doi(dois)
  if (anyNA(normalized)) {
    stop("dois must contain valid DOI identifiers", call. = FALSE)
  }
  requested <- unique(unname(normalized))
  if (!is.logical(verify_integrity) ||
      length(verify_integrity) != 1L || is.na(verify_integrity)) {
    stop("verify_integrity must be TRUE or FALSE", call. = FALSE)
  }

  inventory <- list_search_archive_runs(
    directory, verify_integrity = verify_integrity
  )
  folders <- file.path(directory, "runs", inventory$run)
  matches <- list()
  legacy_runs <- character()

  for (i in seq_along(folders)) {
    folder <- folders[[i]]
    manifest <- .archive_manifest_metadata(
      file.path(folder, "manifest.json")
    )
    index <- .archive_decode_doi_index(manifest)
    if (is.null(index)) {
      if (on_unindexed == "error") {
        stop(
          sprintf("Archive run %s has no DOI index", inventory$run[[i]]),
          call. = FALSE
        )
      }
      # Only previously unindexed observations require RDS deserialization.
      snapshot <- .archive_read_run(folder)
      index <- .archive_index_records(snapshot)
      legacy_runs <- c(legacy_runs, inventory$run[[i]])
    }

    selected <- index[index$doi %in% requested, , drop = FALSE]
    if (nrow(selected) > 0L) {
      matches[[length(matches) + 1L]] <- data.frame(
        doi = selected$doi,
        run = rep(inventory$run[[i]], nrow(selected)),
        run_index = rep.int(as.integer(i), nrow(selected)),
        article_index = selected$article_index,
        observed_at = rep(inventory$finished_at[[i]], nrow(selected)),
        sources = selected$sources,
        stringsAsFactors = FALSE
      )
    }
  }

  prototype <- data.frame(
    doi = character(), run = character(), run_index = integer(),
    article_index = integer(), observed_at = character(),
    sources = character(), stringsAsFactors = FALSE
  )
  occurrences <- if (length(matches) == 0L) {
    prototype
  } else {
    do.call(rbind, matches)
  }
  rownames(occurrences) <- NULL
  if (nrow(occurrences) > 0L) {
    ordered <- order(match(occurrences$doi, requested),
                     occurrences$run_index)
    occurrences <- occurrences[ordered, , drop = FALSE]
    rownames(occurrences) <- NULL
  }

  summary <- data.frame(
    doi = requested,
    observed_runs = integer(length(requested)),
    first_run = rep(NA_character_, length(requested)),
    last_run = rep(NA_character_, length(requested)),
    first_observed_at = rep(NA_character_, length(requested)),
    last_observed_at = rep(NA_character_, length(requested)),
    stringsAsFactors = FALSE
  )
  for (i in seq_along(requested)) {
    rows <- occurrences[occurrences$doi == requested[[i]], , drop = FALSE]
    n <- nrow(rows)
    summary$observed_runs[[i]] <- as.integer(n)
    if (n > 0L) {
      summary$first_run[[i]] <- rows$run[[1L]]
      summary$last_run[[i]] <- rows$run[[n]]
      summary$first_observed_at[[i]] <- rows$observed_at[[1L]]
      summary$last_observed_at[[i]] <- rows$observed_at[[n]]
    }
  }

  structure(
    list(
      occurrences = occurrences,
      summary = summary,
      legacy_runs = legacy_runs,
      integrity_checked = verify_integrity
    ),
    class = "ArchiveDOITrace"
  )
}
