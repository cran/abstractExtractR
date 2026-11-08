# Longitudinal metadata observations for a canonical DOI.
# Only runs identified by DOI manifest indexes need RDS deserialization.

.doi_text_hash <- function(value) {
  if (is.null(value) || !is.character(value) ||
      length(value) != 1L || is.na(value)) return(NA_character_)
  bytes <- charToRaw(enc2utf8(value))
  paste(sprintf("%02x", as.integer(openssl::sha256(bytes))), collapse = "")
}

.doi_value <- function(value) {
  if (is.null(value)) return(NA_character_)
  if (length(value) == 0L) return(NA_character_)
  paste(value, collapse = " | ")
}

#' Follow scholarly article metadata across archived searches
#'
#' Uses canonical DOI identity and archive manifest indexes to find observed
#' runs, loading only the matching RDS files. Tracks title, publication date,
#' authors, abstract presence and SHA-256 fingerprint, source attribution,
#' and metadata changes between successive *observations*. A missing DOI in
#' an intervening run is described as an observation gap, not a retraction.
#'
#' By default full abstract text is excluded. SHA-256 fingerprints indicate
#' exact textual changes, not their significance or scientific validity.
#' Provider failures and varying coverage limit interpretation.
#'
#' @param directory Existing research archive directory.
#' @param doi A single valid DOI or doi.org resolver URL.
#' @param include_abstract Logical; include full abstract text when TRUE.
#' @param on_unindexed Either "read" (default) or "error" for older manifests.
#' @param verify_integrity Verify archive checksums, TRUE by default.
#' @return List with doi, observations, changes, gaps, summary and
#'   integrity_checked. Timestamps are search-observation times.
#' @export
#' @examples
#' plan <- new_search_plan("example", sources = "crossref")
#' mock <- function(url, headers, params, timeout) {
#'   list(message = list(items = list(
#'     list(title = list("Study"), DOI = "10.1000/example")
#'   )))
#' }
#' snap <- run_search_plan(plan, requester = mock)
#' path <- tempfile("doi-history-")
#' create_search_archive(snap, path)
#' article_metadata_timeline(path, "10.1000/example")$summary
#' unlink(path, recursive = TRUE)
article_metadata_timeline <- function(
    directory, doi, include_abstract = FALSE,
    on_unindexed = c("read", "error"), verify_integrity = TRUE) {
  if (!is.character(doi) || length(doi) != 1L || is.na(doi)) {
    stop("doi must be a single valid DOI string", call. = FALSE)
  }
  canonical <- normalize_doi(doi)
  if (is.na(canonical[[1L]])) {
    stop("doi must be a valid DOI identifier", call. = FALSE)
  }
  if (!is.logical(include_abstract) || length(include_abstract) != 1L ||
      is.na(include_abstract)) {
    stop("include_abstract must be TRUE or FALSE", call. = FALSE)
  }
  trace <- trace_archive_dois(
    directory, canonical[[1L]], on_unindexed = on_unindexed,
    verify_integrity = verify_integrity
  )
  inventory <- list_search_archive_runs(
    directory, verify_integrity = verify_integrity
  )
  found <- trace$occurrences
  observed <- vector("list", nrow(found))
  for (i in seq_len(nrow(found))) {
    snapshot <- read_search_archive_run(directory, found$run[[i]])
    index <- found$article_index[[i]]
    if (index > length(snapshot$result$articles)) {
      stop("DOI index points outside snapshot articles", call. = FALSE)
    }
    article <- snapshot$result$articles[[index]]
    actual <- normalize_doi(article$doi)
    if (is.na(actual[[1L]]) ||
        !identical(actual[[1L]], canonical[[1L]])) {
      stop("DOI index does not match saved article evidence", call. = FALSE)
    }
    abstract <- article$abstract
    observed[[i]] <- data.frame(
      run = found$run[[i]], run_index = found$run_index[[i]],
      observed_at = found$observed_at[[i]],
      title = .doi_value(article$title),
      publication_date = .doi_value(article$publication_date),
      authors = paste(article$authors, collapse = "; "),
      sources = found$sources[[i]],
      has_abstract = !is.null(abstract) && nzchar(trimws(abstract)),
      abstract_sha256 = .doi_text_hash(abstract),
      stringsAsFactors = FALSE
    )
    if (include_abstract) {
      observed[[i]]$abstract <- .doi_value(abstract)
    }
  }
  template <- data.frame(
    run = character(), run_index = integer(), observed_at = character(),
    title = character(), publication_date = character(),
    authors = character(), sources = character(),
    has_abstract = logical(), abstract_sha256 = character(),
    stringsAsFactors = FALSE
  )
  if (include_abstract) template$abstract <- character()
  observations <- if (length(observed) == 0L) template else do.call(rbind, observed)
  rownames(observations) <- NULL

  fields <- c("title", "publication_date", "authors", "sources",
              "has_abstract", "abstract_sha256")
  changes <- list()
  if (nrow(observations) > 1L) {
    for (i in seq.int(2L, nrow(observations))) {
      for (field in fields) {
        left <- observations[[field]][[i - 1L]]
        right <- observations[[field]][[i]]
        if (!identical(left, right)) {
          changes[[length(changes) + 1L]] <- data.frame(
            from_run = observations$run[[i - 1L]],
            to_run = observations$run[[i]],
            field = field,
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }
  empty_changes <- data.frame(
    from_run = character(), to_run = character(), field = character(),
    stringsAsFactors = FALSE
  )
  change_table <- if (length(changes) == 0L) {
    empty_changes
  } else do.call(rbind, changes)
  rownames(change_table) <- NULL
  absent <- setdiff(inventory$run, observations$run)
  gaps <- inventory[inventory$run %in% absent,
                    c("run", "finished_at", "complete"), drop = FALSE]
  rownames(gaps) <- NULL
  list(
    doi = canonical[[1L]],
    observations = observations,
    changes = change_table,
    gaps = gaps,
    summary = data.frame(
      observed_runs = nrow(observations),
      unobserved_runs = nrow(gaps),
      metadata_changes = nrow(change_table),
      first_observed_at = if (nrow(observations)) observations$observed_at[[1L]] else NA_character_,
      last_observed_at = if (nrow(observations)) utils::tail(observations$observed_at, 1L) else NA_character_,
      stringsAsFactors = FALSE
    ),
    integrity_checked = verify_integrity
  )
}
