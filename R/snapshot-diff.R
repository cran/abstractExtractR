# Conservative comparison of saved scholarly-search evidence.

.snapshot_doi_index <- function(snapshot) {
  articles <- snapshot$result$articles
  dois <- vapply(articles, function(a) {
    .validate_dedup_article(a)
    if (is.null(a$doi)) NA_character_ else a$doi
  }, character(1L), USE.NAMES = FALSE)
  canonical <- normalize_doi(dois)
  valid <- which(!is.na(canonical))
  if (anyDuplicated(canonical[valid])) {
    stop("Snapshot contains duplicate normalized DOI records", call. = FALSE)
  }
  list(keys = canonical, valid = valid)
}

#' Compare two literature-search snapshots conservatively
#'
#' Uses only normalized DOI identity to compare scholarly Article records.
#' Missing-DOI records are listed separately as unmatched, not paired by
#' title or author. Reports added and removed DOIs and field-level changes
#' for title, abstract, publication_date, authors and DOI.
#'
#' Provenance changes compare provider-name sets, not row positions or counts.
#' Comparison is descriptive: disappearance does not imply retraction, since
#' query settings, coverage, availability and provider failures can differ.
#'
#' @param before,after Valid SearchSnapshot objects (or trusted RDS paths
#'   returned by save_search_snapshot()).
#' @return List containing summary, added, removed, modified, provenance,
#'   unmatched, and comparability diagnostics.
#' @export
#' @examples
#' plan <- new_search_plan("test", sources = "crossref")
#' mock <- function(url, headers, params, timeout) {
#'   list(message = list(items = list()))
#' }
#' snap <- run_search_plan(plan, requester = mock)
#' compare_search_snapshots(snap, snap)$summary
compare_search_snapshots <- function(before, after) {
  if (is.character(before) && length(before) == 1L) {
    before <- read_search_snapshot(before)
  }
  if (is.character(after) && length(after) == 1L) {
    after <- read_search_snapshot(after)
  }
  .check_search_snapshot(before)
  .check_search_snapshot(after)
  old <- .snapshot_doi_index(before)
  new <- .snapshot_doi_index(after)
  old_keys <- old$keys[old$valid]
  new_keys <- new$keys[new$valid]
  added <- setdiff(new_keys, old_keys)
  removed <- setdiff(old_keys, new_keys)
  common <- intersect(old_keys, new_keys)

  fields <- c("title", "abstract", "publication_date", "authors", "doi")
  changes <- list()
  provenance_changes <- list()
  source_set <- function(snapshot, output_index) {
    p <- snapshot$result$provenance
    required <- c("output_index", "source")
    if (!all(required %in% names(p))) {
      stop("Snapshot provenance lacks source or output_index", call. = FALSE)
    }
    sort(unique(as.character(p$source[p$output_index == output_index])))
  }

  for (doi in common) {
    before_index <- match(doi, old$keys)
    after_index <- match(doi, new$keys)
    first <- before$result$articles[[before_index]]
    second <- after$result$articles[[after_index]]
    for (field in fields) {
      left <- first[[field]]
      right <- second[[field]]
      if (!identical(left, right)) {
        changes[[length(changes) + 1L]] <- data.frame(
          doi = doi, field = field,
          before = if (is.null(left)) NA_character_ else paste(left, collapse = " | "),
          after = if (is.null(right)) NA_character_ else paste(right, collapse = " | "),
          stringsAsFactors = FALSE
        )
      }
    }
    a <- source_set(before, before_index)
    b <- source_set(after, after_index)
    if (!identical(a, b)) {
      provenance_changes[[length(provenance_changes) + 1L]] <- data.frame(
        doi = doi, before = paste(a, collapse = ", "),
        after = paste(b, collapse = ", "), stringsAsFactors = FALSE
      )
    }
  }
  empty_changes <- data.frame(
    doi = character(), field = character(), before = character(),
    after = character(), stringsAsFactors = FALSE
  )
  empty_sources <- empty_changes[, c("doi", "before", "after")]
  bind_rows <- function(items, prototype) {
    if (length(items) == 0L) prototype else do.call(rbind, items)
  }
  modified <- bind_rows(changes, empty_changes)
  provenance <- bind_rows(provenance_changes, empty_sources)
  unmatched <- data.frame(
    snapshot = c(rep("before", sum(is.na(old$keys))),
                 rep("after", sum(is.na(new$keys)))),
    input_index = c(which(is.na(old$keys)), which(is.na(new$keys))),
    stringsAsFactors = FALSE
  )
  same_plan <- identical(unclass(before$plan), unclass(after$plan))
  complete <- isTRUE(before$context$complete) &&
    isTRUE(after$context$complete)
  summary <- data.frame(
    added = length(added), removed = length(removed),
    modified_articles = length(unique(modified$doi)),
    provenance_changed = nrow(provenance),
    unmatched_before = sum(is.na(old$keys)),
    unmatched_after = sum(is.na(new$keys)),
    same_plan = same_plan, complete_sources = complete,
    stringsAsFactors = FALSE
  )
  list(
    summary = summary,
    added = data.frame(doi = added, stringsAsFactors = FALSE),
    removed = data.frame(doi = removed, stringsAsFactors = FALSE),
    modified = modified,
    provenance = provenance,
    unmatched = unmatched,
    comparability = list(
      same_plan = same_plan, complete_sources = complete,
      warning = if (same_plan && complete) NULL else
        "Search settings differ or some providers failed; observed differences are not necessarily literature changes."
    )
  )
}
