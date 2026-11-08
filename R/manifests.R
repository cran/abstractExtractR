# Human-readable, credential-free summary of a versioned search snapshot.
#
# Manifests intentionally contain no provider error text or API credentials.
# They complement, rather than replace, the full SearchSnapshot RDS evidence.

.SEARCH_MANIFEST_VERSION <- 1L

#' Summarize a scholarly search as a structured manifest
#'
#' Creates a small, credential-free summary of one validated SearchSnapshot:
#' the search plan, UTC execution times, runtime versions, provider status
#' counts, DOI coverage, and metadata reconciliation counts. Original
#' article abstracts and provider error messages are excluded.
#'
#' A manifest describes retrieved evidence, not a reproducible guarantee
#' that external providers will return the same results in a later search.
#' For a complete record, preserve the source SearchSnapshot RDS as well.
#'
#' @param snapshot SearchSnapshot from run_search_plan().
#' @return A SearchManifest list with schema, plan, context, counts,
#'   and provider summary.
#' @export
#' @examples
#' plan <- new_search_plan("topic", sources = "crossref")
#' mock <- function(url, headers, params, timeout) {
#'   list(message = list(items = list()))
#' }
#' snapshot <- run_search_plan(plan, requester = mock)
#' search_snapshot_manifest(snapshot)$counts
search_snapshot_manifest <- function(snapshot) {
  .check_search_snapshot(snapshot)
  result <- snapshot$result
  sources <- result$sources
  required <- c("source", "status", "retrieved", "contributed")
  if (!all(required %in% names(sources))) {
    stop("Snapshot source summary is missing required columns",
         call. = FALSE)
  }

  # Validate the Article fields before deriving DOI counts.
  for (article in result$articles) {
    .validate_dedup_article(article)
  }
  dois <- vapply(result$articles, function(article) {
    if (is.null(article$doi)) NA_character_ else article$doi
  }, character(1L), USE.NAMES = FALSE)
  canonical <- normalize_doi(dois)
  valid_doi <- sum(!is.na(canonical))

  # Derive the DOI index once, rather than once for every record.
  doi_index <- .archive_index_records(snapshot)
  plan <- unclass(snapshot$plan)
  # Keep one-element source vectors as JSON arrays, not scalar strings.
  # jsonlite's auto_unbox would encode a single source as a scalar.
  # Base R's AsIs wrapper keeps even one provider as a JSON array.
  plan$sources <- I(unname(plan$sources))
  context <- snapshot$context
  manifest <- list(
    manifest_schema_version = .SEARCH_MANIFEST_VERSION,
    snapshot_schema_version = snapshot$schema_version,
    started_at = snapshot$started_at,
    finished_at = snapshot$finished_at,
    plan = plan,
    runtime = list(
      package_version = context$package_version,
      R_version = context$R_version,
      platform = context$platform,
      complete = context$complete
    ),
    counts = list(
      normalized_articles = as.integer(length(result$articles)),
      articles_with_doi = as.integer(valid_doi),
      articles_without_doi = as.integer(length(result$articles) - valid_doi),
      provider_records = as.integer(sum(sources$retrieved)),
      field_conflicts = as.integer(nrow(result$conflicts)),
      provider_failures = as.integer(sum(sources$status == "failed"))
    ),
    providers = sources[, required, drop = FALSE],
    doi_index = lapply(seq_len(nrow(doi_index)), function(i) {
      list(doi = doi_index$doi[[i]],
           article_index = doi_index$article_index[[i]],
           sources = doi_index$sources[[i]])
    })
  )
  structure(manifest, class = "SearchManifest")
}

#' Write a structured JSON search manifest
#'
#' Writes a small, human-readable UTF-8 JSON summary of a SearchSnapshot.
#' The manifest intentionally excludes API keys, contact emails, article
#' abstracts, and raw error messages. A temporary file is written in the
#' destination directory before renaming; existing files are not
#' intentionally overwritten.
#'
#' @param snapshot Valid SearchSnapshot.
#' @param file Path to a new JSON file. Its parent directory must exist.
#' @return File path invisibly.
#' @export
#' @examples
#' plan <- new_search_plan("example", sources = "crossref")
#' mock <- function(url, headers, params, timeout) {
#'   list(message = list(items = list()))
#' }
#' snapshot <- run_search_plan(plan, requester = mock)
#' target <- tempfile(fileext = ".json")
#' write_search_manifest(snapshot, target)
#' unlink(target)
write_search_manifest <- function(snapshot, file) {
  manifest <- search_snapshot_manifest(snapshot)
  file <- .require_string(file, "file")
  directory <- dirname(file)
  if (!dir.exists(directory)) {
    stop("Parent directory for manifest does not exist", call. = FALSE)
  }
  if (file.exists(file)) {
    stop("Manifest file already exists; choose a new path", call. = FALSE)
  }
  json <- jsonlite::toJSON(
    unclass(manifest),
    pretty = TRUE,
    auto_unbox = TRUE,
    null = "null",
    na = "null",
    dataframe = "rows",
    digits = NA
  )
  temporary <- tempfile(
    pattern = ".abstractExtractR-manifest-",
    tmpdir = directory, fileext = ".json"
  )
  on.exit(unlink(temporary), add = TRUE)
  connection <- base::file(temporary, open = "wb")
  tryCatch(
    writeBin(charToRaw(enc2utf8(paste0(as.character(json), "\n"))),
             connection),
    finally = close(connection)
  )
  if (!file.rename(temporary, file)) {
    stop("Could not move manifest into place", call. = FALSE)
  }
  invisible(file)
}
