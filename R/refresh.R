# On-demand refresh of a saved scholarly search.
#
# Store both observations and the derived comparison in one new directory.
# No scheduler, mutable global cache, API key storage, or background process.

.refresh_previous_snapshot <- function(previous) {
  if (is.character(previous) && length(previous) == 1L &&
      !is.na(previous)) {
    previous <- read_search_snapshot(previous)
  }
  .check_search_snapshot(previous)
  previous
}

.refresh_directory <- function(directory) {
  directory <- .require_string(directory, "directory")
  if (file.exists(directory) || dir.exists(directory)) {
    stop("Refresh directory already exists; choose a new path",
         call. = FALSE)
  }
  if (!dir.exists(dirname(directory))) {
    stop("Refresh parent directory does not exist", call. = FALSE)
  }
  directory
}

#' Refresh a literature search and preserve its evidence
#'
#' Execute the SearchPlan contained in an earlier SearchSnapshot, compare the
#' results with that observation and write a self-contained report directory.
#' This is an explicit, synchronous operation; it does not install a scheduler
#' or contact providers outside the requested search.
#'
#' The output contains the previous and current snapshots in RDS format,
#' credential-free JSON manifests for both, and a CSV drift report. All files
#' are staged in a temporary sibling directory and published only when every
#' write succeeds. Existing output directories are never intentionally
#' overwritten. The previous snapshot or its source RDS file is unchanged.
#'
#' If the plan allows partial provider failures, the new snapshot still
#' captures them; its comparison is marked incomparable when either run is
#' incomplete. A missing DOI in the new results does not establish article
#' withdrawal or retraction.
#'
#' API keys and Crossref contact details are supplied only at execution time,
#' and are excluded from the plan and generated manifests. The full RDS files
#' intentionally preserve article text and provider error evidence: store them
#' privately. Changed field values (including abstracts) are omitted from the
#' CSV report unless include_values is TRUE.
#'
#' @param previous An existing SearchSnapshot or a trusted RDS path produced
#'   by [save_search_snapshot()]. Never read RDS from untrusted sources.
#' @param directory Destination directory, which must not already exist.
#'   The parent directory must exist.
#' @param scopus_api_key Optional Elsevier API key supplied at execution.
#' @param openalex_api_key Optional OpenAlex API key supplied at execution.
#' @param crossref_mailto Optional Crossref contact email.
#' @param include_values If TRUE, include changed metadata text in CSV events;
#'   defaults to FALSE to exclude potentially long abstracts.
#' @param requester Optional HTTP callback taking url, headers, params,
#'   timeout. Useful for deterministic offline tests.
#' @param sleep Injectable delay callback (default [Sys.sleep()]).
#' @param clock Injectable zero-argument time callback (default [Sys.time()]).
#'
#' @return A SearchRefresh list containing directory, snapshot and history.
#'   The saved files are named before.rds, after.rds, before-manifest.json,
#'   after-manifest.json, and report/ (CSV drift tables and interpretation
#'   notes).
#' @export
#'
#' @examples
#' plan <- new_search_plan("test", sources = "crossref")
#' mock <- function(url, headers, params, timeout) {
#'   list(message = list(items = list()))
#' }
#' before <- run_search_plan(plan, requester = mock)
#' path <- tempfile("search-refresh-")
#' refreshed <- refresh_search_snapshot(
#'   before, path, requester = mock, sleep = function(...) NULL
#' )
#' refreshed$history$transitions
#' unlink(path, recursive = TRUE)
refresh_search_snapshot <- function(
    previous, directory,
    scopus_api_key = NULL, openalex_api_key = NULL,
    crossref_mailto = NULL, include_values = FALSE,
    requester = NULL, sleep = Sys.sleep, clock = Sys.time) {
  # Validate the snapshot and destination before initiating API requests.
  before <- .refresh_previous_snapshot(previous)
  directory <- .refresh_directory(directory)
  if (!is.logical(include_values) || length(include_values) != 1L ||
      is.na(include_values)) {
    stop("include_values must be TRUE or FALSE", call. = FALSE)
  }
  if (!is.null(requester)) .validate_requester(requester)
  if (!is.function(sleep)) stop("sleep must be a function", call. = FALSE)
  if (!is.function(clock)) stop("clock must be a function", call. = FALSE)

  # On errors (including failed provider queries, invalid timestamps, and
  # disk errors), nothing is published at the requested output directory.
  after <- run_search_plan(
    plan = before$plan,
    scopus_api_key = scopus_api_key,
    openalex_api_key = openalex_api_key,
    crossref_mailto = crossref_mailto,
    requester = requester,
    sleep = sleep,
    clock = clock
  )
  history <- compare_search_history(list(before = before, after = after))

  staging <- tempfile(
    pattern = ".abstractExtractR-refresh-", tmpdir = dirname(directory)
  )
  if (!dir.create(staging)) {
    stop("Could not create refresh staging directory", call. = FALSE)
  }
  on.exit(unlink(staging, recursive = TRUE), add = TRUE)

  save_search_snapshot(before, file.path(staging, "before.rds"))
  save_search_snapshot(after, file.path(staging, "after.rds"))
  write_search_manifest(before, file.path(staging, "before-manifest.json"))
  write_search_manifest(after, file.path(staging, "after-manifest.json"))
  write_search_history_report(
    history, file.path(staging, "report"),
    include_values = include_values
  )

  notes <- c(
    "# Literature search refresh",
    "",
    "Before and after RDS snapshots preserve complete research metadata.",
    "Both JSON manifests summarize provider counts without API credentials.",
    "The report/ directory contains DOI-based comparison CSV tables.",
    "The operation explicitly reran the prior search plan; it was not scheduled.",
    "Provider failures and differences in search coverage affect comparability.",
    "Added or removed DOIs do not establish publication or retraction events.",
    "Treat both RDS snapshots as private research evidence: they may contain",
    "article abstracts and provider error messages.",
    if (include_values) {
      "Changed metadata text is included in report/events.csv."
    } else {
      "Changed metadata text is excluded from report/events.csv by default."
    }
  )
  writeLines(notes, file.path(staging, "README.md"), useBytes = TRUE)

  # Recheck before publishing in case another process created the directory.
  if (file.exists(directory) || dir.exists(directory)) {
    stop("Refresh directory already exists; refusing to overwrite",
         call. = FALSE)
  }
  if (!file.rename(staging, directory)) {
    stop("Could not publish completed refresh directory", call. = FALSE)
  }

  structure(
    list(directory = directory, snapshot = after, history = history),
    class = "SearchRefresh"
  )
}
