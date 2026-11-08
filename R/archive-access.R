# Lightweight archive inventories and selective run access.
#
# The full read_search_archive() deliberately loads all evidence and builds a
# SearchHistory. These helpers avoid deserializing unrelated RDS files when
# only a run inventory or a particular observation is needed.

.archive_run_folders <- function(directory) {
  directory <- .archive_path(directory, exists = TRUE)
  runs_directory <- file.path(directory, "runs")
  if (!dir.exists(runs_directory)) {
    stop("Search archive runs directory is missing", call. = FALSE)
  }
  names <- sort(list.files(runs_directory, all.files = FALSE))
  if (length(names) == 0L ||
      length(names) > .SEARCH_ARCHIVE_MAX_RUNS ||
      !identical(
        names, vapply(seq_along(names), .archive_run_name,
                      character(1L), USE.NAMES = FALSE)
      )) {
    stop("Archive run folders must be consecutive, starting at run_000001",
         call. = FALSE)
  }
  paths <- file.path(runs_directory, names)
  if (!all(dir.exists(paths))) {
    stop("Archive contains a run entry that is not a directory",
         call. = FALSE)
  }
  paths
}

.archive_manifest_metadata <- function(path) {
  if (!file.exists(path) || dir.exists(path)) {
    stop("Archive run manifest is missing", call. = FALSE)
  }
  manifest <- tryCatch(
    jsonlite::fromJSON(path, simplifyVector = FALSE),
    error = function(err) {
      stop("Archive run manifest is not valid JSON", call. = FALSE)
    }
  )
  scalar_string <- function(value) {
    is.character(value) && length(value) == 1L &&
      !is.na(value) && nzchar(value)
  }
  integer_count <- function(value) {
    is.numeric(value) && length(value) == 1L &&
      !is.na(value) && is.finite(value) &&
      value == floor(value) && value >= 0 &&
      value <= .Machine$integer.max
  }
  time_pattern <- "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$"
  if (!is.list(manifest) ||
      !is.numeric(manifest$manifest_schema_version) ||
      length(manifest$manifest_schema_version) != 1L ||
      !identical(as.integer(manifest$manifest_schema_version),
                 .SEARCH_MANIFEST_VERSION) ||
      !is.numeric(manifest$snapshot_schema_version) ||
      length(manifest$snapshot_schema_version) != 1L ||
      !identical(as.integer(manifest$snapshot_schema_version),
                 .SEARCH_SNAPSHOT_VERSION) ||
      !scalar_string(manifest$started_at) ||
      !scalar_string(manifest$finished_at) ||
      !grepl(time_pattern, manifest$started_at) ||
      !grepl(time_pattern, manifest$finished_at) ||
      manifest$finished_at < manifest$started_at ||
      !is.list(manifest$plan) ||
      !identical(names(manifest$plan), .SEARCH_PLAN_FIELDS) ||
      !scalar_string(manifest$plan$query) ||
      !is.list(manifest$plan$sources) ||
      length(manifest$plan$sources) == 0L ||
      !is.character(unlist(manifest$plan$sources, use.names = FALSE)) ||
      !is.list(manifest$counts) ||
      !integer_count(manifest$counts$normalized_articles) ||
      !integer_count(manifest$counts$provider_records) ||
      !is.list(manifest$runtime) ||
      !is.logical(manifest$runtime$complete) ||
      length(manifest$runtime$complete) != 1L ||
      is.na(manifest$runtime$complete)) {
    stop("Archive run manifest has an invalid or unsupported schema",
         call. = FALSE)
  }
  manifest
}

#' List research-archive runs without loading their saved RDS evidence
#'
#' Read the small JSON manifests to inspect run dates, article counts,
#' provider-record counts, and completeness across the archive. Only the
#' manifests are decoded; snapshots and the full SearchHistory are not.
#' Run ordering and plan consistency are validated.
#'
#' With verify_integrity = TRUE (default), checksums for all runs are
#' verified against the RDS and manifest files without deserializing RDS.
#' Set this to FALSE for a faster metadata-only listing. An unchecked run
#' is labeled "unchecked", not "verified"; missing integrity records on
#' protected runs are rejected in either mode. The fast mode does not
#' establish that manifests agree with the contents of the RDS files.
#'
#' Use [read_search_archive()] for comprehensive evidence validation and
#' full chronological history; use [read_search_archive_run()] to load one
#' observation with its checksum and manifest verification.
#'
#' @param directory Existing SearchArchive directory.
#' @param verify_integrity Logical flag. TRUE (default) verifies available
#'   MD5/SHA-256 records and refuses invalid hashes. FALSE checks metadata,
#'   filenames and integrity-policy presence without reading RDS contents.
#'
#' @return A data.frame with index, run, started_at, finished_at, articles,
#'   provider_records, complete, and integrity for each archived observation.
#' @export
#'
#' @examples
#' plan <- new_search_plan("example", sources = "crossref")
#' mock <- function(url, headers, params, timeout) {
#'   list(message = list(items = list()))
#' }
#' path <- tempfile("search-archive-")
#' create_search_archive(run_search_plan(plan, requester = mock), path)
#' list_search_archive_runs(path)
#' unlink(path, recursive = TRUE)
list_search_archive_runs <- function(directory, verify_integrity = TRUE) {
  directory <- .archive_path(directory, exists = TRUE)
  if (!is.logical(verify_integrity) ||
      length(verify_integrity) != 1L || is.na(verify_integrity)) {
    stop("verify_integrity must be TRUE or FALSE", call. = FALSE)
  }
  paths <- .archive_run_folders(directory)
  # The root layout is checked even in the metadata-only mode.
  .archive_read_layout(directory)

  run_names <- basename(paths)
  metadata <- vector("list", length(paths))
  integrity <- character(length(paths))
  for (i in seq_along(paths)) {
    folder <- paths[[i]]
    if (!file.exists(file.path(folder, "snapshot.rds"))) {
      stop("Archive run is missing snapshot.rds", call. = FALSE)
    }
    required <- .archive_integrity_required(directory, run_names[[i]])
    integrity_path <- file.path(folder, .ARCHIVE_INTEGRITY_FILE)
    if (required && !file.exists(integrity_path)) {
      stop("Archive integrity record is missing for a protected run",
           call. = FALSE)
    }
    integrity[[i]] <- if (verify_integrity) {
      .archive_verify_integrity(folder, required = required)
    } else if (!file.exists(integrity_path)) {
      "legacy_unverified"
    } else {
      "unchecked"
    }
    metadata[[i]] <- .archive_manifest_metadata(
      file.path(folder, "manifest.json")
    )
  }

  # Avoid equating searches that changed filtering, page sizes, or policies.
  original_plan <- metadata[[1L]]$plan
  if (!all(vapply(
    metadata, function(item) identical(item$plan, original_plan),
    logical(1L)
  ))) {
    stop("Archive contains different search plans", call. = FALSE)
  }
  started <- vapply(metadata, function(x) x$started_at, character(1L))
  finished <- vapply(metadata, function(x) x$finished_at, character(1L))
  if (is.unsorted(finished, strictly = FALSE)) {
    stop("Archive runs are not ordered by completion time", call. = FALSE)
  }

  data.frame(
    index = seq_along(paths),
    run = run_names,
    started_at = started,
    finished_at = finished,
    articles = vapply(
      metadata, function(x) as.integer(x$counts$normalized_articles),
      integer(1L)
    ),
    provider_records = vapply(
      metadata, function(x) as.integer(x$counts$provider_records),
      integer(1L)
    ),
    complete = vapply(
      metadata, function(x) x$runtime$complete, logical(1L)
    ),
    integrity = integrity,
    stringsAsFactors = FALSE
  )
}

#' Read a single research-archive observation
#'
#' Load and validate one run instead of deserializing the entire archive.
#' The selected run's checksum and manifest are validated as with
#' [read_search_archive()]. Other runs are not checked; use
#' [list_search_archive_runs()] to audit their checksums or
#' [read_search_archive()] to load and validate all evidence.
#'
#' @param directory Existing SearchArchive directory.
#' @param run "latest" (default), a positive one-based integer run index,
#'   or a canonical name such as "run_000002".
#' @return A validated SearchSnapshot for the selected run.
#' @export
#' @examples
#' plan <- new_search_plan("example", sources = "crossref")
#' mock <- function(url, headers, params, timeout) {
#'   list(message = list(items = list()))
#' }
#' path <- tempfile("search-archive-")
#' snapshot <- run_search_plan(plan, requester = mock)
#' create_search_archive(snapshot, path)
#' identical(read_search_archive_run(path), snapshot)
#' unlink(path, recursive = TRUE)
read_search_archive_run <- function(directory, run = "latest") {
  paths <- .archive_run_folders(directory)
  if (is.character(run) && length(run) == 1L &&
      !is.na(run) && identical(run, "latest")) {
    index <- length(paths)
  } else if (is.character(run) && length(run) == 1L &&
             !is.na(run) && grepl("^run_[0-9]{6}$", run)) {
    index <- match(run, basename(paths))
  } else if (is.numeric(run) && length(run) == 1L &&
             !is.na(run) && is.finite(run) &&
             run >= 1 && run == floor(run) &&
             run <= .Machine$integer.max) {
    index <- as.integer(run)
  } else {
    stop("run must be 'latest', a positive integer, or run_NNNNNN",
         call. = FALSE)
  }
  if (is.na(index) || index > length(paths)) {
    stop("Archive run does not exist", call. = FALSE)
  }
  .archive_read_run(paths[[index]])
}
