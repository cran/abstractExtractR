# Append-only collections of search snapshots, managed on demand.
#
# Runs are separate folders so a failed write cannot replace prior research
# evidence. Each completed run is published by renaming a staged directory.

.SEARCH_ARCHIVE_LAYOUT <- 1L
.SEARCH_ARCHIVE_MAX_RUNS <- 999999L

.archive_path <- function(directory, exists = TRUE) {
  directory <- .require_string(directory, "directory")
  if (exists) {
    if (!dir.exists(directory)) {
      stop("Search archive directory does not exist", call. = FALSE)
    }
  } else {
    if (file.exists(directory) || dir.exists(directory)) {
      stop("Search archive directory already exists", call. = FALSE)
    }
    if (!dir.exists(dirname(directory))) {
      stop("Archive parent directory does not exist", call. = FALSE)
    }
  }
  directory
}

.archive_run_name <- function(index) {
  sprintf("run_%06d", as.integer(index))
}

.archive_write_run <- function(snapshot, directory) {
  .check_search_snapshot(snapshot)
  if (!dir.create(directory)) {
    stop("Could not create staged search run", call. = FALSE)
  }
  save_search_snapshot(snapshot, file.path(directory, "snapshot.rds"))
  write_search_manifest(snapshot, file.path(directory, "manifest.json"))
  .archive_write_integrity(directory)
  invisible(directory)
}

.archive_read_run <- function(directory) {
  snapshot_path <- file.path(directory, "snapshot.rds")
  manifest_path <- file.path(directory, "manifest.json")
  if (!file.exists(snapshot_path) || !file.exists(manifest_path)) {
    stop("Archive run is missing a snapshot or manifest", call. = FALSE)
  }
  root <- dirname(dirname(directory))
  .archive_verify_integrity(
    directory,
    required = .archive_integrity_required(root, basename(directory))
  )
  snapshot <- read_search_snapshot(snapshot_path)
  manifest <- tryCatch(
    jsonlite::fromJSON(manifest_path, simplifyVector = FALSE),
    error = function(err) {
      stop("Could not decode the archive run manifest", call. = FALSE)
    }
  )
  if (!is.list(manifest) ||
      !identical(as.integer(manifest$manifest_schema_version),
                 .SEARCH_MANIFEST_VERSION) ||
      !identical(manifest$started_at, snapshot$started_at) ||
      !identical(manifest$finished_at, snapshot$finished_at) ||
      !is.list(manifest$plan) ||
      !identical(manifest$plan$query, snapshot$plan$query) ||
      !identical(
        unname(unlist(manifest$plan$sources, use.names = FALSE)),
        unname(snapshot$plan$sources)
      ) ||
      !identical(
        as.integer(manifest$counts$normalized_articles),
        as.integer(length(snapshot$result$articles))
      )) {
    stop("Archive manifest does not agree with its snapshot", call. = FALSE)
  }
  index <- .archive_decode_doi_index(manifest)
  if (!is.null(index) && !identical(index, .archive_index_records(snapshot))) {
    stop("Archive DOI index does not agree with its snapshot", call. = FALSE)
  }
  snapshot
}

#' Create an append-only research archive from a saved search
#'
#' Initializes a new local research collection with a first SearchSnapshot
#' observation. The archive contains separate run directories with RDS
#' evidence and credential-free JSON manifests, and does not call APIs.
#' Files are staged in a temporary sibling directory before publication.
#'
#' Existing directories are never intentionally overwritten. Archive files
#' are not cryptographically protected against later manual modification.
#' Do not read untrusted RDS files.
#'
#' @param snapshot Valid SearchSnapshot or a trusted RDS snapshot path.
#' @param directory Path to a new archive directory; parent must exist.
#' @return Archive directory path invisibly.
#' @export
#' @examples
#' plan <- new_search_plan("example", sources = "crossref")
#' mock <- function(url, headers, params, timeout) {
#'   list(message = list(items = list()))
#' }
#' snapshot <- run_search_plan(plan, requester = mock)
#' path <- tempfile("research-archive-")
#' create_search_archive(snapshot, path)
#' unlink(path, recursive = TRUE)
create_search_archive <- function(snapshot, directory) {
  if (is.character(snapshot) && length(snapshot) == 1L &&
      !is.na(snapshot)) {
    snapshot <- read_search_snapshot(snapshot)
  }
  .check_search_snapshot(snapshot)
  directory <- .archive_path(directory, exists = FALSE)

  staging <- tempfile(
    pattern = ".abstractExtractR-archive-", tmpdir = dirname(directory)
  )
  if (!dir.create(staging)) {
    stop("Could not create archive staging directory", call. = FALSE)
  }
  on.exit(unlink(staging, recursive = TRUE), add = TRUE)
  runs <- file.path(staging, "runs")
  if (!dir.create(runs)) {
    stop("Could not create archive runs directory", call. = FALSE)
  }
  # Root layout makes missing integrity.json detectable for all new runs.
  .archive_write_layout(staging, required_from = 1L)
  .archive_write_run(
    snapshot, file.path(runs, .archive_run_name(1L))
  )
  writeLines(c(
    "# Literature-search archive",
    "",
    "Each completed runs/run_NNNNNN folder contains snapshot.rds and manifest.json.",
    "Only snapshots from the same SearchPlan may be appended.",
    "Use read_search_archive() for chronological history and provider coverage.",
    "Use refresh_search_archive() for an explicit, on-demand query refresh.",
    "No background scheduling or external file uploads occur.",
    "RDS files preserve article abstracts and provider errors: keep them private.",
    "Archive writes do not provide concurrent-writer locking or cryptographic",
    "integrity guarantees; do not manually modify stored evidence.",
    "Read RDS only from trusted sources."
  ), file.path(staging, "README.md"), useBytes = TRUE)

  if (file.exists(directory) || dir.exists(directory)) {
    stop("Search archive directory already exists", call. = FALSE)
  }
  if (!file.rename(staging, directory)) {
    stop("Could not publish completed search archive", call. = FALSE)
  }
  invisible(directory)
}

#' Inspect an existing append-only research archive
#'
#' Reads each run's trusted RDS evidence and checks the accompanying JSON
#' manifest for matching timestamps, query, sources and article count.
#' Rejects missing or nonsequential run folders and nonchronological results.
#' An archive with at least two runs includes an automatically generated
#' SearchHistory for all successive observations.
#'
#' @param directory Existing research archive directory.
#' @return A SearchArchive containing directory, inventory, snapshots, and
#'   history (NULL when the archive contains only its initial observation).
#' @export
#' @examples
#' plan <- new_search_plan("example", sources = "crossref")
#' mock <- function(url, headers, params, timeout) {
#'   list(message = list(items = list()))
#' }
#' snapshot <- run_search_plan(plan, requester = mock)
#' path <- tempfile("research-archive-")
#' create_search_archive(snapshot, path)
#' read_search_archive(path)$inventory
#' unlink(path, recursive = TRUE)
read_search_archive <- function(directory) {
  directory <- .archive_path(directory, exists = TRUE)
  runs_directory <- file.path(directory, "runs")
  if (!dir.exists(runs_directory)) {
    stop("Search archive runs directory is missing", call. = FALSE)
  }
  run_names <- sort(list.files(runs_directory, all.files = FALSE))
  if (length(run_names) == 0L ||
      length(run_names) > .SEARCH_ARCHIVE_MAX_RUNS ||
      !identical(
        run_names, vapply(seq_along(run_names), .archive_run_name,
                          character(1L), USE.NAMES = FALSE)
      )) {
    stop("Archive run folders must be consecutive, starting at run_000001",
         call. = FALSE)
  }
  folders <- file.path(runs_directory, run_names)
  if (!all(dir.exists(folders))) {
    stop("Archive contains a run entry that is not a directory",
         call. = FALSE)
  }

  snapshots <- lapply(folders, .archive_read_run)
  names(snapshots) <- run_names
  initial_plan <- unclass(snapshots[[1L]]$plan)
  finished <- vapply(
    snapshots, function(s) s$finished_at, character(1L)
  )
  if (!all(vapply(
    snapshots,
    function(s) identical(unclass(s$plan), initial_plan),
    logical(1L)
  ))) {
    stop("Archive contains different search plans", call. = FALSE)
  }
  if (is.unsorted(finished, strictly = FALSE)) {
    stop("Archive runs are not ordered by completion time", call. = FALSE)
  }

  inventory <- data.frame(
    index = seq_along(snapshots),
    run = run_names,
    started_at = vapply(snapshots, function(s) s$started_at, character(1L)),
    finished_at = finished,
    articles = vapply(
      snapshots, function(s) length(s$result$articles), integer(1L)
    ),
    complete = vapply(
      snapshots, function(s) isTRUE(s$context$complete), logical(1L)
    ),
    stringsAsFactors = FALSE
  )
  history <- if (length(snapshots) > 1L) {
    compare_search_history(snapshots, labels = run_names)
  } else {
    NULL
  }
  structure(
    list(
      directory = directory, inventory = inventory,
      snapshots = snapshots, history = history
    ),
    class = "SearchArchive"
  )
}

#' Append an observation to a research archive
#'
#' Appends a SearchSnapshot to a new consecutively numbered run directory.
#' The SearchPlan must match the archived plan, and completed timestamps must
#' be nondecreasing. RDS and JSON evidence are written to a temporary sibling
#' folder before the new run is published. Prior observations are unchanged.
#' All existing runs have their checksums and JSON search-plan metadata
#' inspected, but only the latest archived RDS is deserialized to validate
#' chronology and plan equality. Call [read_search_archive()] to validate
#' every historical RDS and generate the full SearchHistory. Cooperative
#' writer locking is held during the operation; do not force-unlock an
#' active process.
#'
#' @param snapshot New SearchSnapshot or trusted RDS file path.
#' @param directory Existing archive directory.
#' @return New run directory path invisibly.
#' @export
#' @examples
#' plan <- new_search_plan("example", sources = "crossref")
#' mock <- function(url, headers, params, timeout) {
#'   list(message = list(items = list()))
#' }
#' snapshot <- run_search_plan(plan, requester = mock)
#' path <- tempfile("research-archive-")
#' create_search_archive(snapshot, path)
#' append_search_archive(snapshot, path)
#' unlink(path, recursive = TRUE)
append_search_archive <- function(snapshot, directory) {
  if (is.character(snapshot) && length(snapshot) == 1L &&
      !is.na(snapshot)) {
    snapshot <- read_search_snapshot(snapshot)
  }
  .check_search_snapshot(snapshot)
  directory <- .archive_path(directory, exists = TRUE)
  lock <- .archive_acquire_lock(directory)
  on.exit(.archive_release_lock(lock), add = TRUE)
  if (length(.archive_pending_stages(directory)) > 0L) {
    stop("Archive has interrupted staging entries; run recover_search_archive()",
         call. = FALSE)
  }
  .archive_append_locked(snapshot, directory)
}

#' Refresh an archive on demand and append the new search evidence
#'
#' Executes the SearchPlan from the most recent archived observation,
#' using credentials and HTTP request callbacks supplied at invocation time.
#' The new observation is appended only after search and snapshot validation.
#' Provider errors obey the SearchPlan policy. This function does not create
#' timers, recurring jobs, or background processes.
#'
#' @param directory Existing search archive.
#' @param scopus_api_key Optional runtime Scopus API key.
#' @param openalex_api_key Optional runtime OpenAlex API key.
#' @param crossref_mailto Optional runtime contact email for Crossref.
#' @param requester Optional HTTP requester callback.
#' @param sleep Injectable request-spacing function.
#' @param clock Injectable zero-argument time function.
#' @param return_mode "full" (default) preserves the original SearchArchive
#'   result, loading all snapshots and building a full SearchHistory after
#'   the append. "recent" returns a SearchArchiveRefresh containing the
#'   archive inventory, previous and new snapshots, and their two-run
#'   comparison. It does not deserialize older historical observations.
#' @return A complete SearchArchive by default, or SearchArchiveRefresh with
#'   directory, inventory, previous, snapshot and comparison when
#'   return_mode is "recent".
#' @export
#' @examples
#' plan <- new_search_plan("example", sources = "crossref")
#' mock <- function(url, headers, params, timeout) {
#'   list(message = list(items = list()))
#' }
#' snapshot <- run_search_plan(plan, requester = mock)
#' path <- tempfile("research-archive-")
#' create_search_archive(snapshot, path)
#' updated <- refresh_search_archive(path, requester = mock)
#' updated$inventory
#' unlink(path, recursive = TRUE)
refresh_search_archive <- function(
    directory, scopus_api_key = NULL, openalex_api_key = NULL,
    crossref_mailto = NULL, requester = NULL,
    sleep = Sys.sleep, clock = Sys.time,
    return_mode = c("full", "recent")) {
  return_mode <- match.arg(return_mode)
  directory <- .archive_path(directory, exists = TRUE)
  # Refresh obtains the writer lock before the request to prevent two
  # simultaneous refreshes from publishing out-of-order observations.
  lock <- .archive_acquire_lock(directory)
  on.exit(.archive_release_lock(lock), add = TRUE)
  if (length(.archive_pending_stages(directory)) > 0L) {
    stop("Archive has interrupted staging entries; run recover_search_archive()",
         call. = FALSE)
  }
  # Confirm all previous byte checksums and manifest chronology, but load
  # only the final RDS snapshot before rerunning the plan.
  inventory <- list_search_archive_runs(directory)
  latest <- .archive_read_run(file.path(
    directory, "runs", utils::tail(inventory$run, 1L)
  ))
  updated <- run_search_plan(
    plan = latest$plan,
    scopus_api_key = scopus_api_key,
    openalex_api_key = openalex_api_key,
    crossref_mailto = crossref_mailto,
    requester = requester, sleep = sleep, clock = clock
  )
  # The writer lock remains held; do not reacquire it. Reuse the already
  # validated inventory to avoid rescanning historical checksums.
  .archive_append_locked(updated, directory, inventory = inventory)

  if (return_mode == "full") {
    return(read_search_archive(directory))
  }

  # The existing inventory was checksum-verified before appending, while
  # the new run is validated by the single-run reader below.
  current <- read_search_archive_run(directory, run = "latest")
  updated_inventory <- list_search_archive_runs(
    directory, verify_integrity = FALSE
  )
  structure(
    list(
      directory = directory,
      inventory = updated_inventory,
      previous = latest,
      snapshot = current,
      comparison = compare_search_snapshots(latest, current)
    ),
    class = "SearchArchiveRefresh"
  )
}

.archive_append_locked <- function(snapshot, directory, inventory = NULL) {
  # All previous integrity checksums and JSON manifests are checked, but
  # historical RDS objects are not deserialized. A full read remains
  # available for comprehensive archival verification.
  if (is.null(inventory)) {
    inventory <- list_search_archive_runs(directory)
  }
  if (!is.data.frame(inventory) || nrow(inventory) == 0L ||
      !all(c("run", "finished_at") %in% names(inventory))) {
    stop("Archive inventory is invalid", call. = FALSE)
  }
  last <- .archive_read_run(file.path(
    directory, "runs", utils::tail(inventory$run, 1L)
  ))
  if (!identical(unclass(last$plan), unclass(snapshot$plan))) {
    stop("Cannot append a snapshot from a different search plan",
         call. = FALSE)
  }
  if (snapshot$finished_at < last$finished_at) {
    stop("Snapshot completed before the last archived observation",
         call. = FALSE)
  }

  index <- nrow(inventory) + 1L
  if (index > .SEARCH_ARCHIVE_MAX_RUNS) {
    stop("Search archive has reached its maximum run count",
         call. = FALSE)
  }
  runs_directory <- file.path(directory, "runs")
  destination <- file.path(runs_directory, .archive_run_name(index))
  if (file.exists(destination) || dir.exists(destination)) {
    stop("Archive destination run already exists", call. = FALSE)
  }
  # An older archive has no policy marker. Publish one before writing the
  # first v2 run so a crash cannot leave a protected run without a policy.
  if (is.null(.archive_read_layout(directory))) {
    .archive_write_layout(directory, required_from = index)
  }
  staging <- tempfile(
    pattern = ".abstractExtractR-run-", tmpdir = runs_directory
  )
  .archive_write_run(snapshot, staging)
  on.exit(unlink(staging, recursive = TRUE), add = TRUE)
  if (file.exists(destination) || dir.exists(destination)) {
    stop("Archive destination run already exists", call. = FALSE)
  }
  if (!file.rename(staging, destination)) {
    stop("Could not publish archived search run", call. = FALSE)
  }
  invisible(destination)
}
