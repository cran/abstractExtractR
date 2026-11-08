# Compare chosen archive runs without loading unrelated RDS evidence.

.archive_resolve_run <- function(paths, run) {
  if (is.character(run) && length(run) == 1L &&
      !is.na(run) && identical(run, "latest")) {
    return(length(paths))
  }
  if (is.character(run) && length(run) == 1L &&
      !is.na(run) && grepl("^run_[0-9]{6}$", run)) {
    index <- match(run, basename(paths))
  } else if (is.numeric(run) && length(run) == 1L &&
             !is.na(run) && is.finite(run) &&
             run == floor(run) && run >= 1 &&
             run <= .Machine$integer.max) {
    index <- as.integer(run)
  } else {
    stop("run must be 'latest', a positive integer, or run_NNNNNN",
         call. = FALSE)
  }
  if (is.na(index) || index > length(paths)) {
    stop("Archive run does not exist", call. = FALSE)
  }
  index
}

#' Compare two selected archive observations
#'
#' Load and validate exactly the two requested observations, then perform
#' the existing DOI-based comparison. Other archive RDS objects are not
#' deserialized or checked. Use [read_search_archive()] for a full audit.
#'
#' @param directory Existing research archive path.
#' @param before One-based index or run_NNNNNN name of the earlier run.
#' @param after One-based index, run_NNNNNN name, or "latest".
#' @return The result of [compare_search_snapshots()].
#' @export
#' @examples
#' plan <- new_search_plan("topic", sources = "crossref")
#' mock <- function(url, headers, params, timeout) {
#'   list(message = list(items = list()))
#' }
#' snap <- run_search_plan(plan, requester = mock)
#' path <- tempfile("archive-")
#' create_search_archive(snap, path)
#' append_search_archive(snap, path)
#' compare_search_archive_runs(path, 1L, "latest")$summary
#' unlink(path, recursive = TRUE)
compare_search_archive_runs <- function(directory, before, after = "latest") {
  paths <- .archive_run_folders(directory)
  first <- .archive_resolve_run(paths, before)
  second <- .archive_resolve_run(paths, after)
  if (first >= second) {
    stop("before must identify a run earlier than after", call. = FALSE)
  }
  compare_search_snapshots(
    .archive_read_run(paths[[first]]),
    .archive_read_run(paths[[second]])
  )
}

#' Compare only the most recent archive observations
#'
#' Load a bounded tail of completed runs and compare consecutive observations.
#' This does not load or checksum earlier runs and is not a full archive audit.
#'
#' @param directory Existing research archive.
#' @param last_n Number of final runs to compare, at least two and no larger
#'   than the current archive length.
#' @return A SearchHistory as produced by [compare_search_history()].
#' @export
#' @examples
#' plan <- new_search_plan("topic", sources = "crossref")
#' mock <- function(url, headers, params, timeout) {
#'   list(message = list(items = list()))
#' }
#' snap <- run_search_plan(plan, requester = mock)
#' path <- tempfile("archive-")
#' create_search_archive(snap, path)
#' append_search_archive(snap, path)
#' compare_recent_archive_runs(path, last_n = 2L)$transitions
#' unlink(path, recursive = TRUE)
compare_recent_archive_runs <- function(directory, last_n = 2L) {
  paths <- .archive_run_folders(directory)
  if (!is.numeric(last_n) || length(last_n) != 1L ||
      is.na(last_n) || !is.finite(last_n) ||
      last_n != floor(last_n) || last_n < 2L ||
      last_n > length(paths)) {
    stop("last_n must be an integer between 2 and archive length",
         call. = FALSE)
  }
  positions <- utils::tail(seq_along(paths), as.integer(last_n))
  chosen <- paths[positions]
  snapshots <- lapply(chosen, .archive_read_run)
  names(snapshots) <- basename(chosen)
  compare_search_history(snapshots, labels = names(snapshots))
}
