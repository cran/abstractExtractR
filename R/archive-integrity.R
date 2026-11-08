# Versioned file integrity and cooperative local writer coordination.
#
# SHA-256 detects accidental changes and resists hash collisions, but adjacent
# metadata is not signed. It cannot establish archival authenticity.

.ARCHIVE_INTEGRITY_VERSION <- 2L
.ARCHIVE_INTEGRITY_FILE <- "integrity.json"
.ARCHIVE_LAYOUT_VERSION <- 2L
.ARCHIVE_LAYOUT_FILE <- "archive-layout.json"
.ARCHIVE_LOCK_NAME <- ".abstractExtractR.lock"
.ARCHIVE_STAGE_PREFIX <- ".abstractExtractR-run-"

.archive_digest <- function(path, algorithm = "sha256") {
  if (!file.exists(path) || dir.exists(path)) {
    stop("Archive checksum input is not a regular file", call. = FALSE)
  }
  if (identical(algorithm, "md5")) {
    digest <- unname(tools::md5sum(path))
    if (!is.character(digest) || length(digest) != 1L ||
        is.na(digest) || !grepl("^[0-9a-fA-F]{32}$", digest)) {
      stop("Unable to calculate MD5 checksum", call. = FALSE)
    }
    return(tolower(digest))
  }
  if (!identical(algorithm, "sha256")) {
    stop("Unsupported archive checksum algorithm", call. = FALSE)
  }

  # Stream large RDS files without copying them into memory.
  connection <- base::file(path, open = "rb")
  on.exit(close(connection), add = TRUE)
  digest <- openssl::sha256(connection)
  if (!is.raw(digest) || length(digest) != 32L) {
    stop("Unable to calculate SHA-256 checksum", call. = FALSE)
  }
  paste(sprintf("%02x", as.integer(digest)), collapse = "")
}

.archive_valid_hash <- function(value, size) {
  is.character(value) && length(value) == 1L &&
    !is.na(value) && grepl(
      sprintf("^[0-9a-fA-F]{%d}$", size), value, perl = TRUE
    )
}

.archive_write_integrity <- function(directory) {
  value <- list(
    schema_version = .ARCHIVE_INTEGRITY_VERSION,
    algorithm = "sha256",
    snapshot_sha256 = .archive_digest(
      file.path(directory, "snapshot.rds"), "sha256"
    ),
    manifest_sha256 = .archive_digest(
      file.path(directory, "manifest.json"), "sha256"
    )
  )
  jsonlite::write_json(
    value, file.path(directory, .ARCHIVE_INTEGRITY_FILE),
    auto_unbox = TRUE, pretty = TRUE
  )
  invisible(NULL)
}

.archive_read_layout <- function(directory) {
  path <- file.path(directory, .ARCHIVE_LAYOUT_FILE)
  if (!file.exists(path)) {
    # Earlier versions had no root integrity policy.
    return(NULL)
  }
  info <- tryCatch(
    jsonlite::fromJSON(path, simplifyVector = FALSE),
    error = function(err) {
      stop("Archive layout metadata cannot be decoded", call. = FALSE)
    }
  )
  integer_scalar <- function(x) {
    is.numeric(x) && length(x) == 1L &&
      !is.na(x) && is.finite(x) && x == floor(x)
  }
  if (!is.list(info) ||
      !integer_scalar(info$schema_version) ||
      info$schema_version != .ARCHIVE_LAYOUT_VERSION ||
      !identical(info$algorithm, "sha256") ||
      !integer_scalar(info$integrity_required_from_run) ||
      info$integrity_required_from_run < 1L ||
      info$integrity_required_from_run > .SEARCH_ARCHIVE_MAX_RUNS) {
    stop("Archive layout has an unsupported integrity policy",
         call. = FALSE)
  }
  as.integer(info$integrity_required_from_run)
}

.archive_write_layout <- function(directory, required_from) {
  if (!is.numeric(required_from) || length(required_from) != 1L ||
      is.na(required_from) || !is.finite(required_from) ||
      required_from != floor(required_from) || required_from < 1 ||
      required_from > .SEARCH_ARCHIVE_MAX_RUNS) {
    stop("Invalid first archive run requiring SHA-256", call. = FALSE)
  }
  target <- file.path(directory, .ARCHIVE_LAYOUT_FILE)
  if (file.exists(target)) {
    stop("Archive integrity policy already exists", call. = FALSE)
  }
  staging <- tempfile(".abstractExtractR-layout-", tmpdir = directory)
  on.exit(unlink(staging), add = TRUE)
  jsonlite::write_json(
    list(
      schema_version = .ARCHIVE_LAYOUT_VERSION,
      algorithm = "sha256",
      integrity_required_from_run = as.integer(required_from)
    ),
    staging, auto_unbox = TRUE, pretty = TRUE
  )
  if (file.exists(target) || !file.rename(staging, target)) {
    stop("Could not publish archive integrity policy", call. = FALSE)
  }
  invisible(NULL)
}

.archive_integrity_required <- function(archive, run) {
  threshold <- .archive_read_layout(archive)
  if (is.null(threshold)) return(FALSE)
  if (!grepl("^run_[0-9]{6}$", run)) {
    stop("Invalid archive run name", call. = FALSE)
  }
  index <- as.integer(sub("^run_", "", run))
  index >= threshold
}

.archive_verify_integrity <- function(directory, required = FALSE) {
  snapshot <- file.path(directory, "snapshot.rds")
  manifest <- file.path(directory, "manifest.json")
  if (!file.exists(snapshot) || !file.exists(manifest)) {
    stop("Archive run is missing snapshot.rds or manifest.json",
         call. = FALSE)
  }
  path <- file.path(directory, .ARCHIVE_INTEGRITY_FILE)
  if (!file.exists(path)) {
    if (required) {
      stop("Archive integrity record is missing for a protected run",
           call. = FALSE)
    }
    return("legacy_unverified")
  }
  integrity <- tryCatch(
    jsonlite::fromJSON(path, simplifyVector = FALSE),
    error = function(err) {
      stop("Archive integrity metadata cannot be decoded", call. = FALSE)
    }
  )
  if (!is.list(integrity) ||
      !is.numeric(integrity$schema_version) ||
      length(integrity$schema_version) != 1L ||
      is.na(integrity$schema_version)) {
    stop("Archive integrity metadata has an unsupported schema",
         call. = FALSE)
  }
  version <- integrity$schema_version
  if (version == 1L && identical(integrity$algorithm, "md5")) {
    # Prior runs remain valid without rewriting their evidence.
    if (required || !.archive_valid_hash(integrity$snapshot_md5, 32L) ||
        !.archive_valid_hash(integrity$manifest_md5, 32L)) {
      stop("Archive integrity metadata has an unsupported schema",
           call. = FALSE)
    }
    if (!identical(tolower(integrity$snapshot_md5),
                   .archive_digest(snapshot, "md5"))) {
      stop("Archive checksum mismatch: snapshot.rds", call. = FALSE)
    }
    if (!identical(tolower(integrity$manifest_md5),
                   .archive_digest(manifest, "md5"))) {
      stop("Archive checksum mismatch: manifest.json", call. = FALSE)
    }
    return("verified_md5")
  }
  if (version == 2L && identical(integrity$algorithm, "sha256")) {
    if (!.archive_valid_hash(integrity$snapshot_sha256, 64L) ||
        !.archive_valid_hash(integrity$manifest_sha256, 64L)) {
      stop("Archive integrity metadata has an unsupported schema",
           call. = FALSE)
    }
    if (!identical(tolower(integrity$snapshot_sha256),
                   .archive_digest(snapshot, "sha256"))) {
      stop("Archive checksum mismatch: snapshot.rds", call. = FALSE)
    }
    if (!identical(tolower(integrity$manifest_sha256),
                   .archive_digest(manifest, "sha256"))) {
      stop("Archive checksum mismatch: manifest.json", call. = FALSE)
    }
    return("verified_sha256")
  }
  stop("Archive integrity metadata has an unsupported schema",
       call. = FALSE)
}

.archive_pending_stages <- function(directory) {
  runs <- file.path(directory, "runs")
  if (!dir.exists(runs)) {
    stop("Search archive runs directory is missing", call. = FALSE)
  }
  names <- list.files(runs, all.files = TRUE, no.. = TRUE)
  names <- names[startsWith(names, .ARCHIVE_STAGE_PREFIX)]
  file.path(runs, sort(names))
}

.archive_lock_path <- function(directory) {
  file.path(directory, .ARCHIVE_LOCK_NAME)
}

.archive_acquire_lock <- function(directory) {
  lock_path <- .archive_lock_path(directory)
  # Atomic directory creation coordinates writers on ordinary local disks.
  if (!dir.create(lock_path, showWarnings = FALSE)) {
    stop("Search archive is locked; another writer may be active",
         call. = FALSE)
  }
  token <- basename(tempfile(pattern = "archive-writer-"))
  success <- tryCatch({
    writeLines(token, file.path(lock_path, "owner.txt"), useBytes = TRUE)
    TRUE
  }, error = function(err) FALSE)
  if (!success) {
    unlink(lock_path, recursive = TRUE)
    stop("Could not initialize archive writer lock", call. = FALSE)
  }
  list(path = lock_path, token = token)
}

.archive_release_lock <- function(lock) {
  if (!dir.exists(lock$path)) {
    warning("Archive lock disappeared before release", call. = FALSE)
    return(invisible(FALSE))
  }
  file <- file.path(lock$path, "owner.txt")
  current <- tryCatch(
    readLines(file, n = 1L, warn = FALSE),
    error = function(err) character()
  )
  if (!identical(current, lock$token)) {
    warning("Archive lock ownership changed; lock was not deleted",
            call. = FALSE)
    return(invisible(FALSE))
  }
  unlink(lock$path, recursive = TRUE)
  invisible(TRUE)
}

#' Inspect checksums and interrupted writes in a research archive
#'
#' Inspects every run without deserializing RDS. New runs are verified with
#' SHA-256, while older MD5 runs remain readable and are labeled verified_md5.
#' Pre-checksum archives receive legacy_unverified status. A protected run
#' that loses its integrity.json is reported as invalid, not as legacy.
#'
#' The archive root policy is local and unsigned: checksums do not establish
#' authenticity against deliberate manipulation of both data and metadata.
#'
#' @param directory Existing local archive directory.
#' @return List of runs (run, integrity, issue), pending staging paths,
#'   and a logical locked flag.
#' @export
#' @examples
#' plan <- new_search_plan("example", sources = "crossref")
#' mock <- function(url, headers, params, timeout) {
#'   list(message = list(items = list()))
#' }
#' path <- tempfile("archive-audit-")
#' create_search_archive(run_search_plan(plan, requester = mock), path)
#' verify_search_archive(path)$runs
#' unlink(path, recursive = TRUE)
verify_search_archive <- function(directory) {
  directory <- .archive_path(directory, exists = TRUE)
  runs <- file.path(directory, "runs")
  if (!dir.exists(runs)) {
    stop("Search archive runs directory is missing", call. = FALSE)
  }
  # Validate root layout even if there are no completed runs.
  .archive_read_layout(directory)
  names <- sort(list.files(runs, all.files = FALSE))
  result <- lapply(names, function(name) {
    path <- file.path(runs, name)
    verification <- tryCatch(
      list(
        integrity = .archive_verify_integrity(
          path, required = .archive_integrity_required(directory, name)
        ),
        issue = NA_character_
      ),
      error = function(err) list(
        integrity = "invalid",
        issue = conditionMessage(err)
      )
    )
    data.frame(
      run = name, integrity = verification$integrity,
      issue = verification$issue, stringsAsFactors = FALSE
    )
  })
  empty <- data.frame(
    run = character(), integrity = character(),
    issue = character(), stringsAsFactors = FALSE
  )
  report <- if (length(result) == 0L) empty else do.call(rbind, result)
  rownames(report) <- NULL
  list(
    runs = report,
    pending = .archive_pending_stages(directory),
    locked = dir.exists(.archive_lock_path(directory))
  )
}

#' Inspect and explicitly clean up interrupted archive staging directories
#'
#' By default no files are changed. With dry_run = FALSE, remove only hidden
#' staging directories created by this package. If a writer lock remains,
#' deletion is refused unless force_unlock = TRUE; that option is dangerous
#' if any writer is still active.
#'
#' All staging paths are validated before any lock removal or deletion.
#' Completed archive runs are never removed. Suspected checksum corruption
#' must be investigated using a trusted backup instead of this function.
#'
#' @param directory Existing search archive directory.
#' @param dry_run TRUE (default) for inspection only.
#' @param force_unlock Explicitly remove a writer lock when dry_run is FALSE.
#' @return List with pending, locked, removed, and unlocked fields.
#' @export
#' @examples
#' plan <- new_search_plan("example", sources = "crossref")
#' mock <- function(url, headers, params, timeout) {
#'   list(message = list(items = list()))
#' }
#' path <- tempfile("archive-recovery-")
#' create_search_archive(run_search_plan(plan, requester = mock), path)
#' recover_search_archive(path)$pending
#' unlink(path, recursive = TRUE)
recover_search_archive <- function(
    directory, dry_run = TRUE, force_unlock = FALSE) {
  directory <- .archive_path(directory, exists = TRUE)
  for (flag in c("dry_run", "force_unlock")) {
    value <- if (flag == "dry_run") dry_run else force_unlock
    if (!is.logical(value) || length(value) != 1L || is.na(value)) {
      stop(sprintf("%s must be TRUE or FALSE", flag), call. = FALSE)
    }
  }
  stages <- .archive_pending_stages(directory)
  locked <- dir.exists(.archive_lock_path(directory))
  report <- list(
    pending = stages, locked = locked,
    removed = character(), unlocked = FALSE
  )
  if (dry_run) return(report)
  if (locked && !force_unlock) {
    stop("Search archive is locked; investigate before recovery",
         call. = FALSE)
  }
  # Preflight all stage entries before performing any destructive action.
  for (path in stages) {
    linked <- Sys.readlink(path)
    if (!is.na(linked) && nzchar(linked)) {
      stop("Refusing to recover symbolic-link staging entry", call. = FALSE)
    }
    if (!dir.exists(path)) {
      stop("Refusing to recover non-directory staging entry",
           call. = FALSE)
    }
  }
  if (locked) {
    lock <- .archive_lock_path(directory)
    unlink(lock, recursive = TRUE)
    if (dir.exists(lock)) {
      stop("Could not remove archive writer lock", call. = FALSE)
    }
    report$unlocked <- TRUE
  }
  for (path in stages) {
    unlink(path, recursive = TRUE)
    if (dir.exists(path)) {
      stop("Could not remove interrupted archive staging directory",
           call. = FALSE)
    }
    report$removed <- c(report$removed, path)
  }
  report
}
