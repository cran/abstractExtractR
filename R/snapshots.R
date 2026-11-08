# Versioned, credential-free snapshots of a multi-provider literature search.
#
# A SearchPlan contains only reproducible search settings. Runtime credentials,
# request callbacks, and sleep/clock functions are supplied during execution
# and are never members of the plan or snapshot object.

.SEARCH_SNAPSHOT_VERSION <- 1L
.SEARCH_PLAN_FIELDS <- c(
  "query", "sources", "max_results_per_source", "page_size",
  "start_date", "end_date", "crossref_work_type",
  "openalex_work_type", "scopus_author", "on_error",
  "on_conflict", "timeout", "max_tries", "min_interval"
)

#' Create a reproducible multi-provider search plan
#'
#' Capture the arguments of [search_articles()] without any credentials or
#' executable request callbacks. A plan can be reused to run a new search;
#' results are retrieved only when [run_search_plan()] is called.
#'
#' @param query Nonempty scholarly search query.
#' @param sources Unique provider names, in precedence order.
#' @param max_results_per_source Maximum results per provider (1 to 500).
#' @param page_size Maximum results per page (1 to 25).
#' @param start_date,end_date Optional YYYY-MM-DD publication date limits.
#' @param crossref_work_type Optional Crossref work type.
#' @param openalex_work_type Optional OpenAlex work type.
#' @param scopus_author Optional Scopus author expression.
#' @param on_error Provider failure policy: stop, warn, or skip.
#' @param on_conflict Reconciliation policy: keep_first, warn, or error.
#' @param timeout HTTP timeout in seconds, positive and finite.
#' @param max_tries Retry attempts for the built-in transport (1 to 10).
#' @param min_interval Nonnegative request spacing in seconds.
#'
#' @return A validated SearchPlan list containing no credentials.
#' @export
#'
#' @examples
#' plan <- new_search_plan("causal inference", sources = "crossref")
#' plan$query
new_search_plan <- function(
    query, sources = c("crossref", "openalex"),
    max_results_per_source = 25L, page_size = 25L,
    start_date = NULL, end_date = NULL,
    crossref_work_type = NULL, openalex_work_type = NULL,
    scopus_author = NULL, on_error = c("stop", "warn", "skip"),
    on_conflict = c("keep_first", "warn", "error"),
    timeout = .DEFAULT_TIMEOUT, max_tries = 3L, min_interval = 0.25) {
  query <- .require_string(query, "query")
  choices <- c("crossref", "openalex", "scopus")
  if (!is.character(sources) || length(sources) == 0L ||
      anyNA(sources) || anyDuplicated(sources) ||
      !all(sources %in% choices)) {
    stop("sources must be unique names from crossref, openalex, scopus",
         call. = FALSE)
  }
  max_results_per_source <- .validate_crossref_count(
    max_results_per_source, "max_results_per_source", 500L
  )
  page_size <- .validate_crossref_count(page_size, "page_size", 25L)
  max_tries <- .validate_crossref_count(max_tries, "max_tries", 10L)
  start_date <- .validate_crossref_date(start_date, "start_date")
  end_date <- .validate_crossref_date(end_date, "end_date")
  if (!is.null(start_date) && !is.null(end_date) &&
      start_date > end_date) {
    stop("start_date must not be after end_date", call. = FALSE)
  }
  if ("scopus" %in% sources && xor(is.null(start_date), is.null(end_date))) {
    stop("Scopus searches require both start_date and end_date",
         call. = FALSE)
  }
  check_type <- function(value, field) {
    if (is.null(value)) return(NULL)
    value <- .require_string(value, field)
    if (!grepl("^[a-z][a-z0-9-]*$", value)) {
      stop(sprintf("%s must be a lower-case type identifier", field),
           call. = FALSE)
    }
    value
  }
  crossref_work_type <- check_type(crossref_work_type, "crossref_work_type")
  openalex_work_type <- check_type(openalex_work_type, "openalex_work_type")
  if (!is.null(scopus_author)) {
    scopus_author <- .require_string(scopus_author, "scopus_author")
  }
  .validate_timeout(timeout)
  if (!is.numeric(min_interval) || length(min_interval) != 1L ||
      is.na(min_interval) || !is.finite(min_interval) || min_interval < 0) {
    stop("min_interval must be finite and nonnegative", call. = FALSE)
  }

  structure(
    list(
      query = query,
      sources = sources,
      max_results_per_source = max_results_per_source,
      page_size = page_size,
      start_date = start_date,
      end_date = end_date,
      crossref_work_type = crossref_work_type,
      openalex_work_type = openalex_work_type,
      scopus_author = scopus_author,
      on_error = match.arg(on_error),
      on_conflict = match.arg(on_conflict),
      timeout = as.numeric(timeout),
      max_tries = max_tries,
      min_interval = as.numeric(min_interval)
    ),
    class = "SearchPlan"
  )
}

.check_search_plan <- function(plan) {
  if (!inherits(plan, "SearchPlan") ||
      !is.list(plan) ||
      !identical(names(plan), .SEARCH_PLAN_FIELDS)) {
    stop("plan must be a SearchPlan from new_search_plan()",
         call. = FALSE)
  }
  validated <- do.call(new_search_plan, unclass(plan))
  if (!identical(unclass(validated), unclass(plan))) {
    stop("plan fields have changed after validation", call. = FALSE)
  }
  invisible(NULL)
}

.snapshot_utc <- function(clock) {
  if (!is.function(clock)) {
    stop("clock must be a function", call. = FALSE)
  }
  value <- clock()
  if (!inherits(value, "POSIXt") || length(value) != 1L ||
      is.na(value) || !is.finite(as.numeric(value))) {
    stop("clock must return one finite POSIXct/POSIXlt timestamp",
         call. = FALSE)
  }
  format(value, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

.redact_snapshot_errors <- function(result, secrets) {
  if (length(secrets) == 0L) return(result)
  for (secret in secrets) {
    if (!is.character(secret) || length(secret) != 1L ||
        is.na(secret) || !nzchar(secret)) next
    # Source errors can originate from user-defined request callbacks.
    result$errors$message <- gsub(
      secret, "[redacted]", result$errors$message, fixed = TRUE
    )
    result$sources$error_message <- gsub(
      secret, "[redacted]", result$sources$error_message, fixed = TRUE
    )
  }
  result
}

#' Execute a search plan and capture a versioned evidence snapshot
#'
#' Executes [search_articles()] using a validated SearchPlan and saves the
#' resulting normalized articles, provider counts and errors, DOI provenance
#' and field-level conflicts. Records UTC start/end timestamps and runtime
#' version information. Credentials are accepted at execution time only:
#' values of API keys and Crossref contact addresses are not saved.
#'
#' A snapshot is an observation of one search run, not a claim that a provider
#' will return the same results later. Use [save_search_snapshot()] to persist
#' the complete result for reproducible downstream analysis.
#'
#' @param plan A SearchPlan from [new_search_plan()].
#' @param scopus_api_key Optional Elsevier API key.
#' @param openalex_api_key Optional OpenAlex API key.
#' @param crossref_mailto Optional Crossref contact address.
#' @param requester Optional shared HTTP callback for offline tests.
#' @param sleep Optional delay function, by default [Sys.sleep()].
#' @param clock Zero-argument function returning the current time, by default
#'   [Sys.time()]. Injectable for deterministic tests.
#'
#' @return A SearchSnapshot with schema_version, plan, started_at,
#'   finished_at, context (runtime information and completion status),
#'   and result (the five [search_articles()] output tables/objects).
#' @export
#'
#' @examples
#' plan <- new_search_plan("forecasting", sources = "crossref")
#' mock <- function(url, headers, params, timeout) {
#'   list(message = list(items = list(
#'     list(title = list("Example"), DOI = "10.1000/example")
#'   )))
#' }
#' snap <- run_search_plan(plan, requester = mock, sleep = function(...) NULL)
#' snap$result$sources
run_search_plan <- function(
    plan, scopus_api_key = NULL, openalex_api_key = NULL,
    crossref_mailto = NULL, requester = NULL,
    sleep = Sys.sleep, clock = Sys.time) {
  .check_search_plan(plan)
  .validate_crossref_mailto(crossref_mailto)
  if (!is.null(scopus_api_key)) {
    scopus_api_key <- .require_string(scopus_api_key, "scopus_api_key")
  }
  if (!is.null(openalex_api_key)) {
    openalex_api_key <- .require_string(openalex_api_key, "openalex_api_key")
  }
  if (!is.null(requester)) .validate_requester(requester)
  if (!is.function(sleep)) stop("sleep must be a function", call. = FALSE)
  if (!is.function(clock)) stop("clock must be a function", call. = FALSE)

  start <- .snapshot_utc(clock)
  arguments <- c(
    unclass(plan),
    list(
      scopus_api_key = scopus_api_key,
      openalex_api_key = openalex_api_key,
      crossref_mailto = crossref_mailto,
      requester = requester,
      sleep = sleep
    )
  )
  result <- do.call(search_articles, arguments)
  end <- .snapshot_utc(clock)

  result <- .redact_snapshot_errors(
    result, c(scopus_api_key, openalex_api_key, crossref_mailto)
  )
  package_version <- tryCatch(
    as.character(utils::packageVersion("abstractExtractR")),
    error = function(err) NA_character_
  )
  complete <- all(result$sources$status == "ok")

  structure(
    list(
      schema_version = .SEARCH_SNAPSHOT_VERSION,
      plan = plan,
      started_at = start,
      finished_at = end,
      context = list(
        package_version = package_version,
        R_version = as.character(getRversion()),
        platform = R.version$platform,
        scopus_key_provided = !is.null(scopus_api_key),
        openalex_key_provided = !is.null(openalex_api_key),
        crossref_contact_provided = !is.null(crossref_mailto),
        complete = complete
      ),
      result = result
    ),
    class = "SearchSnapshot"
  )
}

.check_search_snapshot <- function(snapshot) {
  fields <- c(
    "schema_version", "plan", "started_at", "finished_at",
    "context", "result"
  )
  if (!inherits(snapshot, "SearchSnapshot") ||
      !is.list(snapshot) || !identical(names(snapshot), fields)) {
    stop("Expected a versioned SearchSnapshot object", call. = FALSE)
  }
  if (!identical(snapshot$schema_version, .SEARCH_SNAPSHOT_VERSION)) {
    stop("Unsupported search snapshot schema_version", call. = FALSE)
  }
  .check_search_plan(snapshot$plan)
  for (name in c("started_at", "finished_at")) {
    value <- snapshot[[name]]
    if (!is.character(value) || length(value) != 1L || is.na(value) ||
        !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$",
               value)) {
      stop(sprintf("Invalid snapshot %s", name), call. = FALSE)
    }
  }
  if (snapshot$finished_at < snapshot$started_at) {
    stop("Snapshot finished_at precedes started_at", call. = FALSE)
  }
  info <- snapshot$context
  if (!is.list(info) ||
      !all(c("package_version", "R_version", "platform",
             "scopus_key_provided", "openalex_key_provided",
             "crossref_contact_provided", "complete") %in% names(info))) {
    stop("Invalid snapshot context", call. = FALSE)
  }
  for (name in c("package_version", "R_version", "platform")) {
    value <- info[[name]]
    if (!is.character(value) || length(value) != 1L ||
        (name != "package_version" && is.na(value))) {
      stop(sprintf("Invalid snapshot context value %s", name),
           call. = FALSE)
    }
  }
  for (name in c("scopus_key_provided", "openalex_key_provided",
                 "crossref_contact_provided", "complete")) {
    value <- info[[name]]
    if (!is.logical(value) || length(value) != 1L || is.na(value)) {
      stop(sprintf("Invalid snapshot context flag %s", name),
           call. = FALSE)
    }
  }
  result <- snapshot$result
  if (!is.list(result) ||
      !all(c("articles", "provenance", "conflicts", "sources", "errors") %in%
           names(result))) {
    stop("Snapshot result is missing search evidence", call. = FALSE)
  }
  if (!is.list(result$articles) ||
      !all(vapply(result$articles, inherits, logical(1L), what = "Article"))) {
    stop("Snapshot articles are not valid Article records", call. = FALSE)
  }
  for (name in c("provenance", "conflicts", "sources", "errors")) {
    if (!is.data.frame(result[[name]])) {
      stop(sprintf("Snapshot %s must be a data.frame", name),
           call. = FALSE)
    }
  }
  invisible(NULL)
}

#' Persist a search snapshot without overwriting existing evidence
#'
#' Writes the full SearchSnapshot to an RDS file, using a temporary file in
#' the same directory before renaming. Existing files are never intentionally
#' overwritten. RDS preserves Article classes and empty table schemas.
#' No network calls or additional R packages are required.
#'
#' @param snapshot A valid SearchSnapshot from [run_search_plan()].
#' @param file Nonempty path to a new RDS file. The parent directory must
#'   already exist. Existing files cause an error.
#'
#' @return The file path invisibly.
#' @export
#'
#' @examples
#' plan <- new_search_plan("test", sources = "crossref")
#' mock <- function(url, headers, params, timeout) {
#'   list(message = list(items = list()))
#' }
#' snap <- run_search_plan(plan, requester = mock)
#' path <- tempfile(fileext = ".rds")
#' save_search_snapshot(snap, path)
#' unlink(path)
save_search_snapshot <- function(snapshot, file) {
  .check_search_snapshot(snapshot)
  file <- .require_string(file, "file")
  directory <- dirname(file)
  if (!dir.exists(directory)) {
    stop("Parent directory for snapshot does not exist", call. = FALSE)
  }
  if (file.exists(file)) {
    stop("Snapshot file already exists; choose a new path", call. = FALSE)
  }

  temporary <- tempfile(pattern = ".abstractExtractR-", tmpdir = directory,
                        fileext = ".rds")
  on.exit(unlink(temporary), add = TRUE)
  saveRDS(snapshot, file = temporary, version = 3L, compress = "gzip")
  if (!file.rename(temporary, file)) {
    stop("Could not move completed snapshot into place", call. = FALSE)
  }
  invisible(file)
}

#' Read and validate a previously saved search snapshot
#'
#' Reloads an RDS search snapshot, validating its schema version and evidence
#' structure. Use only snapshot files from trusted sources. RDS is an R-native
#' serialized format, not a safe interchange format for untrusted files.
#'
#' @param file Path to an existing RDS search snapshot.
#' @return A validated SearchSnapshot preserving articles and provenance.
#' @export
#'
#' @examples
#' plan <- new_search_plan("test", sources = "crossref")
#' mock <- function(url, headers, params, timeout) {
#'   list(message = list(items = list()))
#' }
#' snap <- run_search_plan(plan, requester = mock)
#' path <- tempfile(fileext = ".rds")
#' save_search_snapshot(snap, path)
#' restored <- read_search_snapshot(path)
#' unlink(path)
#' restored$context$complete
read_search_snapshot <- function(file) {
  file <- .require_string(file, "file")
  if (!file.exists(file)) {
    stop("Search snapshot file does not exist", call. = FALSE)
  }
  snapshot <- tryCatch(
    readRDS(file),
    error = function(err) {
      stop(sprintf("Could not read search snapshot: %s",
                   conditionMessage(err)), call. = FALSE)
    }
  )
  .check_search_snapshot(snapshot)
  snapshot
}
