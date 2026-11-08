# Chronological evidence histories from already saved scholarly searches.
#
# No API requests are made. Every transition is derived from the existing
# canonical-DOI comparison, with its comparability limitations preserved.

.SEARCH_HISTORY_VERSION <- 1L

.history_bind <- function(parts, prototype) {
  if (length(parts) == 0L) return(prototype)
  result <- do.call(rbind, parts)
  rownames(result) <- NULL
  result
}

.history_labels <- function(snapshots, labels) {
  if (is.null(labels)) {
    labels <- names(snapshots)
    if (is.null(labels)) {
      labels <- sprintf("run_%03d", seq_along(snapshots))
    }
  }
  if (!is.character(labels) || length(labels) != length(snapshots) ||
      anyNA(labels) || any(!nzchar(trimws(labels))) ||
      anyDuplicated(labels)) {
    stop("labels must be unique, nonempty strings, one per snapshot",
         call. = FALSE)
  }
  labels
}

.history_event_rows <- function(comparison, step, before_label, after_label) {
  event_parts <- list()
  append <- function(change_type, doi, field, before, after) {
    count <- length(doi)
    if (count == 0L) return(invisible(NULL))
    event_parts[[length(event_parts) + 1L]] <<- data.frame(
      transition_index = rep.int(as.integer(step), count),
      from = rep(before_label, count),
      to = rep(after_label, count),
      change_type = rep(change_type, count),
      doi = as.character(doi),
      field = as.character(field),
      before = as.character(before),
      after = as.character(after),
      stringsAsFactors = FALSE
    )
    invisible(NULL)
  }
  if (nrow(comparison$added) > 0L) {
    n <- nrow(comparison$added)
    append("added", comparison$added$doi,
           rep(NA_character_, n), rep(NA_character_, n),
           rep(NA_character_, n))
  }
  if (nrow(comparison$removed) > 0L) {
    n <- nrow(comparison$removed)
    append("removed", comparison$removed$doi,
           rep(NA_character_, n), rep(NA_character_, n),
           rep(NA_character_, n))
  }
  if (nrow(comparison$modified) > 0L) {
    append("modified", comparison$modified$doi,
           comparison$modified$field, comparison$modified$before,
           comparison$modified$after)
  }
  if (nrow(comparison$provenance) > 0L) {
    append("provenance_changed", comparison$provenance$doi,
           rep("source_set", nrow(comparison$provenance)),
           comparison$provenance$before, comparison$provenance$after)
  }
  event_parts
}

#' Compare a chronological series of literature-search snapshots
#'
#' Compare successive `SearchSnapshot` observations using canonical DOI
#' equality. Each transition reports newly observed and disappeared DOIs,
#' modified metadata fields, changed source provenance and records that cannot
#' be matched because they have no recognizable DOI.
#'
#' Alongside the change events, each run records the provider retrieval counts
#' and mean metadata completeness/structural-validity scores. These are
#' properties of retrieved metadata, not measures of scientific quality.
#'
#' Search settings and provider completeness are assessed for each transition.
#' A disappeared DOI does *not* establish withdrawal, retraction or deletion.
#' Settings and provider coverage may explain observed differences.
#'
#' @param snapshots An ordered list of at least two SearchSnapshot objects or
#'   trusted RDS paths from [save_search_snapshot()]. Do not read untrusted
#'   serialized files. Runs must be in nondecreasing finished_at UTC order.
#' @param labels Optional unique nonempty names for the runs. Defaults to
#'   list names, or run_001, run_002, etc. when unnamed.
#'
#' @return A SearchHistory containing schema_version, runs, providers,
#'   transitions, events, and unmatched tables.
#' @export
#'
#' @examples
#' plan <- new_search_plan("example", sources = "crossref")
#' mock <- function(url, headers, params, timeout) {
#'   list(message = list(items = list()))
#' }
#' snapshot <- run_search_plan(plan, requester = mock)
#' compare_search_history(list(snapshot, snapshot))$transitions
compare_search_history <- function(snapshots, labels = NULL) {
  if (!is.list(snapshots) || length(snapshots) < 2L) {
    stop("snapshots must be an ordered list of at least two runs",
         call. = FALSE)
  }
  labels <- .history_labels(snapshots, labels)
  runs_data <- lapply(snapshots, function(snapshot) {
    if (is.character(snapshot) && length(snapshot) == 1L &&
        !is.na(snapshot)) {
      snapshot <- read_search_snapshot(snapshot)
    }
    .check_search_snapshot(snapshot)
    snapshot
  })
  n <- length(runs_data)
  finished <- vapply(
    runs_data, function(snapshot) snapshot$finished_at, character(1L)
  )
  if (is.unsorted(finished, strictly = FALSE)) {
    stop("snapshots must have nondecreasing finished_at UTC timestamps",
         call. = FALSE)
  }

  runs <- data.frame(
    snapshot_index = seq_len(n),
    label = labels,
    started_at = vapply(
      runs_data, function(snapshot) snapshot$started_at, character(1L)
    ),
    finished_at = finished,
    articles = integer(n),
    with_doi = integer(n),
    without_doi = integer(n),
    mean_completeness = rep(NA_real_, n),
    mean_validity = rep(NA_real_, n),
    complete_sources = logical(n),
    stringsAsFactors = FALSE
  )

  provider_parts <- list()
  for (i in seq_len(n)) {
    snapshot <- runs_data[[i]]
    articles <- snapshot$result$articles
    keys <- .snapshot_doi_index(snapshot)$keys
    runs$articles[[i]] <- length(articles)
    runs$with_doi[[i]] <- sum(!is.na(keys))
    runs$without_doi[[i]] <- sum(is.na(keys))
    runs$complete_sources[[i]] <- isTRUE(snapshot$context$complete)
    if (length(articles) > 0L) {
      quality <- assess_article_quality(articles)$records
      runs$mean_completeness[[i]] <- mean(quality$completeness)
      runs$mean_validity[[i]] <- mean(quality$validity)
    }

    providers <- snapshot$result$sources
    cols <- c("source", "status", "retrieved", "contributed")
    if (!all(cols %in% names(providers))) {
      stop("Snapshot provider summary lacks required columns",
           call. = FALSE)
    }
    provider_parts[[i]] <- data.frame(
      snapshot_index = rep.int(as.integer(i), nrow(providers)),
      label = rep(labels[[i]], nrow(providers)),
      source = providers$source,
      status = providers$status,
      retrieved = providers$retrieved,
      contributed = providers$contributed,
      stringsAsFactors = FALSE
    )
  }

  transitions <- data.frame(
    transition_index = seq_len(n - 1L),
    from = labels[-n],
    to = labels[-1L],
    from_finished_at = finished[-n],
    to_finished_at = finished[-1L],
    added = integer(n - 1L),
    removed = integer(n - 1L),
    modified_articles = integer(n - 1L),
    provenance_changed = integer(n - 1L),
    unmatched_before = integer(n - 1L),
    unmatched_after = integer(n - 1L),
    same_plan = logical(n - 1L),
    complete_sources = logical(n - 1L),
    comparable = logical(n - 1L),
    stringsAsFactors = FALSE
  )

  event_parts <- list()
  unmatched_parts <- list()
  for (i in seq_len(n - 1L)) {
    comparison <- compare_search_snapshots(
      runs_data[[i]], runs_data[[i + 1L]]
    )
    summary <- comparison$summary[1L, , drop = FALSE]
    for (field in c(
      "added", "removed", "modified_articles", "provenance_changed",
      "unmatched_before", "unmatched_after", "same_plan", "complete_sources"
    )) {
      transitions[[field]][[i]] <- summary[[field]][[1L]]
    }
    transitions$comparable[[i]] <-
      isTRUE(summary$same_plan[[1L]]) &&
      isTRUE(summary$complete_sources[[1L]])
    event_parts <- c(
      event_parts,
      .history_event_rows(comparison, i, labels[[i]], labels[[i + 1L]])
    )
    unmatched <- comparison$unmatched
    if (nrow(unmatched) > 0L) {
      # compare_search_snapshots uses input_index for its output-article
      # position; do not confuse it with a raw provider input position.
      unmatched_parts[[length(unmatched_parts) + 1L]] <- data.frame(
        transition_index = rep.int(as.integer(i), nrow(unmatched)),
        label = ifelse(unmatched$snapshot == "before",
                       labels[[i]], labels[[i + 1L]]),
        side = unmatched$snapshot,
        article_index = as.integer(unmatched$input_index),
        stringsAsFactors = FALSE
      )
    }
  }

  provider_prototype <- data.frame(
    snapshot_index = integer(), label = character(),
    source = character(), status = character(),
    retrieved = integer(), contributed = integer(),
    stringsAsFactors = FALSE
  )
  event_prototype <- data.frame(
    transition_index = integer(), from = character(), to = character(),
    change_type = character(), doi = character(), field = character(),
    before = character(), after = character(), stringsAsFactors = FALSE
  )
  unmatched_prototype <- data.frame(
    transition_index = integer(), label = character(), side = character(),
    article_index = integer(), stringsAsFactors = FALSE
  )
  structure(
    list(
      schema_version = .SEARCH_HISTORY_VERSION,
      runs = runs,
      providers = .history_bind(provider_parts, provider_prototype),
      transitions = transitions,
      events = .history_bind(event_parts, event_prototype),
      unmatched = .history_bind(unmatched_parts, unmatched_prototype)
    ),
    class = "SearchHistory"
  )
}

.check_search_history <- function(history) {
  expected <- c(
    "schema_version", "runs", "providers",
    "transitions", "events", "unmatched"
  )
  if (!inherits(history, "SearchHistory") ||
      !is.list(history) || !identical(names(history), expected) ||
      !identical(history$schema_version, .SEARCH_HISTORY_VERSION)) {
    stop("history must be a supported SearchHistory", call. = FALSE)
  }
  required <- list(
    runs = c("snapshot_index", "label", "started_at", "finished_at",
             "articles", "with_doi", "without_doi", "mean_completeness",
             "mean_validity", "complete_sources"),
    providers = c("snapshot_index", "label", "source", "status",
                  "retrieved", "contributed"),
    transitions = c("transition_index", "from", "to",
                    "from_finished_at", "to_finished_at", "added", "removed",
                    "modified_articles", "provenance_changed",
                    "unmatched_before", "unmatched_after", "same_plan",
                    "complete_sources", "comparable"),
    events = c("transition_index", "from", "to", "change_type", "doi",
               "field", "before", "after"),
    unmatched = c("transition_index", "label", "side", "article_index")
  )
  for (name in names(required)) {
    value <- history[[name]]
    if (!is.data.frame(value) ||
        !all(required[[name]] %in% names(value))) {
      stop(sprintf("history %s has an invalid table schema", name),
           call. = FALSE)
    }
  }
  invisible(NULL)
}

#' Export an auditable search-history report to CSV files
#'
#' Write six UTF-8 CSV tables (runs, providers, transitions, events, unmatched)
#' and a short README describing interpretation limits. Provider error texts,
#' API keys, and contact addresses are not included. By default, metadata
#' before/after values are suppressed in the event table; opt in using
#' include_values = TRUE. CSV cells are sanitized against spreadsheet
#' formula injection. A new directory is required and existing reports
#' are never intentionally overwritten.
#'
#' @param history A SearchHistory created by [compare_search_history()].
#' @param directory Path to a new report directory, within an existing parent.
#' @param include_values Whether to include changed field values (which may
#'   include full abstracts) in events.csv. Defaults to FALSE.
#'
#' @return The report directory path invisibly.
#' @export
#'
#' @examples
#' plan <- new_search_plan("example", sources = "crossref")
#' mock <- function(url, headers, params, timeout) {
#'   list(message = list(items = list()))
#' }
#' snapshot <- run_search_plan(plan, requester = mock)
#' history <- compare_search_history(list(snapshot, snapshot))
#' directory <- tempfile("search-history-")
#' write_search_history_report(history, directory)
#' unlink(directory, recursive = TRUE)
write_search_history_report <- function(
    history, directory, include_values = FALSE) {
  .check_search_history(history)
  directory <- .require_string(directory, "directory")
  if (!is.logical(include_values) || length(include_values) != 1L ||
      is.na(include_values)) {
    stop("include_values must be TRUE or FALSE", call. = FALSE)
  }
  parent <- dirname(directory)
  if (!dir.exists(parent)) {
    stop("Report parent directory does not exist", call. = FALSE)
  }
  if (file.exists(directory)) {
    stop("Report directory already exists", call. = FALSE)
  }
  staging <- tempfile(pattern = ".abstractExtractR-history-", tmpdir = parent)
  if (!dir.create(staging)) {
    stop("Could not create report staging directory", call. = FALSE)
  }
  on.exit(unlink(staging, recursive = TRUE), add = TRUE)

  tables <- list(
    runs = history$runs,
    providers = history$providers,
    transitions = history$transitions,
    events = history$events,
    unmatched = history$unmatched
  )
  if (!include_values) {
    tables$events$before <- rep(NA_character_, nrow(tables$events))
    tables$events$after <- rep(NA_character_, nrow(tables$events))
  }
  for (name in names(tables)) {
    path <- file.path(staging, paste0(name, ".csv"))
    utils::write.csv(
      .csv_formula_safe(tables[[name]]), file = path,
      row.names = FALSE, na = "", fileEncoding = "UTF-8"
    )
  }
  readme <- c(
    "# Scholarly search-history report",
    "",
    "Each transition compares adjacent UTC-dated search snapshots.",
    "DOIs are matched using canonical identity; DOI-free items remain unmatched.",
    "Added or removed records reflect observed search results, not necessarily",
    "new publications, withdrawals, or retractions.",
    "Check transitions.csv for plan equality and complete provider coverage.",
    "Runs.csv contains structural metadata completeness/validity means only.",
    "No API credentials, contact addresses or raw provider errors are exported.",
    if (include_values) {
      "The event table includes changed metadata values, including abstracts."
    } else {
      "Changed field values are omitted; pass include_values=TRUE to include."
    },
    "CSV cells are escaped to reduce spreadsheet-formula injection risk."
  )
  writeLines(readme, con = file.path(staging, "README.md"), useBytes = TRUE)

  if (!file.rename(staging, directory)) {
    stop("Could not publish the completed search-history report",
         call. = FALSE)
  }
  invisible(directory)
}
