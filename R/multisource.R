# Orchestrate bounded searches across existing scholarly data providers.
#
# Source order is significant: DOI reconciliation keeps the first populated
# metadata value and reports conflicts rather than guessing which is correct.

.search_scopus_bounded <- function(
    query, api_key, max_results, page_size, start_date, end_date,
    author, timeout, requester, min_interval, sleep) {
  query <- .require_string(query, "query")
  api_key <- .require_string(api_key, "scopus_api_key")

  # Scopus requires paired date bounds in the current client API.
  if (xor(is.null(start_date), is.null(end_date))) {
    stop("Scopus needs both start_date and end_date", call. = FALSE)
  }
  if (!is.null(author)) {
    query <- paste0(query, " AND AUTHOR(",
                    .require_string(author, "scopus_author"), ")")
  }
  params <- list(query = query, count = min(page_size, max_results),
                 cursor = "*")
  if (!is.null(start_date)) {
    params$date <- paste0(start_date, " to ", end_date)
  }
  headers <- list(Accept = "application/json",
                  "X-ELS-APIKey" = api_key)
  entries <- list()
  seen <- "*"

  repeat {
    payload <- .call_json(
      requester = requester, url = .SCOPUS_SEARCH_URL,
      headers = headers, params = params, timeout = timeout
    )
    if (!.is_object(payload)) {
      stop("Scopus returned a non-object JSON response", call. = FALSE)
    }
    search_results <- payload[["search-results"]]
    if (!.is_object(search_results)) {
      stop("Scopus response is missing 'search-results'", call. = FALSE)
    }
    page <- search_results[["entry"]]
    if (is.null(page)) page <- list()
    if (!.is_array(page)) {
      stop("Scopus 'entry' field must be a list", call. = FALSE)
    }
    usable <- Filter(.is_object, page)
    if (length(page) > 0L && length(usable) == 0L) {
      stop("Scopus pagination returned no usable entries",
           call. = FALSE)
    }
    entries <- c(entries, usable)
    if (length(entries) >= max_results) break

    cursor <- search_results[["cursor"]]
    next_cursor <- if (.is_object(cursor)) {
      .optional_string(cursor[["@next"]])
    } else {
      NULL
    }
    if (is.null(next_cursor) || !nzchar(next_cursor)) break
    if (next_cursor %in% seen) {
      stop("Scopus returned a repeated pagination cursor", call. = FALSE)
    }
    if (length(usable) == 0L) {
      stop("Scopus pagination made no progress", call. = FALSE)
    }
    seen <- c(seen, next_cursor)
    params$cursor <- next_cursor
    params$count <- min(page_size, max_results - length(entries))
    if (min_interval > 0) sleep(min_interval)
  }
  process_scopus_entries(entries[seq_len(min(length(entries), max_results))])
}

.search_provider_error <- function(error, secrets) {
  # Error callbacks can include request arguments; never emit credentials in
  # diagnostic results or warnings, even for injected custom requesters.
  message <- conditionMessage(error)
  for (secret in secrets) {
    if (!is.null(secret) && nzchar(secret)) {
      message <- gsub(secret, "[redacted]", message, fixed = TRUE)
    }
  }
  message
}

#' Search Scopus, Crossref, and OpenAlex with provenance
#'
#' Fetches bounded result sets from the selected providers, normalizes them to
#' Article records, and reconciles exact DOI matches. Each provider is queried
#' in the requested order. Where populated fields disagree, the earlier
#' provider's value is kept and the disagreement is reported.
#'
#' The result includes a row for each retrieved record in provenance, showing
#' its original source, position, canonical DOI, reconciliation status and
#' final output index. DOI-free articles remain separate. No fuzzy matching
#' or unsupported source-to-source field inferences are performed.
#'
#' By default Crossref and OpenAlex are selected because they allow small
#' queries without credentials. Scopus requires scopus_api_key. OpenAlex
#' requires openalex_api_key for more than one cursor page. Source-specific
#' type filters are separate because the provider taxonomies differ.
#'
#' @param query Non-empty scholarly search expression.
#' @param sources Non-empty vector containing unique elements of "crossref",
#'   "openalex", and "scopus". Order determines metadata precedence.
#' @param max_results_per_source Maximum works fetched from each source,
#'   between 1 and 500. This is not a global result limit.
#' @param page_size Maximum works per request (1 to 25); 25 respects the
#'   Scopus API page-size restriction.
#' @param start_date,end_date Optional YYYY-MM-DD publication date bounds.
#'   Scopus searches require either both or neither.
#' @param scopus_api_key Optional Elsevier API key; required for Scopus.
#' @param openalex_api_key Optional OpenAlex API key; required when OpenAlex
#'   needs more than one cursor page.
#' @param crossref_mailto Optional contact email for Crossref.
#' @param crossref_work_type Optional Crossref type such as "journal-article".
#' @param openalex_work_type Optional OpenAlex type such as "article".
#' @param scopus_author Optional Scopus author expression.
#' @param on_error Provider failure policy: "stop" (default), "warn" or
#'   "skip". A failed source is recorded in the results for warn/skip.
#' @param on_conflict How to handle populated fields differing for one DOI:
#'   "keep_first" (default), "warn", or "error".
#' @param timeout Positive timeout per HTTP request in seconds.
#' @param max_tries Attempts for the built-in Crossref/OpenAlex retry transport
#'   (1 to 10). Scopus uses its standard request transport.
#' @param min_interval Nonnegative delay between requests for each source.
#' @param requester Optional shared HTTP callback receiving url, headers,
#'   params, timeout. Useful for deterministic network-free testing.
#' @param sleep Injectable delay function, default Sys.sleep.
#'
#' @return List with articles (reconciled Article records), provenance
#'   (input-to-output mapping with source labels), conflicts (disagreements
#'   found during reconciliation), sources (one status row per requested
#'   provider), and errors (provider-level failure messages).
#' @export
#'
#' @examples
#' mock <- function(url, headers, params, timeout) {
#'   if (grepl("crossref", url, fixed = TRUE)) {
#'     return(list(message = list(items = list(
#'       list(title = list("Example"), DOI = "10.1000/a")
#'     ))))
#'   }
#'   list(results = list(
#'     list(title = "Example", doi = "https://doi.org/10.1000/a")
#'   ), meta = list())
#' }
#' result <- search_articles("example", requester = mock, min_interval = 0)
#' result$provenance
search_articles <- function(
    query,
    sources = c("crossref", "openalex"),
    max_results_per_source = 25L,
    page_size = 25L,
    start_date = NULL,
    end_date = NULL,
    scopus_api_key = NULL,
    openalex_api_key = NULL,
    crossref_mailto = NULL,
    crossref_work_type = NULL,
    openalex_work_type = NULL,
    scopus_author = NULL,
    on_error = c("stop", "warn", "skip"),
    on_conflict = c("keep_first", "warn", "error"),
    timeout = .DEFAULT_TIMEOUT,
    max_tries = 3L,
    min_interval = 0.25,
    requester = NULL,
    sleep = Sys.sleep) {
  query <- .require_string(query, "query")
  allowed <- c("crossref", "openalex", "scopus")
  if (!is.character(sources) || length(sources) == 0L ||
      anyNA(sources) || anyDuplicated(sources) ||
      !all(sources %in% allowed)) {
    stop("sources must be unique provider names: crossref, openalex, scopus",
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
  if (!is.null(scopus_api_key)) {
    scopus_api_key <- .require_string(scopus_api_key, "scopus_api_key")
  }
  if (!is.null(openalex_api_key)) {
    openalex_api_key <- .require_string(openalex_api_key, "openalex_api_key")
  }
  .validate_crossref_mailto(crossref_mailto)
  if (!is.null(requester)) .validate_requester(requester)
  .validate_timeout(timeout)
  if (!is.numeric(min_interval) || length(min_interval) != 1L ||
      is.na(min_interval) || !is.finite(min_interval) || min_interval < 0) {
    stop("min_interval must be a finite nonnegative number", call. = FALSE)
  }
  if (!is.function(sleep)) stop("sleep must be a function", call. = FALSE)
  on_error <- match.arg(on_error)
  on_conflict <- match.arg(on_conflict)

  summary <- data.frame(
    source = sources,
    status = rep("pending", length(sources)),
    retrieved = integer(length(sources)),
    contributed = integer(length(sources)),
    error_message = rep(NA_character_, length(sources)),
    stringsAsFactors = FALSE
  )
  errors <- data.frame(
    source = character(), message = character(), stringsAsFactors = FALSE
  )
  articles <- list()
  origin <- character()
  source_index <- integer()

  for (i in seq_along(sources)) {
    provider <- sources[[i]]
    # The wrapper permits strict, warning, or best-effort execution without
    # accidentally treating a failed provider as a valid empty result set.
    result <- tryCatch(
      {
        works <- switch(
          provider,
          crossref = search_crossref(
            query = query, max_results = max_results_per_source,
            page_size = page_size, start_date = start_date,
            end_date = end_date, work_type = crossref_work_type,
            mailto = crossref_mailto, timeout = timeout,
            max_tries = max_tries, min_interval = min_interval,
            requester = requester, sleep = sleep
          ),
          openalex = search_openalex(
            query = query, api_key = openalex_api_key,
            max_results = max_results_per_source, page_size = page_size,
            start_date = start_date, end_date = end_date,
            work_type = openalex_work_type, timeout = timeout,
            max_tries = max_tries, min_interval = min_interval,
            requester = requester, sleep = sleep
          ),
          scopus = .search_scopus_bounded(
            query = query, api_key = scopus_api_key,
            max_results = max_results_per_source, page_size = page_size,
            start_date = start_date, end_date = end_date,
            author = scopus_author, timeout = timeout,
            requester = if (is.null(requester)) .http_get_json else requester,
            min_interval = min_interval, sleep = sleep
          )
        )
        if (!is.list(works) || !all(vapply(
          works, inherits, logical(1L), what = "Article"
        ))) {
          stop("Provider returned invalid Article records", call. = FALSE)
        }
        list(ok = TRUE, works = works, error = NULL)
      },
      error = function(err) list(ok = FALSE, works = list(), error = err)
    )

    if (!result$ok) {
      message <- .search_provider_error(
        result$error,
        list(scopus_api_key, openalex_api_key, crossref_mailto)
      )
      summary$status[[i]] <- "failed"
      summary$error_message[[i]] <- message
      errors[nrow(errors) + 1L, ] <- list(provider, message)
      if (on_error == "stop") {
        stop(sprintf("%s search failed: %s", provider, message),
             call. = FALSE)
      }
      if (on_error == "warn") {
        warning(sprintf("%s search failed: %s", provider, message),
                call. = FALSE)
      }
      next
    }
    summary$status[[i]] <- "ok"
    summary$retrieved[[i]] <- length(result$works)
    articles <- c(articles, result$works)
    origin <- c(origin, rep(provider, length(result$works)))
    source_index <- c(source_index, seq_along(result$works))
  }

  reconciled <- deduplicate_articles(
    articles, on_conflict = on_conflict, report = TRUE
  )
  provenance <- data.frame(
    source = origin, source_index = source_index,
    reconciled$mapping, stringsAsFactors = FALSE
  )
  for (i in seq_along(sources)) {
    summary$contributed[[i]] <- sum(
      provenance$source == sources[[i]] &
        provenance$status %in% c("unique", "unkeyed")
    )
  }
  conflicts <- reconciled$conflicts
  conflicts$source <- origin[conflicts$input_index]

  list(
    articles = reconciled$articles,
    provenance = provenance,
    conflicts = conflicts,
    sources = summary,
    errors = errors
  )
}
