# Crossref discovery relies on query.bibliographic, not the deprecated
# query.title parameter. These helpers keep requests and normalization distinct.

.validate_crossref_count <- function(value, name, upper) {
  if (!is.numeric(value) || length(value) != 1L ||
      is.na(value) || !is.finite(value) ||
      value < 1 || value > upper || value != floor(value)) {
    stop(sprintf("%s must be an integer between 1 and %s", name, upper),
         call. = FALSE)
  }
  as.integer(value)
}

.validate_crossref_date <- function(value, name) {
  if (is.null(value)) {
    return(NULL)
  }
  value <- .require_string(value, name)
  if (!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", value)) {
    stop(sprintf("%s must use YYYY-MM-DD", name), call. = FALSE)
  }
  parsed <- as.Date(value, format = "%Y-%m-%d")
  if (is.na(parsed) || format(parsed, "%Y-%m-%d") != value) {
    stop(sprintf("%s must be a valid calendar date", name), call. = FALSE)
  }
  value
}

.crossref_search_params <- function(query, rows, start_date, end_date,
                                    work_type, mailto, cursor) {
  query <- .require_string(query, "query")
  rows <- .validate_crossref_count(rows, "rows", 1000L)
  start_date <- .validate_crossref_date(start_date, "start_date")
  end_date <- .validate_crossref_date(end_date, "end_date")
  if (!is.null(start_date) && !is.null(end_date) &&
      start_date > end_date) {
    stop("start_date must not be after end_date", call. = FALSE)
  }
  .validate_crossref_mailto(mailto)

  filters <- character()
  if (!is.null(work_type)) {
    work_type <- .require_string(work_type, "work_type")
    if (!grepl("^[a-z][a-z0-9-]*$", work_type)) {
      stop("work_type must be a lower-case Crossref type identifier",
           call. = FALSE)
    }
    filters <- c(filters, paste0("type:", work_type))
  }
  if (!is.null(start_date)) {
    filters <- c(filters, paste0("from-pub-date:", start_date))
  }
  if (!is.null(end_date)) {
    filters <- c(filters, paste0("until-pub-date:", end_date))
  }

  params <- list("query.bibliographic" = query, rows = rows)
  if (length(filters) > 0L) {
    params$filter <- paste(filters, collapse = ",")
  }
  if (!is.null(cursor)) {
    params$cursor <- .require_string(cursor, "cursor")
  }
  if (!is.null(mailto)) {
    params$mailto <- mailto
  }
  params
}

#' Fetch one page of Crossref bibliographic search results
#'
#' Search the public Crossref \code{/works} endpoint without an API key.
#' Uses the supported \code{query.bibliographic} parameter, which searches
#' bibliographic metadata (not full article text). Crossref can omit abstracts.
#'
#' @param query Non-empty bibliographic query, such as a paper title or topic.
#' @param rows Number of records requested for the page, from 1 to 1000.
#' @param start_date,end_date Optional inclusive publication date bounds, in
#'   \code{YYYY-MM-DD} format. Either or both bounds may be specified.
#' @param work_type Optional Crossref work type such as "journal-article".
#' @param cursor Optional Crossref cursor; use "*" for the first cursor page.
#' @param mailto Optional contact email for Crossref's polite API pool.
#' @param timeout Positive HTTP timeout in seconds.
#' @param requester Optional callback accepting url, headers, params, timeout.
#'   Defaults to the package HTTP transport. Useful for offline tests.
#'
#' @return Raw Crossref response as a nested R list.
#' @export
#' @examples
#' mock <- function(url, headers, params, timeout) {
#'   list(message = list(items = list(list(title = list("Example")))))
#' }
#' fetch_from_crossref("forecasting", requester = mock)
fetch_from_crossref <- function(
    query, rows = 20L, start_date = NULL, end_date = NULL,
    work_type = NULL, cursor = NULL, mailto = NULL,
    timeout = .DEFAULT_TIMEOUT, requester = .http_get_json) {
  params <- .crossref_search_params(
    query, rows, start_date, end_date, work_type, mailto, cursor
  )
  .validate_timeout(timeout)
  result <- .call_json(
    requester, .CROSSREF_WORKS_URL,
    headers = list(
      Accept = "application/json",
      "User-Agent" = paste0(
        "abstractExtractR/0.3.0 ",
        "(https://gitlab.com/DiogoRibeiro7/abstract-extract-r)"
      )
    ),
    params = params,
    timeout = timeout
  )
  if (!.is_object(result)) {
    stop("Crossref returned a non-object JSON response", call. = FALSE)
  }
  result
}

.first_crossref_string <- function(value) {
  if (is.character(value)) {
    if (length(value) > 0L) {
      return(.optional_string(value[[1L]]))
    }
    return(NULL)
  }
  if (is.list(value) && length(value) > 0L) {
    return(.optional_string(value[[1L]]))
  }
  NULL
}

.crossref_author_name <- function(author) {
  if (!.is_object(author)) {
    return(NULL)
  }
  given <- .optional_string(author[["given"]])
  family <- .optional_string(author[["family"]])
  components <- c(given, family)
  components <- components[nzchar(trimws(components))]
  if (length(components) > 0L) {
    return(paste(trimws(components), collapse = " "))
  }
  for (key in c("name", "literal")) {
    name <- .optional_string(author[[key]])
    if (!is.null(name) && nzchar(trimws(name))) {
      return(name)
    }
  }
  NULL
}

.crossref_publication_date <- function(entry) {
  # Crossref date-parts may contain only a year or year/month. Preserve
  # available precision instead of inventing an unspecified month or day.
  for (field in c("published", "published-online", "published-print", "issued")) {
    date <- entry[[field]]
    if (!.is_object(date)) {
      next
    }
    parts <- date[["date-parts"]]
    if (!.is_array(parts) || length(parts) == 0L) {
      next
    }
    values <- parts[[1L]]
    if (is.list(values)) {
      values <- unlist(values, use.names = FALSE)
    }
    if (!is.numeric(values) || length(values) < 1L ||
        length(values) > 3L || anyNA(values) ||
        any(!is.finite(values)) || any(values != floor(values))) {
      next
    }
    year <- values[[1L]]
    if (year < 1 || year > 9999) {
      next
    }
    candidate <- sprintf("%04d", year)
    if (length(values) >= 2L) {
      month <- values[[2L]]
      if (month < 1L || month > 12L) {
        next
      }
      candidate <- paste0(candidate, sprintf("-%02d", month))
    }
    if (length(values) == 3L) {
      day <- values[[3L]]
      candidate <- paste0(candidate, sprintf("-%02d", day))
      parsed <- as.Date(candidate, format = "%Y-%m-%d")
      if (is.na(parsed) || format(parsed, "%Y-%m-%d") != candidate) {
        next
      }
    }
    return(candidate)
  }
  NULL
}

#' Normalize raw Crossref works into Article objects
#'
#' Convert Crossref metadata into the same [Article()] format as Scopus.
#' Incomplete publication dates retain their original precision (e.g.
#' \code{"2024"} or \code{"2024-08"}). Non-record entries are ignored.
#'
#' @param entries A list of Crossref works metadata objects.
#'
#' @return A list of \code{Article} records.
#' @export
#' @examples
#' works <- list(list(title = list("Example"), DOI = "10.1000/example"))
#' process_crossref_entries(works)
process_crossref_entries <- function(entries) {
  if (!.is_array(entries)) {
    stop("entries must be a list of Crossref work objects", call. = FALSE)
  }
  lapply(Filter(.is_object, entries), function(entry) {
    raw_authors <- entry[["author"]]
    authors <- character()
    if (.is_array(raw_authors)) {
      for (author in raw_authors) {
        name <- .crossref_author_name(author)
        if (!is.null(name)) {
          authors <- c(authors, name)
        }
      }
    }
    Article(
      title = .first_crossref_string(entry[["title"]]),
      abstract = .optional_string(entry[["abstract"]]),
      publication_date = .crossref_publication_date(entry),
      authors = authors,
      doi = .optional_string(entry[["DOI"]])
    )
  })
}

#' Normalize a Crossref works search response
#'
#' @param payload A Crossref response containing \code{message.items}.
#'
#' @return A list of normalized \code{Article} records.
#' @export
#' @examples
#' payload <- list(message = list(items = list(
#'   list(title = list("Example"), DOI = "10.1000/example")
#' )))
#' process_crossref_response(payload)
process_crossref_response <- function(payload) {
  if (!.is_object(payload) || !.is_object(payload[["message"]])) {
    stop("Crossref response is missing 'message'", call. = FALSE)
  }
  items <- payload[["message"]][["items"]]
  if (!.is_array(items)) {
    stop("Crossref response 'items' must be a list", call. = FALSE)
  }
  process_crossref_entries(items)
}

#' Discover articles from Crossref without known DOIs
#'
#' Search Crossref bibliographic metadata and normalize the matching records.
#' Uses cursor pagination to retrieve up to \code{max_results}; unlike a
#' search that retrieves the entire Crossref index, this limit is finite.
#'
#' The cursor endpoint is paged until the requested number of works is
#' collected, a short page is received, or no next cursor is available.
#' Repeated cursors cause an error rather than an infinite loop.
#' Existing Crossref abstracts are preserved as supplied, including JATS/XML.
#'
#' @inheritParams fetch_from_crossref
#' @param max_results Maximum normalized records to return. Defaults to 50.
#' @param page_size Page size from 1 to 1000; defaults to 50.
#' @param max_tries Maximum HTTP attempts per page, 1 to 10, when using the
#'   default transport. Injected callback requesters bypass automatic retries.
#' @param min_interval Nonnegative delay between page requests in seconds.
#' @param sleep Injectable delay callback for offline tests.
#' @param requester Optional request callback; defaults to NULL, meaning
#'   bounded httr2 transient-error retries.
#'
#' @return A list of normalized \code{Article} records in result order.
#' @export
#'
#' @examples
#' mock <- function(url, headers, params, timeout) {
#'   list(message = list(items = list(
#'     list(title = list("Example paper"), DOI = "10.1000/example")
#'   )))
#' }
#' papers <- search_crossref("gaussian processes", requester = mock,
#'                           min_interval = 0)
#' papers[[1L]]$title
search_crossref <- function(
    query, max_results = 50L, page_size = 50L, start_date = NULL,
    end_date = NULL, work_type = NULL, mailto = NULL,
    timeout = .DEFAULT_TIMEOUT, max_tries = 3L, min_interval = 0.25,
    requester = NULL, sleep = Sys.sleep) {
  max_results <- .validate_crossref_count(
    max_results, "max_results", .Machine$integer.max
  )
  page_size <- .validate_crossref_count(page_size, "page_size", 1000L)
  max_tries <- .validate_crossref_count(max_tries, "max_tries", 10L)
  if (!is.numeric(min_interval) || length(min_interval) != 1L ||
      is.na(min_interval) || !is.finite(min_interval) || min_interval < 0) {
    stop("min_interval must be a finite nonnegative number", call. = FALSE)
  }
  if (!is.function(sleep)) {
    stop("sleep must be a function", call. = FALSE)
  }
  if (is.null(requester)) {
    requester <- .crossref_retry_requester(max_tries)
  }
  .validate_requester(requester)

  # Validate the complete search before making a network call.
  .crossref_search_params(
    query, page_size, start_date, end_date, work_type, mailto, cursor = "*"
  )
  .validate_timeout(timeout)

  articles <- list()
  cursor <- "*"
  seen_cursors <- "*"
  repeat {
    remaining <- max_results - length(articles)
    rows <- min(page_size, remaining)
    payload <- fetch_from_crossref(
      query = query, rows = rows, start_date = start_date,
      end_date = end_date, work_type = work_type, cursor = cursor,
      mailto = mailto, timeout = timeout, requester = requester
    )
    # Make unexpected response shapes explicit; do not silently discard pages.
    page <- payload[["message"]]
    records <- if (.is_object(page)) page[["items"]] else NULL
    page_articles <- process_crossref_response(payload)
    # A nonempty page yielding no usable records cannot advance our finite
    # result limit; do not follow arbitrary new cursors indefinitely.
    if (length(records) > 0L && length(page_articles) == 0L) {
      stop("Crossref pagination returned no usable works",
           call. = FALSE)
    }
    articles <- c(articles, page_articles)

    if (length(articles) >= max_results) {
      break
    }
    # Crossref can continue returning next-cursor after its final page.
    if (length(records) < rows) {
      break
    }
    next_cursor <- .optional_string(page[["next-cursor"]])
    if (is.null(next_cursor) || !nzchar(next_cursor)) {
      break
    }
    if (next_cursor %in% seen_cursors) {
      stop("Crossref returned a repeated pagination cursor", call. = FALSE)
    }
    seen_cursors <- c(seen_cursors, next_cursor)
    cursor <- next_cursor
    if (min_interval > 0) {
      sleep(min_interval)
    }
  }
  articles[seq_len(min(length(articles), max_results))]
}
