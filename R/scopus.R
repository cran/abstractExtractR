#' Fetch one page of Scopus search results
#'
#' Query the Scopus Search API. The function returns the raw JSON response as
#' nested lists and does not normalize individual article records.
#'
#' @param query Non-empty Scopus search expression.
#' @param api_key Non-empty Elsevier API key. Do not store keys in source code.
#' @param max_results Integer page size, between 1 and 25.
#' @param timeout Positive request timeout in seconds.
#' @param requester Optional HTTP callback accepting url, headers, params,
#'   and timeout. Primarily useful for deterministic tests.
#'
#' @return Raw Scopus JSON object decoded into an R list.
#' @export
#' @examples
#' mock <- function(url, headers, params, timeout) {
#'   list("search-results" = list(entry = list(list("dc:title" = "Example"))))
#' }
#' fetch_from_scopus("forecasting", "example-key", requester = mock)
fetch_from_scopus <- function(query, api_key, max_results = .MAX_PAGE_SIZE,
                              timeout = .DEFAULT_TIMEOUT,
                              requester = .http_get_json) {
  query <- .require_string(query, "query")
  api_key <- .require_string(api_key, "api_key")
  if (!is.numeric(max_results) || length(max_results) != 1L ||
      is.na(max_results) || !is.finite(max_results) ||
      max_results != as.integer(max_results) ||
      max_results < 1L || max_results > .MAX_PAGE_SIZE) {
    stop("max_results must be an integer between 1 and 25", call. = FALSE)
  }
  .validate_timeout(timeout)
  payload <- .call_json(
    requester, .SCOPUS_SEARCH_URL,
    headers = list(Accept = "application/json", "X-ELS-APIKey" = api_key),
    params = list(query = query, count = as.integer(max_results)),
    timeout = timeout
  )
  if (!.is_object(payload)) {
    stop("Scopus returned a non-object JSON response", call. = FALSE)
  }
  payload
}

#' Fetch all Scopus results through cursor pagination
#'
#' Request successive Scopus pages until no next cursor is returned.
#' A repeated cursor is rejected to prevent infinite loops.
#'
#' @inheritParams fetch_from_scopus
#' @param start_date,end_date Optional ISO date bounds (YYYY-MM-DD). Both must
#'   be supplied together.
#' @param author Optional non-empty author expression appended to the query.
#'
#' @return A list of raw Scopus entry objects, in page order.
#' @export
#' @examples
#' mock <- function(url, headers, params, timeout) {
#'   list("search-results" = list(
#'     entry = list(list("dc:title" = "Example")), cursor = list()
#'   ))
#' }
#' entries <- fetch_all_from_scopus(
#'   "forecasting", "example-key", requester = mock
#' )
#' length(entries)
fetch_all_from_scopus <- function(query, api_key, start_date = NULL,
                                  end_date = NULL, author = NULL,
                                  timeout = .DEFAULT_TIMEOUT,
                                  requester = .http_get_json) {
  query <- .require_string(query, "query")
  api_key <- .require_string(api_key, "api_key")
  if (xor(is.null(start_date), is.null(end_date))) {
    stop("start_date and end_date must be provided together", call. = FALSE)
  }
  if (!is.null(start_date)) {
    start_date <- .require_string(start_date, "start_date")
    end_date <- .require_string(end_date, "end_date")
  }
  if (!is.null(author)) {
    query <- paste0(query, " AND AUTHOR(",
                    .require_string(author, "author"), ")")
  }
  .validate_timeout(timeout)
  params <- list(query = query, count = .MAX_PAGE_SIZE, cursor = "*")
  if (!is.null(start_date)) {
    params$date <- paste0(start_date, " to ", end_date)
  }

  entries <- list()
  seen_cursors <- "*"
  consecutive_empty <- 0L
  repeat {
    payload <- .call_json(
      requester, .SCOPUS_SEARCH_URL,
      headers = list(Accept = "application/json", "X-ELS-APIKey" = api_key),
      params = params,
      timeout = timeout
    )
    if (!.is_object(payload)) {
      stop("Scopus returned a non-object JSON response", call. = FALSE)
    }
    results <- payload[["search-results"]]
    if (!.is_object(results)) {
      stop("Scopus response is missing 'search-results'", call. = FALSE)
    }
    page <- results[["entry"]]
    if (is.null(page)) {
      page <- list()
    }
    if (!.is_array(page)) {
      stop("Scopus 'entry' field must be an array", call. = FALSE)
    }
    usable <- Filter(.is_object, page)
    if (length(page) > 0L && length(usable) == 0L) {
      stop("Scopus pagination returned no usable entries",
           call. = FALSE)
    }
    entries <- c(entries, usable)

    cursor <- results[["cursor"]]
    next_cursor <- if (.is_object(cursor)) {
      .optional_string(cursor[["@next"]])
    } else {
      NULL
    }
    if (is.null(next_cursor) || !nzchar(next_cursor)) {
      break
    }
    if (next_cursor %in% seen_cursors) {
      stop("Scopus returned a repeated pagination cursor", call. = FALSE)
    }
    # Some APIs may provide an empty page with a next-cursor. Allow one,
    # but fail explicitly when successive empty pages make no progress.
    consecutive_empty <- if (length(usable) == 0L) {
      consecutive_empty + 1L
    } else {
      0L
    }
    if (consecutive_empty >= 2L) {
      stop("Scopus pagination made no progress", call. = FALSE)
    }
    seen_cursors <- c(seen_cursors, next_cursor)
    params$cursor <- next_cursor
  }
  entries
}

#' Normalize raw Scopus search entries
#'
#' Converts selected Dublin Core and PRISM fields into validated Article
#' objects. Malformed entry and author objects are ignored.
#'
#' @param entries List of raw Scopus entry objects.
#'
#' @return A list of Article objects.
#' @export
#' @examples
#' entries <- list(list("dc:title" = "Example", "prism:doi" = "10.1000/example"))
#' process_scopus_entries(entries)
process_scopus_entries <- function(entries) {
  if (!is.list(entries)) {
    stop("entries must be a list", call. = FALSE)
  }
  lapply(Filter(.is_object, entries), function(entry) {
    raw_authors <- entry[["author"]]
    authors <- character()
    if (.is_array(raw_authors)) {
      for (raw_author in raw_authors) {
        if (.is_object(raw_author)) {
          name <- .optional_string(raw_author[["authname"]])
          if (!is.null(name)) {
            authors <- c(authors, name)
          }
        }
      }
    }
    Article(
      title = .optional_string(entry[["dc:title"]]),
      abstract = .optional_string(entry[["dc:description"]]),
      publication_date = .optional_string(entry[["prism:coverDate"]]),
      authors = authors,
      doi = .optional_string(entry[["prism:doi"]])
    )
  })
}

#' Normalize a complete Scopus search response
#'
#' @param payload Raw Scopus JSON object containing search-results.entry.
#'
#' @return A list of Article objects.
#' @export
#' @examples
#' payload <- list("search-results" = list(
#'   entry = list(list("dc:title" = "Example"))
#' ))
#' process_scopus_response(payload)
process_scopus_response <- function(payload) {
  if (!.is_object(payload) || !.is_object(payload[["search-results"]])) {
    stop("Scopus response is missing 'search-results'", call. = FALSE)
  }
  entries <- payload[["search-results"]][["entry"]]
  if (is.null(entries)) {
    entries <- list()
  }
  if (!.is_array(entries)) {
    stop("Scopus 'entry' field must be an array", call. = FALSE)
  }
  process_scopus_entries(entries)
}
