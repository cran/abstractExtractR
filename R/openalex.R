# OpenAlex Works API search, pagination and normalized article records.
.OPENALEX_WORKS_URL <- "https://api.openalex.org/works"
.OPENALEX_MAX_PER_PAGE <- 100L

.openalex_key <- function(api_key) {
  if (is.null(api_key)) return(NULL)
  .require_string(api_key, "api_key")
}

.openalex_headers <- function(api_key) {
  headers <- list(
    Accept = "application/json",
    "User-Agent" = "abstractExtractR/0.3.0 (scholarly metadata client)"
  )
  if (!is.null(api_key)) {
    # OpenAlex supports bearer authorization. Keys never enter URLs or logs.
    headers$Authorization <- paste("Bearer", api_key)
  }
  headers
}

.openalex_params <- function(query, api_key, per_page, page, cursor,
                             start_date, end_date, work_type) {
  query <- .require_string(query, "query")
  api_key <- .openalex_key(api_key)
  per_page <- .validate_crossref_count(
    per_page, "per_page", .OPENALEX_MAX_PER_PAGE
  )
  if (!is.null(page) && !is.null(cursor)) {
    stop("page and cursor must not be supplied together", call. = FALSE)
  }
  if (!is.null(page)) {
    # OpenAlex basic paging cannot access records past position 10,000.
    page <- .validate_crossref_count(
      page, "page", as.integer(10000L %/% per_page)
    )
  }
  if (!is.null(cursor)) {
    cursor <- .require_string(cursor, "cursor")
    if (is.null(api_key) && cursor != "*") {
      stop("api_key is required after the first OpenAlex cursor page",
           call. = FALSE)
    }
  }
  start_date <- .validate_crossref_date(start_date, "start_date")
  end_date <- .validate_crossref_date(end_date, "end_date")
  if (!is.null(start_date) && !is.null(end_date) &&
      start_date > end_date) {
    stop("start_date must not be after end_date", call. = FALSE)
  }

  filters <- character()
  if (!is.null(work_type)) {
    work_type <- .require_string(work_type, "work_type")
    if (!grepl("^[a-z][a-z0-9-]*$", work_type)) {
      stop("work_type must be a lower-case OpenAlex type identifier",
           call. = FALSE)
    }
    filters <- c(filters, paste0("type:", work_type))
  }
  if (!is.null(start_date)) {
    filters <- c(filters, paste0("from_publication_date:", start_date))
  }
  if (!is.null(end_date)) {
    filters <- c(filters, paste0("to_publication_date:", end_date))
  }
  params <- list(search = query, "per-page" = per_page)
  if (length(filters) > 0L) params$filter <- paste(filters, collapse = ",")
  if (!is.null(cursor)) params$cursor <- cursor
  if (!is.null(page)) params$page <- page
  list(params = params, api_key = api_key)
}

#' Request one page of OpenAlex works
#'
#' Search the Works endpoint with the current \code{search} and
#' \code{per-page} parameters. Filters use OpenAlex work types and
#' publication dates. Basic, keyless queries are supported; a free
#' API key is necessary for cursor paging after the first page.
#'
#' Credentials, if provided, are sent in the Authorization header rather than
#' the query string. The requester callback supports offline testing.
#'
#' @param query Non-empty OpenAlex works search expression.
#' @param api_key Optional OpenAlex API key; required for multiple cursor pages.
#' @param per_page Number of results requested, between 1 and 100.
#' @param page Optional one-based basic page; cannot combine with cursor.
#' @param cursor Optional cursor. Use "*" to start cursor pagination.
#' @param start_date,end_date Optional inclusive YYYY-MM-DD publication dates.
#' @param work_type Optional OpenAlex type such as "article" or "review".
#' @param timeout Positive request timeout in seconds.
#' @param requester Injectable callback taking url, headers, params, timeout.
#' @return Raw OpenAlex response as a nested list.
#' @export
#' @examples
#' mock <- function(url, headers, params, timeout) {
#'   list(meta = list(count = 0), results = list())
#' }
#' fetch_from_openalex("gaussian processes", requester = mock)
fetch_from_openalex <- function(
    query, api_key = NULL, per_page = 25L, page = NULL, cursor = NULL,
    start_date = NULL, end_date = NULL, work_type = NULL,
    timeout = .DEFAULT_TIMEOUT, requester = .http_get_json) {
  values <- .openalex_params(
    query, api_key, per_page, page, cursor, start_date, end_date, work_type
  )
  .validate_timeout(timeout)
  payload <- .call_json(
    requester = requester,
    url = .OPENALEX_WORKS_URL,
    headers = .openalex_headers(values$api_key),
    params = values$params, timeout = timeout
  )
  if (!.is_object(payload)) {
    stop("OpenAlex returned a non-object JSON response", call. = FALSE)
  }
  payload
}

#' Restore an OpenAlex abstract from its inverted index
#'
#' OpenAlex represents available abstracts as tokens mapped to their
#' zero-based positions. This function sorts those positions and joins the
#' tokens with spaces. Missing or empty indexes yield NULL. Malformed
#' positions, duplicate positions, or gaps cause an explicit error rather
#' than silently fabricating text.
#'
#' @param index Named list of tokens mapped to integer position vectors,
#'   as returned in abstract_inverted_index, or NULL.
#' @return A character abstract or NULL when no index is supplied.
#' @export
#' @examples
#' reconstruct_openalex_abstract(list(
#'   evidence = 2L, Research = 0L, needs = 1L
#' ))
reconstruct_openalex_abstract <- function(index) {
  if (is.null(index) || (is.list(index) && length(index) == 0L)) {
    return(NULL)
  }
  if (!.is_object(index) || length(index) == 0L ||
      is.null(names(index)) || anyNA(names(index)) ||
      any(!nzchar(names(index))) || anyDuplicated(names(index))) {
    stop("OpenAlex abstract index must be a named token-position object",
         call. = FALSE)
  }
  words <- character()
  positions <- integer()
  for (word in names(index)) {
    values <- index[[word]]
    if (is.list(values)) {
      values <- unlist(values, recursive = TRUE, use.names = FALSE)
    }
    if (!is.numeric(values) || length(values) == 0L ||
        anyNA(values) || any(!is.finite(values)) ||
        any(values < 0) || any(values != floor(values)) ||
        any(values > .Machine$integer.max)) {
      stop("OpenAlex abstract index has invalid word positions",
           call. = FALSE)
    }
    positions <- c(positions, as.integer(values))
    words <- c(words, rep(word, length(values)))
  }

  # The total token count bounds the reconstruction size, not the largest
  # untrusted index value. Missing and overlapping indexes are rejected.
  if (anyDuplicated(positions) ||
      !identical(sort(positions), seq_len(length(positions)) - 1L)) {
    stop("OpenAlex abstract positions must be unique and contiguous",
         call. = FALSE)
  }
  paste(words[order(positions)], collapse = " ")
}

.openalex_author_name <- function(authorship) {
  if (!.is_object(authorship)) return(NULL)
  person <- authorship[["author"]]
  value <- if (.is_object(person)) {
    .optional_string(person[["display_name"]])
  } else NULL
  if (is.null(value) || !nzchar(trimws(value))) {
    value <- .optional_string(authorship[["raw_author_name"]])
  }
  if (is.null(value) || !nzchar(trimws(value))) NULL else value
}

.openalex_publication_date <- function(work) {
  value <- .optional_string(work[["publication_date"]])
  if (!is.null(value) && nzchar(trimws(value))) return(value)
  # Fall back to the year only: do not invent a month or day.
  year <- work[["publication_year"]]
  if (is.numeric(year) && length(year) == 1L &&
      !is.na(year) && is.finite(year) && year == floor(year) &&
      year >= 1 && year <= 9999) {
    return(sprintf("%04d", as.integer(year)))
  }
  NULL
}

#' Normalize OpenAlex works as Article records
#'
#' Converts titles, inverted-index abstracts, dates, authorship display names
#' and DOI resolver URLs into the package's standard Article representation.
#' Missing fields remain NULL. A present malformed abstract index fails
#' visibly to avoid silently returning truncated or invented text.
#'
#' @param entries List of OpenAlex works objects.
#' @return List of normalized Article records, in source order.
#' @export
#' @examples
#' works <- list(list(
#'   title = "Example", doi = "https://doi.org/10.1000/example",
#'   authorships = list(list(author = list(display_name = "Ada")))
#' ))
#' process_openalex_entries(works)
process_openalex_entries <- function(entries) {
  if (!.is_array(entries)) {
    stop("entries must be a list of OpenAlex works", call. = FALSE)
  }
  lapply(Filter(.is_object, entries), function(work) {
    names <- character()
    authorships <- work[["authorships"]]
    if (.is_array(authorships)) {
      for (entry in authorships) {
        name <- .openalex_author_name(entry)
        if (!is.null(name)) names <- c(names, name)
      }
    }

    raw_doi <- .optional_string(work[["doi"]])
    doi <- raw_doi
    if (!is.null(raw_doi) && nzchar(trimws(raw_doi))) {
      canonical <- normalize_doi(raw_doi)
      if (!is.na(canonical[[1L]])) doi <- canonical[[1L]]
    }
    title <- .optional_string(work[["title"]])
    if (is.null(title) || !nzchar(trimws(title))) {
      title <- .optional_string(work[["display_name"]])
    }

    Article(
      title = title,
      abstract = reconstruct_openalex_abstract(
        work[["abstract_inverted_index"]]
      ),
      publication_date = .openalex_publication_date(work),
      authors = names,
      doi = doi
    )
  })
}

#' Normalize an OpenAlex search response
#'
#' @param payload Raw OpenAlex response including a results array.
#' @return List of Article records.
#' @export
#' @examples
#' process_openalex_response(list(results = list(
#'   list(title = "A work", publication_date = "2025-01-02")
#' )))
process_openalex_response <- function(payload) {
  if (!.is_object(payload)) {
    stop("OpenAlex response must be an object", call. = FALSE)
  }
  results <- payload[["results"]]
  if (!.is_array(results)) {
    stop("OpenAlex response 'results' must be an array", call. = FALSE)
  }
  process_openalex_entries(results)
}

#' Discover scholarly articles from OpenAlex
#'
#' Search OpenAlex with finite result limits and cursor pagination. For
#' multi-page searches an OpenAlex API key is required; single-page
#' searches can use the smaller keyless budget. Cursor loops and malformed
#' pages raise errors. Request spacing avoids bursts of API traffic.
#'
#' @inheritParams fetch_from_openalex
#' @param max_results Maximum normalized results to return, at least 1.
#' @param page_size Requested works per cursor page, between 1 and 100.
#' @param max_tries Retry attempts for transient failures using httr2.
#' @param min_interval Delay in seconds between page requests.
#' @param sleep Injectable delay function for offline tests.
#' @param requester Optional callback; NULL enables retry-capable httr2.
#' @return List of normalized Article records.
#' @export
#' @examples
#' mock <- function(url, headers, params, timeout) {
#'   list(results = list(list(title = "Example")), meta = list())
#' }
#' search_openalex("forecasting", requester = mock, min_interval = 0)
search_openalex <- function(
    query, api_key = NULL, max_results = 25L, page_size = 25L,
    start_date = NULL, end_date = NULL, work_type = NULL,
    timeout = .DEFAULT_TIMEOUT, max_tries = 3L, min_interval = 0.25,
    requester = NULL, sleep = Sys.sleep) {
  max_results <- .validate_crossref_count(
    max_results, "max_results", .Machine$integer.max
  )
  page_size <- .validate_crossref_count(
    page_size, "page_size", .OPENALEX_MAX_PER_PAGE
  )
  max_tries <- .validate_crossref_count(max_tries, "max_tries", 10L)
  api_key <- .openalex_key(api_key)
  if (max_results > page_size && is.null(api_key)) {
    stop("api_key is required for OpenAlex cursor pagination past one page",
         call. = FALSE)
  }
  if (!is.numeric(min_interval) || length(min_interval) != 1L ||
      is.na(min_interval) || !is.finite(min_interval) || min_interval < 0) {
    stop("min_interval must be a finite nonnegative number", call. = FALSE)
  }
  if (!is.function(sleep)) {
    stop("sleep must be a function", call. = FALSE)
  }
  .openalex_params(
    query, api_key, page_size, page = NULL, cursor = "*",
    start_date, end_date, work_type
  )
  .validate_timeout(timeout)
  if (is.null(requester)) {
    requester <- .crossref_retry_requester(max_tries)
  }
  .validate_requester(requester)

  output <- list()
  cursor <- "*"
  seen <- "*"
  repeat {
    remaining <- max_results - length(output)
    count <- min(page_size, remaining)
    payload <- fetch_from_openalex(
      query = query, api_key = api_key, per_page = count,
      cursor = cursor, start_date = start_date,
      end_date = end_date, work_type = work_type,
      timeout = timeout, requester = requester
    )
    works <- payload[["results"]]
    page_articles <- process_openalex_response(payload)
    # Treat a nonempty but wholly unusable page as a provider failure,
    # rather than silently following an unbounded sequence of cursors.
    if (length(works) > 0L && length(page_articles) == 0L) {
      stop("OpenAlex pagination returned no usable works",
           call. = FALSE)
    }
    output <- c(output, page_articles)

    if (length(output) >= max_results || length(works) < count) break
    meta <- payload[["meta"]]
    if (!.is_object(meta)) {
      stop("OpenAlex response is missing 'meta' for pagination",
           call. = FALSE)
    }
    next_cursor <- .optional_string(meta[["next_cursor"]])
    if (is.null(next_cursor) || !nzchar(next_cursor)) break
    if (next_cursor %in% seen) {
      stop("OpenAlex returned a repeated pagination cursor", call. = FALSE)
    }
    seen <- c(seen, next_cursor)
    cursor <- next_cursor
    if (min_interval > 0) sleep(min_interval)
  }
  output[seq_len(min(length(output), max_results))]
}
