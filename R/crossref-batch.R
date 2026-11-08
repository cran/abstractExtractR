# Internal httr2 transport with bounded retries for transient HTTP failures.
# httr2 respects Retry-After and otherwise uses exponential backoff with jitter.
.crossref_retry_requester <- function(max_tries) {
  force(max_tries)
  function(url, headers, params, timeout) {
    req <- httr2::request(url)
    req <- do.call(httr2::req_headers, c(list(req), headers))
    if (length(params) > 0L) {
      req <- do.call(httr2::req_url_query, c(list(req), params))
    }
    req <- httr2::req_timeout(req, seconds = timeout)
    if (max_tries > 1L) {
      req <- httr2::req_retry(
        req,
        max_tries = max_tries,
        retry_on_failure = TRUE,
        is_transient = function(resp) {
          httr2::resp_status(resp) %in% c(408L, 429L, 500L, 502L, 503L, 504L)
        }
      )
    }
    response <- httr2::req_perform(req)
    httr2::resp_body_json(response, simplifyVector = FALSE)
  }
}

#' Create a reusable in-memory Crossref abstract cache
#'
#' Reusing the cache across calls avoids requesting the same DOI repeatedly.
#' Both found and missing abstracts are cached. Network failures are not
#' persisted in the cache.
#'
#' @return An environment suitable for the cache argument of
#'   [get_abstracts_from_dois()] and [enrich_articles_with_crossref()].
#' @export
#' @examples
#' cache <- new_crossref_cache()
#' is.environment(cache)
new_crossref_cache <- function() {
  new.env(hash = TRUE, parent = emptyenv())
}

#' Retrieve a batch of Crossref abstracts by DOI
#'
#' Looks up each distinct DOI at most once, preserving the input order.
#' Both present and absent abstracts are cached, so repeated queries can be
#' avoided by passing the same cache to multiple calls.
#'
#' Requests are sequential and spaced with min_interval seconds between
#' uncached lookups. With the default HTTP requester, httr2 retries transient
#' status codes (408, 429, 500, 502, 503, 504) and connection failures,
#' honoring Retry-After or exponential backoff. Injected request callbacks
#' are called directly without automatic HTTP retries.
#'
#' @param dois Character vector of DOIs. Missing or blank values yield NULL
#'   without a request.
#' @param cache Optional environment, such as [new_crossref_cache()]. If NULL,
#'   a fresh per-call cache is used.
#' @param mailto Optional email address identifying the caller to Crossref.
#' @param max_tries Integer 1 to 10, including the initial HTTP request.
#' @param min_interval Nonnegative minimum delay in seconds between distinct
#'   uncached DOI lookups. Defaults to 0.25 (four requests per second).
#' @param on_error One of "stop", "warn", or "skip". Failed lookups are not
#'   cached across calls.
#' @param timeout Positive HTTP request timeout in seconds.
#' @param requester Optional callback accepting url, headers, params, timeout.
#'   NULL uses httr2 with retry policy; callbacks bypass retries.
#' @param sleep Function used for delays; defaults to [Sys.sleep()]. Injectable
#'   for deterministic, network-free tests.
#'
#' @return Named list with one entry per input DOI in input order. Entries
#'   contain a character string, or NULL for missing/failed abstracts. Names
#'   retain the original DOI values, including duplicates.
#' @export
#'
#' @examples
#' mock <- function(url, headers, params, timeout) {
#'   list(message = list(abstract = "Sample abstract"))
#' }
#' get_abstracts_from_dois(c("10.1000/example", "10.1000/example"),
#'                         requester = mock, min_interval = 0)
get_abstracts_from_dois <- function(
    dois, cache = NULL, mailto = NULL, max_tries = 3L,
    min_interval = 0.25, on_error = c("stop", "warn", "skip"),
    timeout = .DEFAULT_TIMEOUT, requester = NULL,
    sleep = Sys.sleep) {
  if (!is.character(dois)) {
    stop("dois must be a character vector", call. = FALSE)
  }
  if (is.null(cache)) {
    cache <- new_crossref_cache()
  }
  if (!is.environment(cache)) {
    stop("cache must be an environment or NULL", call. = FALSE)
  }
  .validate_crossref_mailto(mailto)
  if (!is.numeric(max_tries) || length(max_tries) != 1L ||
      is.na(max_tries) || !is.finite(max_tries) ||
      max_tries < 1 || max_tries > 10 || max_tries != floor(max_tries)) {
    stop("max_tries must be an integer between 1 and 10", call. = FALSE)
  }
  if (!is.numeric(min_interval) || length(min_interval) != 1L ||
      is.na(min_interval) || !is.finite(min_interval) || min_interval < 0) {
    stop("min_interval must be a finite nonnegative number", call. = FALSE)
  }
  on_error <- match.arg(on_error)
  .validate_timeout(timeout)
  if (!is.function(sleep)) {
    stop("sleep must be a function", call. = FALSE)
  }
  if (is.null(requester)) {
    requester <- .crossref_retry_requester(as.integer(max_tries))
  }
  .validate_requester(requester)

  results <- vector("list", length(dois))
  if (length(dois) > 0L) {
    names(results) <- dois
  }
  attempted <- new.env(hash = TRUE, parent = emptyenv())
  request_count <- 0L

  for (i in seq_along(dois)) {
    doi <- dois[[i]]
    if (is.na(doi) || !nzchar(trimws(doi))) {
      next
    }
    doi <- trimws(doi)
    key <- paste0("doi:", tolower(doi))

    # NULL values are intentional negative-cache entries.
    if (exists(key, envir = cache, inherits = FALSE)) {
      results[i] <- list(get(key, envir = cache, inherits = FALSE))
      next
    }
    if (exists(key, envir = attempted, inherits = FALSE)) {
      results[i] <- list(get(key, envir = attempted, inherits = FALSE))
      next
    }

    if (request_count > 0L && min_interval > 0) {
      sleep(min_interval)
    }
    request_count <- request_count + 1L

    outcome <- tryCatch(
      list(
        ok = TRUE,
        value = get_abstract_from_doi(
          doi, timeout = timeout, requester = requester, mailto = mailto
        )
      ),
      error = function(err) {
        if (on_error == "stop") {
          stop(err)
        }
        if (on_error == "warn") {
          warning(
            sprintf("Crossref lookup failed for DOI %s: %s",
                    doi, conditionMessage(err)),
            call. = FALSE
          )
        }
        list(ok = FALSE, value = NULL)
      }
    )

    assign(key, outcome$value, envir = attempted)
    if (outcome$ok) {
      assign(key, outcome$value, envir = cache)
    }
    results[i] <- list(outcome$value)
  }
  results
}
