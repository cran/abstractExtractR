# Internal input and response handling shared by Scopus and Crossref.
.SCOPUS_SEARCH_URL <- "https://api.elsevier.com/content/search/scopus"
.CROSSREF_WORKS_URL <- "https://api.crossref.org/works"
.MAX_PAGE_SIZE <- 25L
.DEFAULT_TIMEOUT <- 30

.require_string <- function(value, name) {
  if (!is.character(value) || length(value) != 1L ||
      is.na(value) || !nzchar(trimws(value))) {
    stop(sprintf("%s must be a non-empty string", name), call. = FALSE)
  }
  trimws(value)
}

.optional_string <- function(value) {
  if (is.character(value) && length(value) == 1L && !is.na(value)) {
    return(value)
  }
  NULL
}

.validate_timeout <- function(timeout) {
  if (!is.numeric(timeout) || length(timeout) != 1L ||
      is.na(timeout) || !is.finite(timeout) || timeout <= 0) {
    stop("timeout must be a positive finite number", call. = FALSE)
  }
  invisible(NULL)
}

.validate_requester <- function(requester) {
  if (!is.function(requester)) {
    stop("requester must be a function", call. = FALSE)
  }
  invisible(NULL)
}

.is_object <- function(x) {
  is.list(x) && (length(x) == 0L || !is.null(names(x)))
}

.is_array <- function(x) {
  is.list(x) && (length(x) == 0L || is.null(names(x)))
}

# The callback makes HTTP fully replaceable in tests and custom environments.
.http_get_json <- function(url, headers, params, timeout) {
  req <- httr2::request(url)
  req <- do.call(httr2::req_headers, c(list(req), headers))
  if (length(params) > 0L) {
    req <- do.call(httr2::req_url_query, c(list(req), params))
  }
  req <- httr2::req_timeout(req, seconds = timeout)
  response <- httr2::req_perform(req)
  httr2::resp_body_json(response, simplifyVector = FALSE)
}

.call_json <- function(requester, url, headers, params, timeout) {
  .validate_requester(requester)
  tryCatch(
    requester(url = url, headers = headers, params = params, timeout = timeout),
    error = function(err) {
      if (inherits(err, "abstract_extract_request_error")) {
        stop(err)
      }
      # Endpoints carry no credentials; keys and contact email are in
      # headers or query parameters, which are never copied into the error.
      condition <- errorCondition(
        sprintf("Failed to retrieve or decode data from %s", url),
        parent = err,
        source = url,
        class = "abstract_extract_request_error",
        call = NULL
      )
      stop(condition)
    }
  )
}
