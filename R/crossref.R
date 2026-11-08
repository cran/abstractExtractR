#' Retrieve an abstract from Crossref using its DOI
#'
#' Looks up a DOI via the Crossref works endpoint. Abstracts can include
#' JATS/XML markup, which is returned without modification. Crossref does not
#' have abstracts for every indexed article.
#'
#' @param doi Non-empty DOI, without the doi.org URL prefix.
#' @param timeout Positive request timeout in seconds.
#' @param requester Optional HTTP callback accepting url, headers, params,
#'   and timeout. Primarily useful for deterministic tests.
#' @param mailto Optional contact email for Crossref's polite API pool.
#'
#' @return An abstract string as supplied by Crossref, or NULL if absent.
#' @export
#' @examples
#' mock <- function(url, headers, params, timeout) {
#'   list(message = list(abstract = "Example abstract"))
#' }
#' get_abstract_from_doi("10.1000/example", requester = mock)
get_abstract_from_doi <- function(doi, timeout = .DEFAULT_TIMEOUT,
                                  requester = .http_get_json, mailto = NULL) {
  doi <- .require_string(doi, "doi")
  .validate_timeout(timeout)
  .validate_crossref_mailto(mailto)
  url <- paste0(.CROSSREF_WORKS_URL, "/",
                utils::URLencode(doi, reserved = TRUE))
  params <- if (is.null(mailto)) list() else list(mailto = mailto)
  payload <- .call_json(
    requester, url,
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
  if (!.is_object(payload)) {
    stop("Crossref returned a non-object JSON response", call. = FALSE)
  }
  message <- payload[["message"]]
  if (!.is_object(message)) {
    stop("Crossref response is missing 'message'", call. = FALSE)
  }
  .optional_string(message[["abstract"]])
}

.validate_crossref_mailto <- function(mailto) {
  if (is.null(mailto)) {
    return(invisible(NULL))
  }
  if (!is.character(mailto) || length(mailto) != 1L ||
      is.na(mailto) ||
      !grepl("^[^[:space:]@]+@[^[:space:]@]+\\.[^[:space:]@]+$", mailto)) {
    stop("mailto must be a valid contact email or NULL", call. = FALSE)
  }
  invisible(NULL)
}
