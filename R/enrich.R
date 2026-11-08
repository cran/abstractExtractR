#' Fill missing article abstracts from Crossref
#'
#' Enrich normalized Scopus [Article()] records using batch DOI retrieval.
#' Only missing or whitespace-only abstracts are fetched by default.
#' Records, order, and list names are preserved, and duplicate DOI lookups
#' are avoided. Crossref JATS/XML markup is preserved unmodified.
#'
#' @param articles List of Article objects.
#' @param overwrite If TRUE, replace existing abstracts only when Crossref
#'   supplies a non-blank string.
#' @param on_error Either "stop", "warn" or "skip" for Crossref failures.
#' @param timeout Positive HTTP request timeout in seconds.
#' @param requester Optional HTTP callback taking url, headers, params and
#'   timeout. NULL (default) enables bounded httr2 HTTP retries.
#' @param cache Optional reusable environment from [new_crossref_cache()].
#' @param mailto Optional Crossref contact email.
#' @param max_tries Total number of HTTP attempts (1 to 10); with custom
#'   requester callbacks, retries are controlled by that callback.
#' @param min_interval Delay in seconds between distinct uncached DOI lookups.
#' @param sleep Injectable delay function, defaults to [Sys.sleep()].
#'
#' @return Updated list of Article records with existing metadata preserved.
#' @export
#'
#' @examples
#' articles <- list(Article(title = "Example", doi = "10.1000/example"))
#' mock <- function(url, headers, params, timeout) {
#'   list(message = list(abstract = "Example abstract"))
#' }
#' enrich_articles_with_crossref(articles, requester = mock)
enrich_articles_with_crossref <- function(
    articles, overwrite = FALSE, on_error = c("stop", "warn", "skip"),
    timeout = .DEFAULT_TIMEOUT, requester = NULL, cache = NULL,
    mailto = NULL, max_tries = 3L, min_interval = 0.25,
    sleep = Sys.sleep) {
  if (!is.list(articles) ||
      !all(vapply(articles, inherits, logical(1L), what = "Article"))) {
    stop("articles must be a list of Article objects", call. = FALSE)
  }
  if (!is.logical(overwrite) || length(overwrite) != 1L ||
      is.na(overwrite)) {
    stop("overwrite must be TRUE or FALSE", call. = FALSE)
  }

  eligible <- vapply(articles, function(article) {
    doi <- article$doi
    abstract <- article$abstract
    has_doi <- !is.null(doi) && nzchar(trimws(doi))
    needs_abstract <- is.null(abstract) || !nzchar(trimws(abstract))
    has_doi && (overwrite || needs_abstract)
  }, logical(1L))
  indexes <- which(eligible)
  dois <- vapply(articles[indexes], function(article) article$doi,
                character(1L))

  # Call even with an empty vector to validate options consistently.
  abstracts <- get_abstracts_from_dois(
    dois, cache = cache, mailto = mailto, max_tries = max_tries,
    min_interval = min_interval, on_error = on_error, timeout = timeout,
    requester = requester, sleep = sleep
  )
  updated <- articles

  for (j in seq_along(indexes)) {
    value <- abstracts[[j]]
    if (!is.null(value) && nzchar(trimws(value))) {
      updated[[indexes[[j]]]]$abstract <- value
    }
  }
  updated
}
