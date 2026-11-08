#' Construct a normalized scholarly article
#'
#' Create a validated S3 Article record. Optional metadata is stored as NULL
#' when absent, while authors are stored in source order as a character vector.
#'
#' @param title,abstract,publication_date,doi Optional scalar strings or NULL.
#' @param authors Character vector of author names in source order.
#'
#' @return A validated list with class Article.
#' @export
#'
#' @examples
#' article <- Article(
#'   title = "Example paper",
#'   abstract = "Example abstract",
#'   authors = c("Ada Lovelace", "Grace Hopper"),
#'   doi = "10.1000/example"
#' )
#' article$title
Article <- function(title = NULL, abstract = NULL, publication_date = NULL,
                    authors = character(), doi = NULL) {
  fields <- list(
    title = title,
    abstract = abstract,
    publication_date = publication_date,
    doi = doi
  )
  for (name in names(fields)) {
    value <- fields[[name]]
    if (!is.null(value) &&
        (!is.character(value) || length(value) != 1L || is.na(value))) {
      stop(sprintf("%s must be a scalar string or NULL", name), call. = FALSE)
    }
  }
  if (!is.character(authors) || anyNA(authors)) {
    stop("authors must be a character vector without NA", call. = FALSE)
  }
  structure(
    list(
      title = title,
      abstract = abstract,
      publication_date = publication_date,
      authors = authors,
      doi = doi
    ),
    class = "Article"
  )
}
