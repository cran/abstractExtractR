test_that("Crossref DOI path is URL-encoded and abstract is preserved", {
  callback <- function(url, headers, params, timeout) {
    expect_match(url, "10.1000%2Fexample%2Fpath$", perl = TRUE)
    expect_equal(headers$Accept, "application/json")
    expect_length(params, 0L)
    list(message = list(abstract = "<jats:p>Abstract</jats:p>"))
  }
  expect_equal(get_abstract_from_doi(" 10.1000/example/path ",
                                     requester = callback),
               "<jats:p>Abstract</jats:p>")
  empty <- function(url, headers, params, timeout) list(message = list())
  expect_null(get_abstract_from_doi("10.1000/example", requester = empty))
  expect_error(get_abstract_from_doi(" "), "doi")
  expect_error(get_abstract_from_doi("10.1000/example", timeout = -1), "timeout")
  missing <- function(url, headers, params, timeout) list()
  expect_error(get_abstract_from_doi("10.1000/example", requester = missing),
               "message")
})

test_that("Article validates metadata and preserves author order", {
  article <- Article(title = "Paper", authors = c("Alice", "Bob"))
  expect_s3_class(article, "Article")
  expect_equal(article$authors, c("Alice", "Bob"))
  expect_error(Article(title = c("one", "two")), "title")
  expect_error(Article(authors = NA_character_), "authors")
})
