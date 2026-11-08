test_that("OpenAlex search request keeps key out of URL", {
  mock <- function(url, headers, params, timeout) {
    expect_identical(url, "https://api.openalex.org/works")
    expect_equal(headers$Authorization, "Bearer private-key")
    expect_false("api_key" %in% names(params))
    expect_equal(params$search, "forecasting")
    expect_equal(params[["per-page"]], 2L)
    expect_equal(params$filter, paste(
      "type:article", "from_publication_date:2020-01-01",
      "to_publication_date:2026-01-01", sep = ","
    ))
    list(results = list(), meta = list())
  }
  expect_type(fetch_from_openalex(
    "forecasting", api_key = "private-key", per_page = 2,
    start_date = "2020-01-01", end_date = "2026-01-01",
    work_type = "article", requester = mock
  ), "list")
})

test_that("inverted indexes are reconstructed deterministically", {
  index <- list(word = list(1L, 3L), First = list(0L),
                appears = list(2L))
  expect_identical(reconstruct_openalex_abstract(index),
                   "First word appears word")
  expect_null(reconstruct_openalex_abstract(NULL))
  expect_null(reconstruct_openalex_abstract(list()))
  expect_error(reconstruct_openalex_abstract(
    list(hello = 0L, world = 0L)), "unique")
  expect_error(reconstruct_openalex_abstract(
    list(hello = 0L, world = 2L)), "contiguous")
  expect_error(reconstruct_openalex_abstract(list(hello = -1L)), "invalid")
})

test_that("works normalize into Article without invented fields", {
  result <- process_openalex_entries(list(
    list(title = "Research", doi = "https://doi.org/10.1000/Example",
         abstract_inverted_index = list(Useful = list(0L),
                                         results = list(1L)),
         publication_date = "2024-04-01",
         authorships = list(
           list(author = list(display_name = "Ada Lovelace")),
           list(raw_author_name = "Research Group"))),
    list(display_name = "No abstract", publication_year = 2020L),
    "invalid"
  ))
  expect_length(result, 2)
  expect_s3_class(result[[1]], "Article")
  expect_identical(result[[1]]$doi, "10.1000/example")
  expect_identical(result[[1]]$abstract, "Useful results")
  expect_identical(result[[1]]$authors, c("Ada Lovelace", "Research Group"))
  expect_identical(result[[2]]$publication_date, "2020")
  expect_null(result[[2]]$abstract)
})

test_that("cursor paging respects limits and detects looping", {
  calls <- list()
  callback <- function(url, headers, params, timeout) {
    calls[[length(calls) + 1L]] <<- params
    if (length(calls) == 1L) {
      return(list(results = list(list(title = "One"),
                                 list(title = "Two")),
                  meta = list(next_cursor = "next")))
    }
    list(results = list(list(title = "Three")),
         meta = list(next_cursor = "later"))
  }
  papers <- search_openalex("time series", api_key = "key",
                           max_results = 3L, page_size = 2L,
                           requester = callback, min_interval = 0)
  expect_length(papers, 3L)
  expect_equal(calls[[1]][["per-page"]], 2L)
  expect_equal(calls[[2]][["per-page"]], 1L)
  expect_equal(calls[[2]]$cursor, "next")

  looping <- function(url, headers, params, timeout) {
    list(results = list(list(title = "One")),
         meta = list(next_cursor = "*"))
  }
  expect_error(search_openalex(
    "topic", api_key = "key", max_results = 2L, page_size = 1L,
    requester = looping, min_interval = 0), "repeated")
})

test_that("input errors occur before any live request", {
  never <- function(url, headers, params, timeout) {
    fail("Should not request live API")
  }
  expect_error(fetch_from_openalex("", requester = never), "query")
  expect_error(fetch_from_openalex("topic", per_page = 101L,
                                   requester = never), "per_page")
  expect_error(fetch_from_openalex("topic", page = 2L, cursor = "*",
                                   requester = never), "together")
  expect_error(search_openalex("topic", max_results = 100L,
                               page_size = 10L, requester = never), "api_key")
  expect_error(fetch_from_openalex("topic", start_date = "2026-02-30",
                                   requester = never), "date")
  expect_error(process_openalex_response(list()), "results")
})
