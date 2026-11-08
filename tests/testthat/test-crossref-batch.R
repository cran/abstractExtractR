test_that("batch retrieval preserves duplicates and caches absent abstracts", {
  count <- 0L
  callback <- function(url, headers, params, timeout) {
    count <<- count + 1L
    if (grepl("missing", url, fixed = TRUE)) {
      return(list(message = list()))
    }
    list(message = list(abstract = "Found"))
  }
  cache <- new_crossref_cache()
  dois <- c("10.1000/FOUND", " 10.1000/found ", "10.1000/missing",
            "10.1000/MISSING", "", NA_character_)
  result <- get_abstracts_from_dois(
    dois, cache = cache, requester = callback, min_interval = 0
  )
  expect_identical(names(result), dois)
  expect_length(result, length(dois))
  expect_identical(result[[1]], "Found")
  expect_identical(result[[2]], "Found")
  expect_null(result[[3]])
  expect_null(result[[4]])
  expect_null(result[[5]])
  expect_null(result[[6]])
  expect_equal(count, 2L)
  again <- get_abstracts_from_dois(
    dois, cache = cache, requester = callback, min_interval = 0
  )
  expect_identical(again, result)
  expect_equal(count, 2L)
})

test_that("batch throttles distinct uncached requests only", {
  count <- 0L
  delays <- numeric()
  callback <- function(url, headers, params, timeout) {
    count <<- count + 1L
    list(message = list(abstract = "Abstract"))
  }
  clock <- function(seconds) delays <<- c(delays, seconds)
  get_abstracts_from_dois(
    c("10.1000/a", "10.1000/a", "10.1000/b", "10.1000/c"),
    requester = callback, sleep = clock, min_interval = 0.4
  )
  expect_equal(count, 3L)
  expect_equal(delays, c(0.4, 0.4))
})

test_that("mailto is forwarded and metadata is preserved during enrichment", {
  callback <- function(url, headers, params, timeout) {
    expect_equal(params$mailto, "research@example.org")
    expect_match(headers[["User-Agent"]], "abstractExtractR", fixed = TRUE)
    list(message = list(abstract = "<jats:p>Recovered</jats:p>"))
  }
  original <- list(one = Article(
    title = "Paper", authors = c("Alice", "Bob"),
    abstract = NULL, doi = "10.1000/a"
  ))
  output <- enrich_articles_with_crossref(
    original, requester = callback, mailto = "research@example.org",
    min_interval = 0
  )
  expect_identical(names(output), names(original))
  expect_identical(output$one$abstract, "<jats:p>Recovered</jats:p>")
  expect_identical(output$one$authors, c("Alice", "Bob"))
  expect_null(original$one$abstract)
})

test_that("failed lookups are not stored in reusable cache", {
  cache <- new_crossref_cache()
  count <- 0L
  callback <- function(url, headers, params, timeout) {
    count <<- count + 1L
    if (count == 1L) stop("temporary failure")
    list(message = list(abstract = "Recovered"))
  }
  first <- get_abstracts_from_dois(
    "10.1000/a", cache = cache, requester = callback,
    on_error = "skip", min_interval = 0
  )
  expect_null(first[[1]])
  second <- get_abstracts_from_dois(
    "10.1000/a", cache = cache, requester = callback, min_interval = 0
  )
  expect_identical(second[[1]], "Recovered")
  expect_equal(count, 2L)
})

test_that("invalid inputs fail before HTTP", {
  callback <- function(url, headers, params, timeout) {
    fail("no request expected")
  }
  expect_error(get_abstracts_from_dois(10, requester = callback), "character")
  expect_error(get_abstracts_from_dois("x", cache = list(),
                                       requester = callback), "cache")
  expect_error(get_abstracts_from_dois("x", max_tries = 0,
                                       requester = callback), "max_tries")
  expect_error(get_abstracts_from_dois("x", min_interval = -1,
                                       requester = callback), "min_interval")
  expect_error(get_abstracts_from_dois("x", requester = callback,
                                       sleep = "wrong"), "sleep")
  expect_error(get_abstracts_from_dois("x", requester = callback,
                                       mailto = "not-an-email"), "mailto")
  expect_identical(get_abstracts_from_dois(character(),
                        requester = callback), list())
})
