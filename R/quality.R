# Structural metadata diagnostics. No online validation or imputation.

.quality_fields <- c("title", "abstract", "publication_date", "authors", "doi")

.quality_weights <- function(weights) {
  if (!is.numeric(weights) || is.null(names(weights)) ||
      length(weights) != length(.quality_fields) ||
      anyDuplicated(names(weights)) ||
      !setequal(names(weights), .quality_fields) ||
      anyNA(weights) || any(!is.finite(weights)) ||
      any(weights < 0) || sum(weights) <= 0) {
    stop("weights must name the five metadata fields with finite nonnegative values and positive total",
         call. = FALSE)
  }
  weights[.quality_fields] / sum(weights)
}

.quality_date_valid <- function(value) {
  if (!grepl("^[0-9]{4}(-[0-9]{2}){0,2}$", value)) {
    return(FALSE)
  }
  parts <- strsplit(value, "-", fixed = TRUE)[[1L]]
  year <- as.integer(parts[[1L]])
  if (is.na(year) || year < 1L || year > 9999L) return(FALSE)
  if (length(parts) == 1L) return(TRUE)
  month <- as.integer(parts[[2L]])
  if (is.na(month) || month < 1L || month > 12L) return(FALSE)
  if (length(parts) == 2L) return(TRUE)
  parsed <- as.Date(value, format = "%Y-%m-%d")
  !is.na(parsed) && identical(format(parsed, "%Y-%m-%d"), value)
}

.quality_present <- function(value) {
  if (is.null(value)) return(FALSE)
  is.character(value) && length(value) > 0L &&
    !anyNA(value) && any(nzchar(trimws(value)))
}

.quality_valid <- function(value, field) {
  if (!.quality_present(value)) return(FALSE)
  if (field == "doi") return(!is.na(normalize_doi(value)[[1L]]))
  if (field == "publication_date") return(.quality_date_valid(value))
  if (field == "authors") return(all(nzchar(trimws(value))))
  length(value) == 1L
}

#' Evaluate structural quality of scholarly article metadata
#'
#' Computes weighted completeness (fields present) and weighted validity
#' (fields present and passing structural validation), plus a DOI duplicate
#' indicator. This is a metadata audit, not a measure of scientific reliability,
#' publication quality, or factual correctness. No HTTP requests are made.
#'
#' The default weights are equal across title, abstract, publication_date,
#' authors, and doi. Scores are normalized to the closed interval from 0 to 1.
#' Blank values are missing; malformed DOI and dates are present but invalid.
#' DOI duplication is reported separately and does not affect scores.
#'
#' @param articles List of Article records, including an empty list.
#' @param weights Named numeric vector with exactly the five field names;
#'   finite nonnegative values, at least one positive.
#'
#' @return A list with records (one row per article, scores and flags),
#'   summary (per-field counts and rates), issues (field-level missing/invalid
#'   diagnostics), and weights (normalized scoring weights).
#' @export
#' @examples
#' papers <- list(Article(title = "A paper", doi = "10.1000/example"))
#' assess_article_quality(papers)$records
assess_article_quality <- function(
    articles, weights = c(title = 1, abstract = 1,
                          publication_date = 1, authors = 1, doi = 1)) {
  if (!is.list(articles)) {
    stop("articles must be a list of Article records", call. = FALSE)
  }
  for (article in articles) .validate_dedup_article(article)
  weights <- .quality_weights(weights)
  n <- length(articles)

  present <- matrix(FALSE, nrow = n, ncol = length(.quality_fields),
                    dimnames = list(NULL, .quality_fields))
  valid <- present
  doi_values <- rep(NA_character_, n)
  for (i in seq_len(n)) {
    article <- articles[[i]]
    for (field in .quality_fields) {
      value <- article[[field]]
      present[i, field] <- .quality_present(value)
      valid[i, field] <- .quality_valid(value, field)
    }
    if (valid[i, "doi"]) {
      doi_values[[i]] <- normalize_doi(article$doi)[[1L]]
    }
  }

  seen <- new.env(hash = TRUE, parent = emptyenv())
  duplicate <- logical(n)
  for (i in seq_len(n)) {
    if (is.na(doi_values[[i]])) next
    key <- paste0("doi:", doi_values[[i]])
    duplicate[[i]] <- exists(key, envir = seen, inherits = FALSE)
    assign(key, TRUE, envir = seen)
  }

  completeness <- as.vector(present %*% weights)
  validity <- as.vector(valid %*% weights)
  records <- data.frame(
    input_index = seq_len(n), canonical_doi = doi_values,
    completeness = completeness, validity = validity,
    duplicate_doi = duplicate, stringsAsFactors = FALSE
  )
  for (field in .quality_fields) {
    records[[paste0(field, "_present")]] <- present[, field]
    records[[paste0(field, "_valid")]] <- valid[, field]
  }

  n_present <- colSums(present)
  n_valid <- colSums(valid)
  summary <- data.frame(
    field = .quality_fields, weight = unname(weights),
    present = as.integer(n_present), valid = as.integer(n_valid),
    missing = as.integer(n - n_present),
    invalid = as.integer(n_present - n_valid),
    presence_rate = if (n > 0L) as.numeric(n_present) / n else NA_real_,
    validity_rate = if (n > 0L) as.numeric(n_valid) / n else NA_real_,
    stringsAsFactors = FALSE
  )

  # Keep the established row-major issue order while allocating once.
  missing <- !present
  invalid <- present & !valid
  count <- sum(missing) + sum(invalid) + sum(duplicate)
  issue_rows <- vector("list", count)
  cursor <- 0L
  for (i in seq_len(n)) {
    for (j in seq_along(.quality_fields)) {
      status <- if (missing[i, j]) "missing" else if (
        invalid[i, j]) "invalid" else NULL
      if (!is.null(status)) {
        cursor <- cursor + 1L
        issue_rows[[cursor]] <- list(i, .quality_fields[[j]], status)
      }
    }
    if (duplicate[[i]]) {
      cursor <- cursor + 1L
      issue_rows[[cursor]] <- list(i, "doi", "duplicate")
    }
  }
  issues <- data.frame(
    input_index = if (count) vapply(issue_rows, `[[`, integer(1L), 1L) else integer(),
    field = if (count) vapply(issue_rows, `[[`, character(1L), 2L) else character(),
    issue = if (count) vapply(issue_rows, `[[`, character(1L), 3L) else character(),
    stringsAsFactors = FALSE
  )
  list(records = records, summary = summary, issues = issues,
       weights = weights)
}
