# abstractExtractR 0.3.0

First CRAN release.

## Breaking changes

- Remove the GitHub-only `dataexcept` dependency so that every dependency is
  available from CRAN. Request failures are still signalled as
  `abstract_extract_request_error` conditions with the endpoint in `$source`
  and the original error in `$parent`, but they no longer inherit from
  `dataexcept_error` or serialise to a `dataexcept` JSON envelope.

## Documentation

- Generate all help pages from roxygen2 comments, so that every exported
  function documents its return value and has a runnable offline example.

# abstractExtractR 0.2.0

Internal development release (not published on CRAN). It consolidates the
changes listed below: native Crossref, OpenAlex and Scopus search; DOI
reconciliation; quality diagnostics; reproducible SearchSnapshot evidence;
SHA-256 research archives; longitudinal metadata comparisons; offline
integration and clean-install tests.

# abstractExtractR 0.1.0

Initial development release.

- Implement Scopus search with cursor pagination and article normalization.
- Support Crossref DOI lookups and bibliographic article discovery.
- Enrich missing abstracts using reusable in-memory DOI caching.
- Add bounded retry policies and configurable request intervals.
- Export normalized article records as data frames, CSV, BibTeX, and RIS.
- Provide offline examples, unit tests, and full HTML/PDF documentation checks.

This package has not yet been submitted to CRAN.

## Development additions (unreleased)

- Add deterministic DOI normalization, article deduplication and an audit trail
  for conflicting metadata across Crossref and Scopus.
- CRAN submission is deferred until the package matures.

- Add metadata completeness and structural validity scoring with customizable
  field weights, DOI-duplicate flags and issue reports (unreleased).

- Add OpenAlex works search, paginated retrieval, abstract reconstruction,
  and normalized article metadata (unreleased).

- Use the pinned GitHub dataexcept R package for structured request failures,
  preserving the existing request error class and cause (unreleased).

- Add bounded multi-provider search with provenance and error diagnostics (unreleased).

- Add versioned, credential-free search plans and reproducible RDS snapshots
  containing results, provenance, conflicts, and UTC timestamps (unreleased).

- Add JSON search manifests and DOI-based comparisons between saved search snapshots (unreleased).

- Add chronological search histories and safe CSV metadata-drift reports (unreleased).

- Add explicit on-demand search refresh with non-overwriting evidence bundles
  and drift reports (unreleased).

- Add local append-only research archives with chronological history (unreleased).

- Add local archive checksums, cooperative writer locks and explicit
  interrupted-write recovery (unreleased).

- Introduce SHA-256 integrity v2 with protected-run policy and backward
  compatibility for previous MD5 archive runs (unreleased).

- Add manifest-based archive inventory and validated per-run selective access
  for large archives (unreleased).

- Add bounded and selective archive comparison without loading all RDS snapshots (unreleased).

- Speed up archive append by avoiding full historical RDS loading and add
  optional recent-only refresh results (unreleased).

- Add per-run DOI indexes and historical DOI observation tracing (unreleased).

- Add DOI-level longitudinal metadata timelines with abstract fingerprints and observed gaps (unreleased).

- Align refresh documentation with the API and qualify stats helper imports;
  use a warning-free development check independently of CRAN eligibility.

- Add representative frozen JSON contract fixtures for three providers and
  an explicitly invoked live smoke-test script (unreleased).

- Add bounded pagination stress tests and reject unusable provider pages;
  redact contact email in provider error reports (unreleased).

- Reduce repeated allocations in DOI reconciliation, metadata-quality issue
  reporting and manifest indexing; add larger offline regression datasets.

- Add deterministic end-to-end research workflow regression tests covering
  search, reconciliation, quality assessment, archive refresh and exports.

- Consolidate primary research workflows in getting-started and API documentation,
  with a regression test of the documented offline path (unreleased).

- Add clean source-tarball installation smoke tests and R 4.2.3 optional compatibility checks.

- Audit development-release validation evidence and correct outdated archive
  locking and checksum documentation (unreleased).

- Add an opt-in manual GitLab job for live provider connectivity and
  normalization checks, isolated from routine offline CI (unreleased).
