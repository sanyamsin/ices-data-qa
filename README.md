# ices-data-qa

Reproducible data quality pipeline for fisheries stock assessment data
from the [ICES Stock Assessment Graphs (SAG) API](https://sag.ices.dk/SAG_API/).

The pipeline ingests the published stock list and assessment summary
tables, then runs a three-tier QA framework:

| Tier | Question | Checks |
|---|---|---|
| **Structural** | Can the data be used at all? | schema conformance, missing values, key uniqueness |
| **Statistical** | Are the values plausible? | ranges, IQR outliers, non-negativity, plateaus |
| **Timeliness** | Is the data fit for the advice cycle? | recency, yearly continuity per stock |

Every check returns a standardised result row (`category`, `check`,
`column`, `n_issues`, `share_issues`, `status`, `detail`), which makes the
framework easy to extend, test and report.

## Findings from the first run

A sample of 10 stocks from the 2025 assessment year (171 stocks) already
surfaced real data characteristics:

- ~51% missing values in F, SSB and Catches series — driven by
  data-limited stocks (e.g. sandeel, dab, eel) for which full analytical
  assessment outputs are not published every year;
- Outlier flags on pooled F and Discards values, highlighting that
  cross-stock pooling mixes very different scales (see roadmap).

## Quick start

```r
# 1. Install dependencies
install.packages(c("targets", "tarchetypes", "testthat", "httr2", "jsonlite",
                   "dplyr", "tidyr", "purrr", "tibble", "cli",
                   "ggplot2", "scales", "knitr", "rmarkdown", "quarto"))

# 2. Run the full pipeline (ingestion + QA + HTML report)
targets::tar_make()

# 3. Inspect results
targets::tar_read(qa_all)       # all check results
targets::tar_read(qa_overview)  # summary by tier
targets::tar_visnetwork()       # pipeline dependency graph
```

The rendered report is written to `report/qa-report.html`.

## Run the tests

```r
source("tests/testthat.R")
```

Unit tests run offline against synthetic data; tests calling the live SAG
API are skipped automatically when the API is unreachable.

## Project layout

```
ices-data-qa/
├── _targets.R              # pipeline definition (targets)
├── R/
│   ├── fetch_ices.R        # resilient client for the SAG API
│   ├── qa_structural.R     # schema / missingness / uniqueness checks
│   ├── qa_statistical.R    # range / outlier / plateau checks
│   ├── qa_timeliness.R     # recency / continuity checks
│   └── utils.R             # standardised QA result helpers
├── report/qa-report.qmd    # Quarto report reading from the targets store
├── tests/testthat/         # unit tests (offline) + live API tests
├── .github/workflows/      # CI: tests on push, weekly pipeline run
└── DESCRIPTION             # dependency manifest for CI
```

## Design choices

- **Fail loud, fail early** : API errors abort with the failing URL;
  nothing is silently swallowed.
- **Pure functions** : checks take a data frame and return a result row;
  no hidden state, so they are trivially unit-testable.
- **Defensive parsing** : the API returns JSON null for unpopulated
  fields and nests the series under `Lines`; the client handles both.
- **Polite caching** : the stock list is cached for one day
  (`tar_cue_age`) to avoid hammering the API during development.

## Roadmap

- [ ] Group-aware statistical checks (per-stock outlier detection)
- [ ] Extend coverage to the ICES DATRAS (trawl survey) web services
- [ ] Publish QA results as machine-readable JSON for downstream dashboards
- [ ] Wrap the QA framework as an R package with pkgdown documentation

## Author

Serge-Alain Nyamsin  Data scientist & AI with 10+ years of experience managing
monitoring & evaluation data for EU-funded humanitarian and development
programmes. [GitHub](https://github.com/sanyamsin)

## License

MIT : see [LICENSE](LICENSE).