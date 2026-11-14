# ices-data-qa

Reproducible data quality pipeline for fisheries stock assessment data
from the [ICES Stock Assessment Graphs (SAG) API](https://sag.ices.dk/SAG_API/).

The pipeline ingests the published stock list and assessment summary
tables, then runs a three-tier QA framework:

| Tier | Question | Checks |
|---|---|---|
| **Structural** | Can the data be used at all? | schema conformance, key uniqueness, missing values per stock |
| **Statistical** | Are the values plausible? | non-negativity, ranges, IQR outliers and plateaus per stock |
| **Timeliness** | Is the data fit for the advice cycle? | recency, yearly continuity per stock |

Every check returns a standardised result row (`category`, `check`,
`column`, `n_issues`, `share_issues`, `status`, `detail`), which makes the
framework easy to extend, test and report.

## Findings

On a reproducible sample of 10 stocks from the 2025 assessment year
(171 stocks), the main result is about the checks as much as the data:
**pooling stocks makes sound data look broken.**

| Check on F | Stocks pooled | Per stock |
|---|---|---|
| Missing values | 51.9% (fail) | 0.4% (pass) |
| IQR outliers | 34 values (fail) | 1 value (pass) |

Of the 248 missing F values, only one is a gap inside a reported series.
The others are structure:

- 3 stocks do not report F at all (`san.sa.7r`, `bss.27.8c9a`,
  `ele.2737.nea`);
- 77 values precede the first reported year: the catch history is longer
  than the assessment period (`spr.27.7de` has catches from 1988 and F
  from 2013);
- 5 values are the latest year of the series, which has no F yet.

The checks therefore work stock by stock, and count separately the stocks
that do not report a variable, the years outside its reported period, and
the gaps inside it. Only the gaps drive the status; the rest is stated in
the `detail` column of each result.

Stocks also differ in what they report as removals: some fill `Catches`,
others only `Landings`. A check on a single column is misleading (see
roadmap).

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

- [x] Group-aware checks (per-stock missing values, outliers, plateaus)
- [ ] Removals check that accepts either `Catches` or `Landings`
- [ ] Extend coverage to the ICES DATRAS (trawl survey) web services
- [ ] Publish QA results as machine-readable JSON for downstream dashboards
- [ ] Wrap the QA framework as an R package with pkgdown documentation

## Author

Serge-Alain Nyamsin  Data scientist & AI with 10+ years of experience managing
monitoring & evaluation data for EU-funded humanitarian and development
programmes. [GitHub](https://github.com/sanyamsin)

## License

MIT : see [LICENSE](LICENSE).