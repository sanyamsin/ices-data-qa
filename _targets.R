# Reproducible pipeline: ICES SAG data quality assessment.
#
# Run with:  targets::tar_make()
# Inspect:   targets::tar_visnetwork()   targets::tar_read(qa_all)

library(targets)

library(targets)
library(tarchetypes)   # fournit tar_cue_age()
tar_source("R")

tar_option_set(
  packages = c("dplyr", "tidyr", "purrr", "tibble", "httr2", "jsonlite", "cli"),
  seed = 20261110L
)

# ---- Configuration ------------------------------------------------------

assessment_year <- 2025L   # latest published SAG assessment year
n_stocks        <- 10L     # stocks sampled for the detailed QA

# ---- Pipeline -----------------------------------------------------------

list(
  tar_target(year, assessment_year),
  
  # 1. Ingest ---------------------------------------------------------------
  tar_target(
    stock_list,
    fetch_stock_list(year),
    # Polite caching: do not re-download if re-run within a day.
    cue = tar_cue_age(name = stock_list, age = as.difftime(1, units = "days"))
  ),
  
  tar_target(
    assessment_keys,
    stock_list |>
      dplyr::filter(!is.na(.data$AssessmentKey)) |>
      dplyr::slice_sample(n = n_stocks) |>
      dplyr::pull(.data$AssessmentKey)
  ),
  
  tar_target(summary_tbl, fetch_summary_table(assessment_keys)),
  tar_target(ref_points, fetch_reference_points(assessment_keys)),
  
  # 2. Structural QA ---------------------------------------------------------
  # Note: columns that are entirely empty in the JSON payload arrive as
  # logical (all-NA) vectors, so numeric-or-logical is the honest
  # expectation for SAG series columns. The missing-value check is what
  # actually judges emptiness.
  tar_target(
    qa_struct_stock_list,
    run_structural_qa(
      stock_list,
      required = c(
        AssessmentKey = "numeric|integer",
        StockKeyLabel = "character",
        AssessmentYear = "numeric|integer",
        SpeciesName = "character"
      ),
      key_cols = "AssessmentKey"
    )
  ),
  
  tar_target(
    qa_struct_summary,
    run_structural_qa(
      summary_tbl,
      required = c(
        AssessmentKey = "numeric|integer",
        Year = "numeric|integer",
        F = "numeric|logical",
        SSB = "numeric|logical",
        Catches = "numeric|logical"
      ),
      key_cols = c("AssessmentKey", "Year"),
      na_cols = c("F", "SSB", "Catches")
    )
  ),
  
  # 3. Statistical QA --------------------------------------------------------
  tar_target(qa_stat_summary, run_summary_statistical_qa(summary_tbl)),
  
  # 4. Timeliness QA ---------------------------------------------------------
  tar_target(
    qa_time_summary,
    bind_qa(
      check_recency(summary_tbl, "Year", max_lag = 3),
      check_group_continuity(summary_tbl, "Year", "AssessmentKey")
    )
  ),
  
  # 5. Consolidation ---------------------------------------------------------
  tar_target(
    qa_all,
    bind_qa(qa_struct_stock_list, qa_struct_summary, qa_stat_summary, qa_time_summary)
  ),
  
  tar_target(qa_overview, summarise_qa(qa_all)),
  tar_target(qa_status, overall_status(qa_all))
)

# 6. Report ----------------------------------------------------------------
tar_target(
  report,
  {
    quarto::quarto_render("report/qa-report.qmd")
    "report/qa-report.html"
  },
  format = "file"
)