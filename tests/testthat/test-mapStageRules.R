test_that("Stage mapping preserves patients with missing or unmatched TNM", {
  # Setup -------------------------------
  cohort <- tibble::tribble(
    ~subject_id, ~t0, ~t1, ~t2, ~n0, ~n1, ~m0,
    1L, NA, 0, NA, 0, NA, 0,
    2L, NA, NA, NA, NA, NA, NA,
    3L, 0, NA, NA, 0, NA, 0,
    4L, NA, 0, 1, 0, NA, NA,
    5L, NA, 0, 1, 0, NA, 0,
    6L, NA, NA, NA, 0, 1, 0,
    7L, NA, 0, 1, NA, NA, 0,
    8L, NA, 2, 0, 0, NA, 0,
    9L, NA, -1, 2, 0, NA, 0
  ) |>
    dplyr::mutate(
      cohort_definition_id = 1L,
      cohort_start_date = as.Date("2020-01-01"),
      cohort_end_date = as.Date("2020-01-01")
    )
  cdm <- omopgenerics::cdmFromTables(
    tables = list(
      person = tibble::tibble(
        person_id = cohort$subject_id,
        gender_concept_id = 0L,
        year_of_birth = 1970L,
        race_concept_id = 0L,
        ethnicity_concept_id = 0L
      ),
      observation_period = tibble::tibble(
        observation_period_id = cohort$subject_id,
        person_id = cohort$subject_id,
        observation_period_start_date = as.Date("2019-01-01"),
        observation_period_end_date = as.Date("2021-01-01"),
        period_type_concept_id = 0L
      )
    ),
    cdmName = "stage_mapping",
    cdmVersion = "5.4",
    cohortTables = list(cancer_cohorts = cohort)
  )
  ruleset <- readStagesRDS("mapping") |>
    extractStageRuleset(
      .cancer = "breast",
      .edition = "8th",
      .type = "base"
    )

  # Test ---------------------------------
  result <- .mapStageRules(cdm$cancer_cohorts, ruleset)
  expect_equal(result$subject_id, cohort$subject_id)
  expect_equal(
    result$cancer_stage,
    c(
      "IA", "no_stage_found", "no_stage_found",
      "no_stage_found", "IA", "no_stage_found",
      "no_stage_found", "IIA", "IA"
    )
  )

  # No stage concepts anywhere in the cohort ----------
  result <- cdm$cancer_cohorts |>
    dplyr::filter(.data$subject_id == 2L) |>
    .mapStageRules(ruleset)
  expect_equal(result$subject_id, 2L)
  expect_equal(result$cancer_stage, "no_stage_found")

  # Both oesophageal histologies map T1 N0 M0 to stage I.
  oesophageal_ruleset <- readStagesRDS("mapping") |>
    extractStageRuleset(
      .cancer = "oesophageal",
      .edition = "8th",
      .type = "clinical"
    )
  expect_equal(
    sum(oesophageal_ruleset$T == "T1" &
      oesophageal_ruleset$N == "N0" &
      oesophageal_ruleset$M == "M0"),
    2L
  )
  result <- cdm$cancer_cohorts |>
    dplyr::filter(.data$subject_id == 1L) |>
    .mapStageRules(oesophageal_ruleset)
  expect_equal(result$cancer_stage, "I")
})
