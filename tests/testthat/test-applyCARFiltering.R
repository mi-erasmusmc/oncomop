test_that("applyCARFiltering works", {

  # --- Set-up
  cdmVersion <- "5.4"
  test_name <- "test_patients_CAR_filtering"

  cdm <- TestGenerator::patientsCDM(
    testName = test_name,
    vocabulary = "v20260227_complete",
    cdmVersion = cdmVersion
  )

  # --- Test default filtering

  dbname = "DK-DHR"

  conditions_before <- cdm[["condition_occurrence"]] |>
    dplyr::distinct(condition_status_concept_id) |>
    dplyr::pull()
  n_subjects_before <- cdm[["condition_occurrence"]] |>
    dplyr::distinct(person_id) |>
    dplyr::collect() |>
    nrow()

  cdm <- applyCARFiltering(cdm, dbname)

  conditions_after <- cdm[["condition_occurrence"]] |>
    dplyr::distinct(condition_status_concept_id) |>
    dplyr::pull()
  n_subjects_after <- cdm[["condition_occurrence"]] |>
    dplyr::distinct(person_id) |>
    dplyr::collect() |>
    nrow()

  expect_equal(n_subjects_before, 2)
  expect_equal(conditions_before, c(4203942, 32893))
  expect_equal(n_subjects_after, 1)
  expect_equal(conditions_after, 32893)

})
