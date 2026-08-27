test_that("assertUniqueConcepts checks stages concept sets", {

  testName <- "stages_patients_one_patient"

  cdm <- TestGenerator::patientsCDM(
    testName = testName,
    vocabulary = "v20260227_complete",
    cdmVersion = "5.4"
  )

  cdm <- createCancerCohorts(
    cdm,
    path = "cancer_cohorts",
    name = "cancer_cohorts"
  )

  expect_warning(
    assertUniqueConcepts(cdm, concept_folder = "stages")
  )
})


test_that("vizConceptDuplicates works with custom parameters", {

  testName <- "stages_patients_one_patient"

  cdm <- TestGenerator::patientsCDM(
    testName = testName,
    vocabulary = "v20260227_complete",
    cdmVersion = "5.4"
  )

  cdm <- createCancerCohorts(
    cdm,
    path = "cancer_cohorts",
    name = "cancer_cohorts"
  )

  # Visualize all available codelists for N2
  expect_no_error(
    vizConceptDuplicates(
      cdm,
      input_subcategory = "N2")
  )

  # Visualize all available codelists for M1 from 7th edition
  expect_no_error(
    vizConceptDuplicates(
      cdm,
      input_subcategory = "M1",
      input_edition = "7th"
    )
  )

  # Visualize all available codelists for T4 of clinical type
  expect_no_error(
    vizConceptDuplicates(
      cdm,
      input_subcategory = "T4",
      input_classification = "clinical"
    )
  )

})


test_that("shinyConceptDuplicates opens up correctly", {

  testName <- "stages_patients_one_patient"

  cdm <- TestGenerator::patientsCDM(
    testName = testName,
    vocabulary = "v20260227_complete",
    cdmVersion = "5.4"
  )

  cdm <- createCancerCohorts(
    cdm,
    path = "cancer_cohorts",
    name = "cancer_cohorts"
  )

  expect_no_error(
    shinyConceptDuplicates(cdm)
  )
})
