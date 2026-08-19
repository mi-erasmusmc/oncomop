test_that("checkConceptIntersection works on stages concept sets", {

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

  stages_intersection <- checkConceptIntersection(cdm, concept_folder = "stages")
  expect_true(stages_intersection)

})

test_that("vizConceptIntersection works with custom parameters", {

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
    vizConceptIntersection(
      cdm,
      input_subcategory = "N2")
  )

  # Visualize all available codelists for M1 from 7th edition
  expect_no_error(
    vizConceptIntersection(
      cdm,
      input_subcategory = "M1",
      input_edition = "7th"
    )
  )

  # Visualize all available codelists for T4 of clinical type
  expect_no_error(
    vizConceptIntersection(
      cdm,
      input_subcategory = "T4",
      input_classification = "clinical"
    )
  )

})


test_that("shinyConceptIntersection opens up correctly", {

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

  shinyConceptIntersection(cdm)

})
