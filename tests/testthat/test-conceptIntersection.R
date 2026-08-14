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

  expect_no_error(
    vizConceptIntersection(cdm)
  )

  expect_no_error(
    vizConceptIntersection(
      cdm,
      input_subcategory = "N1",
      input_edition = "7th"
    )
  )

  expect_no_error(
    vizConceptIntersection(
      cdm,
      input_subcategory = "T3",
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
