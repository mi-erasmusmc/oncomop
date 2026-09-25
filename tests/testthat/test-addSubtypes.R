test_that("addSubtype to correct breast cancer cohort", {
  expect_equal(2 * 2, 4)
})

test_that("assertCancerCohortName FUN", {
  testName <- "default_rules_multiple_subjects"
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
  expect_no_error({
    cdm$cancer_cohorts |>
      assertCancerCohortName(
        cancer= "breast"
      )
    })
  expect_no_error({
    cdm$cancer_cohorts |>
      assertCancerCohortName(
        cancer = supportedCancerSites()
      )
    })
  expect_error({
    cdm$cancer_cohorts |>
      assertCancerCohortName(
        cancer = "bladder"
      )
    },
    class = "Invalid cohort names"
  )

})
