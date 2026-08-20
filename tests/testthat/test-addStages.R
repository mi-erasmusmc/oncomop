test_that("add stages", {
  testName <- "stages_ts_ns_ms"
  cdm <- TestGenerator::patientsCDM(
    testName = testName,
    vocabulary = "v20260227_complete"
  )
  cdm <- createCancerCohorts(
    cdm = cdm,
    name = "test_cancer_cohorts"
  )
  stageCodelist <- extractInnerConcepts(
    cdm = cdm,
    path = stages
  ) 

  cdm$test_cancer_cohorts |> 
    addStages(
      cdm
    )

})
  