test_that("Extract concept ids from filtered codelist", {
  testName <- "stages_ts_ns_ms"
  cdmVersion <- "5.4"
  cdm <- TestGenerator::patientsCDM(
    testName = testName,
    vocabulary = "v20260227_complete",
    cdmVersion = cdmVersion
  )
  concept_ids <- extractInnerConcepts(
    cdm,
    "stages"
  ) |>
    filterStageConcepts() |> 
    extractConceptIds() |>
    pull(concept_id)

  concept_ids |> 
    expect_length(99)

  # not unique
  concept_ids |>
    unique() |> 
    expect_length(68)

})

test_that("check error works when invalid category; check valid categories", {
  testName <- "stages_patients_one_patient"
  cdm <- TestGenerator::patientsCDM(
    testName = testName,
    vocabulary = "v20260227_complete",
    cdmVersion = "5.4"
  )
  expect_error({
    codelist <- extractInnerConcepts(
      cdm,
      path = "grade"
    )
  }, 
  class = "Invalid characteristics"
  )
  characteristics <- c(
    "biomarkers",
    "performance_status",
    "stages"
  )
  for (i in seq_along(characteristics)) {
    expect_no_error({
      codelist <- extractInnerConcepts(
        cdm,
        path = characteristics[i]
      )
    })
    expect_no_error({
      cdm[[characteristics[i]]] <- conceptCohort(
        cdm,
        conceptSet = codelist,
        name = characteristics[i],
        exit = "event_start_date"
      )
    })
  }
})
