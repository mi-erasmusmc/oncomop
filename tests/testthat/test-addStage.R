test_that("addStage() insert column with last record", {
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
  stage_inner_concepts <- extractInnerConcepts(
    cdm,
    "stages"
    ) |>
    filterStageConcepts()
  cdm$cancer_cohorts |>
    addStages(
      cdm,
      stageConcepts = stage_inner_concepts,
      name = "cancer_stage_concepts"
    ) |>
    dplyr::collect() |>
    dplyr::pull(stages) |>
    # Provided list of concepts overlap
    expect_in(
      c(
      "ajcc_uicc_7th_m1_category_m90_to_90",
      "ajcc_uicc_7th_clinical_m1_category_m90_to_90"
      )
    )
})

test_that("Stages is added to cancer cohort", {
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
  stage_concepts <- extractInnerConcepts(
    cdm,
    "stages"
    ) |>
    filterStageConcepts()
  cdm$cancer_cohorts |>
    addStages(
      cdm,
      stageConcepts = stage_concepts,
      name = "cancer_stage_concepts"
    ) |>
      dplyr::collect() |>
      dplyr::pull(stages) |>
      expect_in(
        c("ajcc_uicc_m1_category_m90_to_90",
          "ajcc_uicc_7th_m1_category_m90_to_90",
          "ajcc_uicc_7th_clinical_m1_category_m90_to_90")
      )
})

test_that("Filter children concept sets after extraction", {
  testName <- "stages_patients_one_patient"
  cdm <- TestGenerator::patientsCDM(
    testName = testName,
    vocabulary = "v20260227_complete",
    cdmVersion = "5.4"
  )
  codelist <- extractInnerConcepts(
    cdm,
    path = "stages"
  )
  codelist |>
    filterStageConcepts()
})

test_that("Adding stages to patients with UICC 7th TNM measurements", {

  testName <- "test_patients_staging_uicc7"

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

  codelist <- extractInnerConcepts(
    cdm,
    path = "stages"
  )

  stage_concepts <- codelist |>
    filterStageConcepts()

  cdm$cancer_stage_concepts <- cdm$cancer_cohorts |>
    addStages(
      cdm,
      stageConcepts = stage_concepts,
      name = "cancer_stage_concepts"
    )

  # Expectations
  cdm$cancer_stage_concepts |>
    dplyr::count(stages)

  stages_overview <- cdm$cancer_stage_concepts |>
    dplyr::collect() |>
    dplyr::mutate(
      edition = dplyr::case_when(
        stringr::str_detect(stages, "7th") ~ "7th",
        stringr::str_detect(stages, "8th") ~ "8th",
        TRUE ~ "unspecified"
      ),
      macro_stage = stringr::str_extract(
        stages,
        "(?<=ajcc_uicc_(?:(?<edition>[78]th)_)?(?:(?<classification>clinical|pathological)_)?)(?<stage>[tnm])"
      ),
      micro_stage = stringr::str_extract(
        stages,
        "(?<=ajcc_uicc_(?:(?<edition>[78]th)_)?(?:(?<classification>clinical|pathological)_)?)(?<stage>[tnm][0-9]*[a-z]*)"
      )
    )

  stages_edition <- stages_overview |>
    dplyr::distinct(edition) |>
    dplyr::pull()
  expect_true(all(stages_edition %in% c("7th", "unspecified")))

  stages_overview |>
    dplyr::count()

  stages_overview |>
    dplyr::count(micro_stage)
})

test_that("Adding stages to patients with UICC 8th TNM measurements", {

  testName <- "test_patients_staging_uicc8"

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

  codelist <- extractInnerConcepts(
    cdm,
    path = "stages"
  )

  stage_concepts <- codelist |>
    filterStageConcepts()

  cdm$cancer_stage_concepts <- cdm$cancer_cohorts |>
    addStages(
      cdm,
      stageConcepts = stage_concepts,
      name = "cancer_stage_concepts"
    )

  # Expectations
  cdm$cancer_stage_concepts |>
    dplyr::count(stages)

  stages_overview <- cdm$cancer_stage_concepts |>
    dplyr::collect() |>
    dplyr::mutate(
      edition = dplyr::case_when(
        stringr::str_detect(stages, "7th") ~ "7th",
        stringr::str_detect(stages, "8th") ~ "8th",
        TRUE ~ "unspecified"
      ),
      macro_stage = stringr::str_extract(
        stages,
        "(?<=ajcc_uicc_(?:(?<edition>[78]th)_)?(?:(?<classification>clinical|pathological)_)?)(?<stage>[tnm])"
      ),
      micro_stage = stringr::str_extract(
        stages,
        "(?<=ajcc_uicc_(?:(?<edition>[78]th)_)?(?:(?<classification>clinical|pathological)_)?)(?<stage>[tnm][0-9]*[a-z]*)"
      )
    )

  stages_edition <- stages_overview |>
    dplyr::distinct(edition) |>
    dplyr::pull()
  expect_true(all(stages_edition %in% c("8th", "unspecified")))

  stages_overview |>
    dplyr::count()

  stages_overview |>
    dplyr::count(micro_stage)
})

test_that("Adding stages to patients from broader test set", {

  testName <- "test_patients_staging_general"

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

  codelist <- extractInnerConcepts(
    cdm,
    path = "stages"
  )

  stage_concepts <- codelist |>
    filterStageConcepts()

  cdm$cancer_stage_concepts <- cdm$cancer_cohorts |>
    addStages(
      cdm,
      stageConcepts = stage_concepts,
      name = "cancer_stage_concepts"
    )

  # Expectations
  cdm$cancer_stage_concepts |>
    dplyr::count(stages)

  stages_overview <- cdm$cancer_stage_concepts |>
    dplyr::collect() |>
    dplyr::mutate(
      edition = dplyr::case_when(
        stringr::str_detect(stages, "7th") ~ "7th",
        stringr::str_detect(stages, "8th") ~ "8th",
        TRUE ~ "unspecified"
      ),
      macro_stage = stringr::str_extract(
        stages,
        "(?<=ajcc_uicc_(?:(?<edition>[78]th)_)?(?:(?<classification>clinical|pathological)_)?)(?<stage>[tnm])"
      ),
      micro_stage = stringr::str_extract(
        stages,
        "(?<=ajcc_uicc_(?:(?<edition>[78]th)_)?(?:(?<classification>clinical|pathological)_)?)(?<stage>[tnm][0-9]*[a-z]*)"
      )
    )

  stages_edition <- stages_overview |>
    dplyr::distinct(edition) |>
    dplyr::pull()
  expect_true(all(stages_edition %in% c("7th", "8th", "unspecified")))

  stages_overview |>
    dplyr::count()

  stages_overview |>
    dplyr::count(micro_stage)
})

test_that("Overlap of TNM codes", {

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

  codelist <- extractInnerConcepts(
    cdm,
    path = "stages"
  )

  stage_concepts <- codelist |>
    filterStageConcepts()

  # Sample overlap of codelists
  overlap <- length(intersect(codelist$`AJCC/UICC 7th M0 Category`, codelist$`AJCC/UICC 8th M0 Category`))
  expect_equal(overlap, 0)    # no overlap
  overlap <- length(intersect(codelist$`AJCC/UICC clinical M0 Category`, codelist$`AJCC/UICC 7th clinical M0 Category`))
  expect_false(overlap == 0)  # OVERLAP
  overlap <- length(intersect(codelist$`AJCC/UICC clinical M0 Category`, codelist$`AJCC/UICC 8th clinical M0 Category`))
  expect_false(overlap == 0)  # OVERLAP

  # Sample overlap of stage_concepts
  overlap <- length(intersect(stage_concepts$`AJCC/UICC 7th M1 Category`, stage_concepts$`AJCC/UICC 8th M1 Category`))
  expect_equal(overlap, 0)    # no overlap
  overlap <- length(intersect(stage_concepts$`AJCC/UICC N2 Category`, stage_concepts$`AJCC/UICC 7th N2 Category`))
  expect_equal(overlap, 0)    # no overlap
  overlap <- length(intersect(stage_concepts$`AJCC/UICC N2 Category`, stage_concepts$`AJCC/UICC 8th N2 Category`))
  expect_equal(overlap, 0)    # no overlap
})
