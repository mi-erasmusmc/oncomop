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
      cancer = "breast",
      window = list(c(0,0)),
      edition = "8th",
      type = "base",
      order = "last"
    ) |>
    dplyr::collect() |>
    dplyr::pull(cancer_stages) |>
    # Provided list of concepts overlap
    expect_in(
      c(
      "ajcc_uicc_7th_m1_category_m90_to_90",
      "ajcc_uicc_7th_clinical_m1_category_m90_to_90",
      "ajcc_uicc_clinical_m1_category_m90_to_90",
      "ajcc_uicc_clinical_m1_category_m90_to_90"
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
          "ajcc_uicc_clinical_m1_category_m90_to_90",
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
  expect_no_error({
    codelist |>
      filterStageConcepts()
    })
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

  # Check overlap of codelists
  overlap <- length(intersect(codelist$`AJCC/UICC 7th M0 Category`, codelist$`AJCC/UICC 8th M0 Category`))
  expect_equal(overlap, 0)    # no overlap
  overlap <- length(intersect(codelist$`AJCC/UICC clinical M0 Category`, codelist$`AJCC/UICC 7th clinical M0 Category`))
  expect_false(overlap == 0)  # OVERLAP
  overlap <- length(intersect(codelist$`AJCC/UICC clinical M0 Category`, codelist$`AJCC/UICC 8th clinical M0 Category`))
  expect_false(overlap == 0)  # OVERLAP

  intersection_codelists <- outer(
    names(codelist),
    names(codelist),
    Vectorize(function(x, y) {
      length(intersect(codelist[[x]], codelist[[y]]))
      })
    )
  diag(intersection_codelists) <- 0
  expect_true(any(intersection_codelists > 0)) # there is overall overlap of codelists

  # Check overlap of stage_concepts
  overlap <- length(intersect(stage_concepts$`AJCC/UICC 7th M1 Category`, stage_concepts$`AJCC/UICC 8th M1 Category`))
  expect_equal(overlap, 0)    # no overlap
  overlap <- length(intersect(stage_concepts$`AJCC/UICC N2 Category`, stage_concepts$`AJCC/UICC 7th N2 Category`))
  expect_false(overlap == 0)  # OVERLAP
  overlap <- length(intersect(stage_concepts$`AJCC/UICC N2 Category`, stage_concepts$`AJCC/UICC 8th N2 Category`))
  expect_false(overlap == 0)  # OVERLAP

  intersection_stage_concepts <- outer(
    names(stage_concepts),
    names(stage_concepts),
    Vectorize(function(x, y) {
      length(intersect(stage_concepts[[x]], stage_concepts[[y]]))
    })
  )
  diag(intersection_stage_concepts) <- 0
  expect_true(any(intersection_stage_concepts > 0)) # there is overall overlap of stage_concepts

  # Check relation between codelists and stage_concepts
  expect_equal(length(codelist), 134)
  expect_equal(length(stage_concepts), 99)
  expect_equal(intersect(codelist, stage_concepts), stage_concepts)  # stage_concepts is a subset of codelist
  expect_equal(length(setdiff(codelist, stage_concepts)), 35)        # codelist has additional concepts for NX, Ta, Tis, TX
})

# Inner core functions -----------------------------

test_that("read stages rds", {

  tnm_files <- c(
    "tnm_concepts",
    "tnm_stage_mapping",
    "tnm_stage_shortcut_mapping"
  )

  tnm_files <- system.file(
    "tnm_files",
    package = "oncomop"
  ) |> 
    list.files(
      full.names = TRUE
    ) |>
    readStagesRDS() |> 
    names() |> 
    expect_equal(
      c("tnm_concepts.rds", 
        "tnm_stage_mapping.rds",
        "tnm_stage_shortcut_mapping.rds"
      )
    )
})

test_that("to form tnmCodelist", {
  tnm_files_data <- system.file(
    "tnm_files",
    package = "oncomop"
  ) |> 
    list.files(
      full.names = TRUE
    ) |>
    readStagesRDS() 

  expect_no_error({
    tnm_codelist <- tnm_files_data$tnm_concepts |>
      createTNMCodelist(
        .edition = "7th",
        .type = "clinical"
      ) 
    })
  tnm_codelist |> 
    expect_length(42)
})

test_that("General .addColumnsRules", {
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

  tnm_files_data <- system.file(
    "tnm_files",
    package = "oncomop"
  ) |> 
    list.files(
      full.names = TRUE
    ) |>
    readStagesRDS() 

  tnm_codelist <- tnm_files_data$tnm_concepts |>
    createTNMCodelist(
      .edition = "7th",
      .type = "clinical"
    ) 

  cdm$cancer_cohorts |> 
    .addColumnsRules(
      conceptSet = tnm_codelist,
      window = list(c(0, 0))
    ) |> 
    colnames() |> 
    expect_equal(
      c("cohort_definition_id", "subject_id", "cohort_start_date", 
        "cohort_end_date", "m0", "m1", "m1a", "m1b", "m1c", "m1d", "n0", 
        "n1", "n1a", "n1b", "n1c", "n1mi", "n2", "n2a", "n2b", "n2c", 
        "n3", "n3a", "n3b", "n3c", "nx", "t0", "t1", "t1a", "t1b", "t1c", 
        "t1mi", "t2", "t2a", "t2b", "t2c", "t3", "t3a", "t3b", "t4", 
        "t4a", "t4b", "t4c", "t4d", "ta", "tis", "tx")
      )

})

test_that("Extracting and formating rules from RDS data'", {

  tnm_files_data <- system.file(
    "tnm_files",
    package = "oncomop"
  ) |> 
    list.files(
      full.names = TRUE
    ) |>
    readStagesRDS() 
  
  tnm_files_data$tnm_stage_mapping |> 
    extractStageRuleset(
      .cancer = "breast",
      .edition = "8th",
      .type = "base"
    ) |> 
    names() |> 
    expect_equal(
      c("rule_id", "T", "N", "M", "uicc_stage")
    )

})
test_that("Imposing rules with 'mappingRules()'", {

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

  tnm_files_data <- system.file(
    "tnm_files",
    package = "oncomop"
  ) |> 
    list.files(
      full.names = TRUE
    ) |>
    readStagesRDS() 

  tnm_codelist <- tnm_files_data$tnm_concepts |>
    createTNMCodelist(
      .edition = "7th",
      .type = "clinical"
    ) 
  
  ruleset <- tnm_files_data$tnm_stage_mapping |> 
    extractStageRuleset(
      .cancer = "breast",
      .edition = "8th",
      .type = "base"
    ) 

  cdm$cancer_cohorts |> 
    .addColumnsRules(
      conceptSet = tnm_codelist,
      window = list(c(0, 0))
    ) |> 
      .mapRules(ruleset)

})