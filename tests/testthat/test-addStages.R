test_that("FinOmop breast cancer issue", {
  # Setup -------------------------------
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
  
  # No pathological rule for breast cancer
  readStagesRDS("mapping") |>
    extractStageRuleset(
      .cancer = "breast",
      .edition = "8th",
      .type = "pathological"
    ) |> 
    nrow() |> 
    expect_equal(0)

  # only base
  readStagesRDS("mapping") |>
    extractStageRuleset(
      .cancer = "breast",
      .edition = "8th",
      .type = "base"
    ) |> 
    nrow() |> 
    expect_equal(348)
})

test_that("addStage() insert cancer column with last record multiple subjects", {
  # Setup -------------------------------
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

  # Oncomop execution --------------------
  result_stages <- cdm$cancer_cohorts |>
    addStages(
      cancer = "breast",
      window = list(c(0,0)),
      edition = "8th",
      type = "clinical",
      order = "last",
      showIntersect = TRUE
    )

  # Test ---------------------------------
  result_stages |>
    dplyr::pull(cancer_stage) |>
    sort() |>
    expect_in(
      sort(
        c("IV", "IA", "IIIC")
      )
    )

  # Summarised result --------------------
  result_stages_summarised <- result_stages |>
    PatientProfiles::summariseResult()

  # Test stages --------------------------
  result_stages_summarised |>
    dplyr::filter(
      variable_name == "cancer_stage",
      estimate_name == "count",
    ) |>
    pull(variable_level) |>
    sort() |>
    expect_in(
      sort(
        c("IV", "IA", "IIIC")
      )
    )
  # Test counts --------------------------
  result_stages_summarised |>
    dplyr::filter(
      variable_name == "cancer_stage",
      estimate_name == "count",
    ) |>
    pull(estimate_value) |>
    as.numeric() |>
    unique() |>
    expect_equal(1)
  # Test visOmopResults -------------------
  expect_no_error({
    result_stages_summarised |>
      visOmopResults::visOmopTable()
  })

})

test_that("1 patient 'no_stage_found'", {
  # Setup -------------------------------
  testName <- "1_patient_no_stage_found"
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
  # Oncomop execution --------------------
  result_stages <- cdm$cancer_cohorts |>
    addStages(
      cancer = "breast",
      window = list(c(0,0)),
      edition = "8th",
      type = "clinical",
      order = "last",
      showIntersect = TRUE
    )

  # Test ---------------------------------
  result_stages |>
    dplyr::pull(cancer_stage) |>
    sort() |>
    expect_in(
      sort(
        c("IA", "IIIC", "no_stage_found")
      )
    )
})

test_that("read stages rds", {
  readStagesRDS("concepts") |>
    names() |>
    expect_equal(
      c("component_tnm",
        "type",
        "classification_version",
        "concept_id",
        "note")
    )
  readStagesRDS("mapping") |>
    names() |>
    expect_equal(
      c("rule_id",
        "staging_system",
        "edition",
        "site",
        "stage_grouping_scope",
        "histology",
        "T",
        "N",
        "M",
        "uicc_stage",
        "source_file",
        "source_pages",
        "source_table",
        "notes")
    )
})

test_that("to form tnmCodelist", {
  expect_no_error({
    tnm_codelist <- readStagesRDS("concepts") |>
      createTNMCodelist(
        .edition = "7th",
        .type = "clinical"
      )
    })
  tnm_codelist |>
    expect_length(42)
})

test_that("Extracting and formating rules from RDS data'", {
  readStagesRDS("mapping") |>
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

test_that("Imposing rules with 'mappingRules' multiple subjects", {
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
  tnm_codelist <- readStagesRDS("concepts") |>
    createTNMCodelist(
      .edition = "8th",
      .type = "clinical"
    )
  ruleset <- readStagesRDS("mapping") |>
    extractStageRuleset(
      .cancer = "breast",
      .edition = "8th",
      .type = "base"
    )
  cdm$cancer_cohorts |>
    .addStageRules(
      conceptSet = tnm_codelist,
      window = list(c(0, 0)),
      ruleset = ruleset
    ) |>
    dplyr::pull(cancer_stage) |>
    expect_in(c("IV", "IA", "IIIC"))
})

test_that("Imposing rules with 'mappingRules' single patient", {
  testName <- "default_rules_single_subject"
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
  tnm_codelist <- readStagesRDS("concepts") |>
    createTNMCodelist(
      .edition = "8th",
      .type = "clinical"
    )
  ruleset <- readStagesRDS("mapping")|>
    extractStageRuleset(
      .cancer = "breast",
      .edition = "8th",
      .type = "base"
    )
  cdm$cancer_cohorts |>
    .addStageRules(
      conceptSet = tnm_codelist,
      window = list(c(0, 0)),
      ruleset = ruleset
    ) |>
    dplyr::pull(cancer_stage) |>
    expect_equal("IA")
})

test_that("General .addStageRules", {
  testName <- "default_rules_single_subject"
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
  tnm_codelist <- readStagesRDS("concepts") |>
    createTNMCodelist(
      .edition = "8th",
      .type = "clinical"
    )
  ruleset <- readStagesRDS("mapping")|>
    extractStageRuleset(
      .cancer = "breast",
      .edition = "8th",
      .type = "base"
    )
  cdm$cancer_cohorts |>
    .addStageRules(
      conceptSet = tnm_codelist,
      window = list(c(0, 0)),
      ruleset = ruleset
    ) |>
    collect() |>
    pull(t1) |>
    expect_equal(0)
  cdm$cancer_cohorts |>
    .addStageRules(
      conceptSet = tnm_codelist,
      window = list(c(0, 0)),
      ruleset = ruleset
    ) |>
    collect() |>
    pull(n0) |>
    expect_equal(0)
  cdm$cancer_cohorts |>
    .addStageRules(
      conceptSet = tnm_codelist,
      window = list(c(0, 0)),
      ruleset = ruleset
    ) |>
    collect() |>
    pull(m0) |>
    expect_equal(0)
})

test_that("addStage() insert cancer column with last record one staging patient", {
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
  cdm$cancer_cohorts |>
    addStages(
      cancer = "bladder",
      window = list(c(0, 30)),
      edition = "7th",
      type = "clinical",
      order = "last"
    ) |>
    dplyr::collect() |>
    dplyr::pull(cancer_stage) |>
    expect_equal("IV")
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

test_that("Filter only T, N and M", {
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

test_that("9th edition prostate case from onconet", {
  cdm <- TestGenerator::patientsCDM(
    testName = "test_patients_staging_uicc7",
    vocabulary = "v20260227_complete",
    cdmVersion = "5.4"
  )
  cdm <- createCancerCohorts(
    cdm = cdm,
    path = "cancer_cohorts",
    name = "cancer_cohorts"
  )
  expect_invisible({
    cdm[["cancer_cohorts"]] |>
      CohortConstructor::subsetCohorts(
        cohortId = "prostate_cancer"
      ) |>
      addStages(
        cancer = "prostate",
        window = list(c(-90, 90)),
        edition = "9th",
        type = "base",
        order = "first",
        showIntersect = FALSE
      )
    })

})


test_that("7th and 8th edition ovarian case from VTE", {

  test_name <- "test_patients_stage_ovarian"

  cdm <- TestGenerator::patientsCDM(
    testName = test_name,
    vocabulary = "v20260227_complete",
    cdmVersion = "5.4"
  )

  cdm <- createCancerCohorts(
    cdm = cdm,
    path = "cancer_cohorts",
    name = "cancer_cohorts",
    cancer = "all"
  )

  cdm[["cancer_cohorts"]] |>
    CohortConstructor::subsetCohorts(
      cohortId = "ovarian_cancer"
    ) |>
    addStages(
      cancer = "ovarian",
      window = list(c(-90, 90)),
      edition = "7th",
      type = "base",
      order = "first",
      showIntersect = FALSE
    ) |>
    dplyr::pull(cancer_stage) |>
    expect_equal(c("I", "no_stage_found"))

  cdm[["cancer_cohorts"]] |>
    CohortConstructor::subsetCohorts(
      cohortId = "ovarian_cancer"
    ) |>
    addStages(
      cancer = "ovarian",
      window = list(c(-90, 90)),
      edition = "8th",
      type = "base",
      order = "first",
      showIntersect = FALSE
    ) |>
    dplyr::pull(cancer_stage) |>
    expect_equal(c("II", "no_stage_found"))

})


