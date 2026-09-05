test_that("addStage() insert cancer column with last record multiple subjects", {
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

  # -------------------- Oncomop starts here

  cdm$cancer_cohorts |>
    addStages(
      cdm,
      cancer = "breast",
      window = list(c(0,0)),
      edition = "8th",
      type = "clinical",
      order = "last",
      showTnm = FALSE
    ) |>
    dplyr::pull(cancer_stage) |>
    expect_in(c("IV", "IA", "IIIC"))
})

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
      .edition = "8th",
      .type = "clinical"
    )
  ruleset <- tnm_files_data$tnm_stage_mapping |>
    extractStageRuleset(
      .cancer = "breast",
      .edition = "8th",
      .type = "base"
    )

  cdm$cancer_cohorts |>
    .addColumnRules(
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
      .edition = "8th",
      .type = "clinical"
    )
  ruleset <- tnm_files_data$tnm_stage_mapping |>
    extractStageRuleset(
      .cancer = "breast",
      .edition = "8th",
      .type = "base"
    )

  cdm$cancer_cohorts |>
    .addColumnRules(
      conceptSet = tnm_codelist,
      window = list(c(0, 0)),
      ruleset = ruleset
    ) |>
    dplyr::pull(cancer_stage) |>
    expect_equal("IA")

})

test_that("General .addColumnRules", {

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
      .edition = "8th",
      .type = "clinical"
    )

  ruleset <- tnm_files_data$tnm_stage_mapping |>
    extractStageRuleset(
      .cancer = "breast",
      .edition = "8th",
      .type = "base"
    )

  cdm$cancer_cohorts |>
    .addColumnRules(
      conceptSet = tnm_codelist,
      window = list(c(0, 0)),
      ruleset = ruleset
    ) |>
    collect() |>
    pull(t1) |>
    expect_equal(
      as.Date("2023-01-15")
    )

  cdm$cancer_cohorts |>
    .addColumnRules(
      conceptSet = tnm_codelist,
      window = list(c(0, 0)),
      ruleset = ruleset
    ) |>
    collect() |>
    pull(n0) |>
    expect_equal(
      as.Date("2023-01-15")
    )

  cdm$cancer_cohorts |>
    .addColumnRules(
      conceptSet = tnm_codelist,
      window = list(c(0, 0)),
      ruleset = ruleset
    ) |>
    collect() |>
    pull(m0) |>
    expect_equal(
      as.Date("2023-01-15")
    )
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
      cdm,
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
