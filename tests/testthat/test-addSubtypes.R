test_that("addSubtype to correct breast cancer cohort", {
  testName <- "default_rules_multiple_subjects"
  TestGenerator::patientsCDM(
    testName = testName,
    vocabulary = "v20260227_complete",
    cdmVersion = "5.4"
  ) |>
    createCancerCohorts(
      path = "cancer_cohorts",
      name = "cancer_cohorts"
    ) |> 
    addSubtypes(
      cohort = "cancer_cohorts",
      cdm = _,
      cancer = "breast",
      window = list(c(-90 ,90)),
      showIntersect = TRUE
    ) |> 
    expect_no_error()
})

test_that("create subtypeCodelist correctly", {
  testName <- "default_rules_multiple_subjects"
  TestGenerator::patientsCDM(
    testName = testName,
    vocabulary = "v20260227_complete",
    cdmVersion = "5.4"
  ) |> 
    subtypeCodelist(
      concepts = c(35957667L, 35948983L, 35955862L),
      cdm = _
    ) |> 
    names() |> 
    expect_identical(
    c("erbb2_erb-b2_receptor_tyrosine_kinase_2_gene_variant_measurement",
      "esr1_estrogen_receptor_1_gene_variant_measurement", 
      "pgr_progesterone_receptor_gene_variant_measurement"
    )
  )
})

test_that("extractConceptName correctly", {
  testName <- "default_rules_multiple_subjects"
  TestGenerator::patientsCDM(
    testName = testName,
    vocabulary = "v20260227_complete",
    cdmVersion = "5.4"
  ) |> 
  lapply(
    c(35957667L, 35948983L, 35955862L),
    extractConceptName,
    cdm = _
  ) |> 
    unlist() |> 
    expect_identical(
    c("pgr_progesterone_receptor_gene_variant_measurement",
      "esr1_estrogen_receptor_1_gene_variant_measurement", 
      "erbb2_erb-b2_receptor_tyrosine_kinase_2_gene_variant_measurement"
    )
  )
})

# test_that(".mapStageRules correct breast cancer cohort", {
#   testName <- "default_rules_multiple_subjects"
#   cdm <- TestGenerator::patientsCDM(
#     testName = testName,
#     vocabulary = "v20260227_complete",
#     cdmVersion = "5.4"
#   ) |>
#     createCancerCohorts(
#       path = "cancer_cohorts",
#       name = "cancer_cohorts"
#     ) 
#   cdm$cancer_cohorts |>
#     PatientProfiles::addConceptIntersectDate(
#       conceptSet,
#       indexDate = "cohort_start_date",
#       censorDate = NULL,
#       window = window,
#       targetDate = "event_start_date",
#       order = "last",
#       inObservation = TRUE,
#       nameStyle = "{concept_name}",
#       name = NULL
#     ) |> 
#     .mapSubtypeRules(ruleset)
# })


