test_that("assertCancerCohortName FUN", {
  testName <- "stages_baseline"
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
        cancer = "breast"
      )
    })
  expect_error({
    cdm$cancer_cohorts |>
      assertCancerCohortName(
        cancer = supportedCancerSites()
      )
    }, 
   class = "Invalid cohort names"
  )
  expect_error({
    cdm$cancer_cohorts |>
      assertCancerCohortName(
        cancer = "bladder"
      )
    },
    class = "Invalid cohort names"
  )
})

test_that("requirePackage checks package availability", {
  expect_true(requirePackage("base"))
  expect_error(
    requirePackage("oncomop-package-that-does-not-exist"),
    class = "missing_package"
  )
})

test_that("stage assertions return data or identify missing inputs", {
  codelist <- list(T1 = 1L)
  ruleset <- data.frame(rule_id = 1L)

  expect_identical(
    assertTNMCodelist(codelist, "breast", "clinical", "8th"),
    codelist
  )
  expect_identical(
    assertStageRuleset(ruleset, "breast", "clinical", "8th"),
    ruleset
  )
  expect_error(
    assertTNMCodelist(list(), "breast", "clinical", "8th"),
    "No TNM concepts found for 'breast cancer', 'clinical', '8th edition'",
    fixed = TRUE
  )
  expect_error(
    assertStageRuleset(ruleset[0, , drop = FALSE], "breast", "clinical", "8th"),
    "No TNM mapping found for 'breast cancer', 'clinical', '8th edition'",
    fixed = TRUE
  )
})

# test_that("filters surgery codelist by cancer", {
#   testName <- "stages_patients_one_patient"
#   cdmVersion <- "5.4"
#   cdm <- TestGenerator::patientsCDM(
#     testName = testName,
#     vocabulary = "v20260227_complete",
#     cdmVersion = cdmVersion
#   )
#
#   codelist <- system.file(
#     "concept_sets",
#     "surgery",
#     package = "P4C5006"
#     ) |>
#     omopgenerics::importConceptSetExpression() |>
#     CodelistGenerator::asCodelist(cdm)
#
#   cancerSpecificCodelist <- filterCodelist(
#     codelist,
#     pattern = "bladder"
#   ) |>
#     omopgenerics::assertList() |>
#     names() |>
#     expect_equal(
#       "bladder_cancer_surgery"
#     )
# })
#
# test_that("filters treatments codelist by cancer", {
#   testName <- "stages_patients_one_patient"
#   cdmVersion <- "5.4"
#   cdm <- TestGenerator::patientsCDM(
#     testName = testName,
#     vocabulary = "v20260227_complete",
#     cdmVersion = cdmVersion
#   )
#   codelist <- system.file(
#     "concept_sets",
#     "treatments_procedures",
#     package = "P4C5006"
#     ) |>
#     omopgenerics::importConceptSetExpression() |>
#     CodelistGenerator::asCodelist(cdm)
#
#   result_codelist <- filterCodelist(
#     codelist,
#     pattern = "bladder"
#   ) |>
#     omopgenerics::assertList() |>
#     names() |>
#     expect_equal(
#       c(
#         "bladder_cancer_chemotherapy",
#         "bladder_cancer_immunotherapy",
#         "bladder_cancer_other_therapy",
#         "bladder_cancer_targeted_therapy"
#       )
#     )
# })
