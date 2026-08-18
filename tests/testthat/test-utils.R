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
