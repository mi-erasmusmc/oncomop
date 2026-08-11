#------------------------ Unit tests createCancerCohorts -----------------------#

# Cancer types of interest and their ATLAS concept set id:
# - Bladder cancer (1937)
# - Breast cancer (4415)
# - Colorectal cancer (1444)
# - Esophageal cancer (1414)
# - Lung cancer (1413)
# - Prostate cancer (1489)
# - Skin melanoma (1451)

# Phenotype specifications:
# - Study period from 01-01-2010 to latest available data (end of observation period)
# - Date of first diagnosis of a selected type of cancer during the study period
# - Age >= 18 at the date of cancer diagnosis
# - Exclude patients with death before and at index date
# - Exclude males for breast cancer
# - Exclude females for prostate cancer
test_that("createCancerCohorts works with specific xlsx cancer files", {

  cdmVersion <- "5.4"

  cancer_types <- c(
    "bladder_cancer", "breast_cancer", "colorectal_cancer", "lung_cancer_optimal",
    "lung_cancer_broad", "melanoma_of_skin", "oesophageal_cancer", "prostate_cancer"
    )

  for (cancer_type in cancer_types) {
    # browser()
    print(cancer_type)

    test_name <- paste0("test_", cancer_type)
    TestGenerator::readPatients.xl(
      filePath = file.path(
        testthat::test_path(
          "excel_files",
          paste0(
            "test_cdm_5.4_",
            cancer_type,
            ".xlsx"
          )
        )
      ),
      testName = test_name
    )

    # create cdm instance with current cancer patients
    cdm <- TestGenerator::patientsCDM(
      pathJson = NULL,
      testName = test_name,
      cdmVersion = cdmVersion
      )

    # call createCancerCohorts to generate codelists and create cohorts
    cdm <- createCancerCohorts(
      cdm = cdm,
      path = "cancer_cohorts",
      name = "cancer_cohorts"
      )

    # extract cohort id
    cohort_id <- cdm$cancer_cohorts |>
      omopgenerics::settings() |>
      dplyr::filter(cohort_name == cancer_type) |>
      dplyr::pull(cohort_definition_id)

    # test number of patients in cdm instance
    cdm$person |>
      dplyr::collect() |>
      nrow() |>
      expect_equal(3)

    # test number of patients in cohort
    cdm$cancer_cohorts |>
      dplyr::collect() |>
      dplyr::filter(cohort_definition_id == cohort_id) |>
      nrow() |>
      expect_equal(2)

    # test valid sex variable
    cdm$cancer_cohorts |>
      PatientProfiles::addSex() |>
      dplyr::pull(sex) |>
      unique() |>
      expect_in(c(
        "Male",
        "Female")
        )

    if (cancer_type == "breast_cancer") {
      cdm$cancer_cohorts |>
        PatientProfiles::addSex() |>
        dplyr::pull(sex) |>
        unique() |>
        expect_equal("Female")
    } else if (cancer_type == "prostate_cancer") {
      cdm$cancer_cohorts |>
        PatientProfiles::addSex() |>
        dplyr::pull(sex) |>
        unique() |>
        expect_equal("Male")
    }

    # test attrition
    cdm$cancer_cohorts |>
      CohortConstructor::attrition() |>
      dplyr::filter(
        cohort_definition_id == cohort_id
      ) |> 
      dplyr::select(excluded_records) |>
      sum() |>
      expect_equal(1)

  }

})


test_that("createCancerCohorts works with a xlsx file for all cancer types", {

  cdmVersion <- "5.4"

  test_name <- "test_all_cancer_patients"
  TestGenerator::readPatients.xl(
      filePath = file.path(
        testthat::test_path(
          "excel_files",
          paste0(
            "test_cdm_5.4_",
            "all_cancer_patients",
            ".xlsx"
          )
        )
      ),
      testName = test_name
    )

  # create cdm instance with current cancer patients
  cdm <- TestGenerator::patientsCDM(
    pathJson = NULL,
    testName = test_name,
    cdmVersion = cdmVersion
  )

  # call createCancerCohorts to generate codelists and create cohorts
  cdm <- createCancerCohorts(
    cdm = cdm,
    path = "cancer_cohorts",
    name = "cancer_cohorts"
  )

  # test number of patients in cdm instance
  cdm$person |>
    dplyr::collect() |>
    nrow() |>
    expect_equal(21)

  # test number of patients in cohort
  cdm$cancer_cohorts |>
    dplyr::collect() |>
    nrow() |>
    expect_equal(16)

  # test valid sex variable
  cdm$cancer_cohorts |>
    PatientProfiles::addSex() |>
    dplyr::pull(sex) |>
    unique() |>
    expect_in(c(
      "Male",
      "Female")
    )

  breast_cohort_id <- cdm$cancer_cohorts |>
    omopgenerics::settings() |>
    dplyr::filter(cohort_name == "breast_cancer") |>
    dplyr::pull(cohort_definition_id)

  if (length(breast_cohort_id) > 0) {
    cdm$cancer_cohorts |>
      PatientProfiles::addSex() |>
      dplyr::filter(cohort_definition_id == breast_cohort_id) |>
      dplyr::pull(sex) |>
      unique() |>
      expect_equal("Female")
  }

  prostate_cohort_id <- cdm$cancer_cohorts |>
    omopgenerics::settings() |>
    dplyr::filter(cohort_name == "prostate_cancer") |>
    dplyr::pull(cohort_definition_id)

  if (length(prostate_cohort_id) > 0) {
    cdm$cancer_cohorts |>
      PatientProfiles::addSex() |>
      dplyr::filter(cohort_definition_id == prostate_cohort_id) |>
      dplyr::pull(sex) |>
      unique() |>
      expect_equal("Male")
  }

  # test attrition
  # One more for the extra lung cancer cohort
  cdm$cancer_cohorts |>
    CohortConstructor::attrition() |>
    dplyr::select(excluded_records) |>
    sum() |>
    expect_equal(8)

})

# DEPRECATED 
# test_that("createCancerCohorts works with DECK concept sets", {

#   cdmVersion <- "5.4"

#   test_name <- "test_all_cancer_patients"

#   # create cdm instance with current cancer patients
#   cdm <- TestGenerator::patientsCDM(
#     pathJson = NULL,
#     testName = test_name,
#     cdmVersion = cdmVersion
#   )

#   # call createCancerCohorts to generate codelists and create cohorts
#   cdm <- createCancerCohorts(
#     cdm = cdm,
#     path = "cancer_cohorts_deck",
#     name = "cancer_cohorts"
#   )

#   # test number of patients in cdm instance
#   cdm$person |>
#     dplyr::collect() |>
#     nrow() |>
#     expect_equal(21)

#   # test number of patients in cohort
#   # note: we have 3 "flavours" now, so each expectation
#   cdm$cancer_cohorts |>
#     dplyr::collect() |>
#     nrow() |>
#     expect_equal(42)

#   # test total attrition
#   cdm$cancer_cohorts |>
#     CohortConstructor::attrition() |>
#     dplyr::select(excluded_records) |>
#     sum() |>
#     expect_equal(21)

#   # test valid sex variable
#   cdm$cancer_cohorts |>
#     PatientProfiles::addSex() |>
#     dplyr::pull(sex) |>
#     unique() |>
#     expect_in(c(
#       "Male",
#       "Female")
#     )

# })


test_that("createCancerCohorts works with subset of cancer types", {

  cdmVersion <- "5.4"

  test_name <- "test_all_cancer_patients"

  cdm <- TestGenerator::patientsCDM(
    pathJson = NULL,
    testName = test_name,
    cdmVersion = cdmVersion
  )

  # We createCancerCohorts to test the parameter to specify cancer types
  cdm <- createCancerCohorts(
    cdm = cdm,
    path = "cancer_cohorts",
    name = "cancer_cohorts",
    cancer = c("breast_cancer", "lung_cancer_optimal")
  )

  cdm$cancer_cohorts |>
    dplyr::collect() |>
    nrow() |>
    expect_equal(4)


  # Testing we filter the correct cohorts

  # -
  breast_cohort_id <- cdm$cancer_cohorts |>
    omopgenerics::settings() |>
    dplyr::filter(cohort_name == "breast_cancer") |>
    dplyr::pull(cohort_definition_id)

  expect_equal(breast_cohort_id, 1)

  cdm$cancer_cohorts |>
    PatientProfiles::addSex() |>
    dplyr::filter(cohort_definition_id == breast_cohort_id) |>
    dplyr::pull(sex) |>
    unique() |>
    expect_equal(c("Female"))

  # -
  lung_cohort_id <- cdm$cancer_cohorts |>
    omopgenerics::settings() |>
    dplyr::filter(cohort_name == "lung_cancer_optimal") |>
    dplyr::pull(cohort_definition_id)

  expect_equal(lung_cohort_id, 2)

  cdm$cancer_cohorts |>
    PatientProfiles::addSex() |>
    dplyr::filter(cohort_definition_id == breast_cohort_id) |>
    dplyr::pull(sex) |>
    unique() |>
    expect_equal(c("Female"))

  # Test that 2 patients are excluded from the cohort
  cdm$cancer_cohorts |>
    CohortConstructor::attrition() |>
    dplyr::select(excluded_records) |>
    sum() |>
    expect_equal(2)

})


test_that("createCancerCohorts works with single cancer type", {

  cdmVersion <- "5.4"

  test_name <- "test_all_cancer_patients"

  cdm <- TestGenerator::patientsCDM(
    pathJson = NULL,
    testName = test_name,
    cdmVersion = cdmVersion
  )

  # We createCancerCohorts to test the parameter to specify a cancer type
  cdm <- createCancerCohorts(
    cdm = cdm,
    path = "cancer_cohorts",
    name = "cancer_cohorts",
    cancer = "melanoma_of_skin"
  )

  cdm$cancer_cohorts |>
    dplyr::collect() |>
    nrow() |>
    expect_equal(2)

  # Test that 1 patient is excluded from the cohort
  cdm$cancer_cohorts |>
    CohortConstructor::attrition() |>
    dplyr::select(excluded_records) |>
    sum() |>
    expect_equal(1)

})








