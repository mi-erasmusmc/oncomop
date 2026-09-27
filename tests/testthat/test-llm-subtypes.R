test_that("Subtypes six rules", {
  skip_if(is.null(Sys.getenv("OPENAI_API_KEY")))
  testName <- "subtypes_six_rules"
  #------------------------------------------------

  # patientGenerator <- PatientGenerator::patientChat$new(
  #   model = "gpt-6-astra"
  # )
  # patientGenerator$prompt({
  #   "PERSON table:
  #     - A population of 3 persons over 18 years old.
  #     - The 6 persons have observation period from 2000 to 2024.
  #     - The 6 persons are females with gender_concept_id = 8532.
  #   CONDITION_OCCURRENCE table:
  #   The patients from the PERSON table have occurrences of 1 types of cancer recorded during their respective observation periods:
  #     - All three persons (3 females) have breast cancer with condition_concept_id: 4308306
  #     - Everyone has condition_type_concept_id 32817
  #   MEASUREMENT table:
  #   Cancer stage information is recorded in this table through TNM categories.
  #   The measurements occur withing 90 days after cancer index date (condition_occurrence):
  #     - The first female person with breast cancer has a measurement record of:
  #       - 'PGR (progesterone receptor) gene variant measurement' (concept ID: 35957667) with a value_as_concept_id: 9191, which means positive.
  #     - The second female person with breast cancer has a measurement record of:
  #       - 'ESR1 (estrogen receptor 1) gene variant measurement' (concept ID: 35948983) with a value_as_concept_id: 9191, which means positive.
  #     - The third female person with breast cancer has a measurement record of:
  #       - 'PGR (progesterone receptor) gene variant measurement' (concept ID: 35957667) with a value_as_concept_id: 9189, which means negative.
  #       - 'ESR1 (estrogen receptor 1) gene variant measurement' (concept ID: 35948983) with a value_as_concept_id: 9189, which means negative.
  #     - The fourth female person with breast cancer has a measurement record of:
  #       - 'ERBB2 (erb-b2 receptor tyrosine kinase 2) gene variant measurement' (concept ID: 35955862) with a value_as_concept_id: 9191, which means positive.
  #     - The fifth female person with breast cancer has a measurement record of
  #       - 'ERBB2 (erb-b2 receptor tyrosine kinase 2) gene variant measurement' (concept ID: 35955862) with a value_as_concept_id: 9189, which means negative.
  #     - The sixth female person with breast cancer has a measurement record of
  #       - 'ERBB2 (erb-b2 receptor tyrosine kinase 2) gene variant measurement' (concept ID: 35955862) with a value_as_concept_id: 9189, which means negative.
  #       - 'PGR (progesterone receptor) gene variant measurement' (concept ID: 35957667) with a value_as_concept_id: 9189, which means negative.
  #       - 'ESR1 (estrogen receptor 1) gene variant measurement' (concept ID: 35948983) with a value_as_concept_id: 9189, which means negative.
  #   Output requirements:
  #     - All patients in PERSON have an observation period.
  #     - All conditions occurrences and measurement records of a patient must have happened during their observation period.
  #     - Fill out the condition end date 2023-12-31 for everyone."
  # })
  # patientGenerator$save(testName)

  #---------------------------------
  cdm <- TestGenerator::patientsCDM(
    testName = testName,
    vocabulary = "v20260227_complete",
    cdmVersion = "5.4"
  )
  cdm <- createCancerCohorts(
    cdm = cdm,
    path = "cancer_cohorts",
    name = "cancer_cohorts"
  )
  cdm$cancer_cohorts |>
    collect() |>
    nrow() |>
    expect_equal(6)
  cdm$measurement |>
    collect() |>
    nrow() |>
    expect_equal(9)
  # Female 1 - PR - Positive
  cdm$measurement |>
    collect() |>
    dplyr::filter(
      person_id == 1,
      measurement_concept_id == 35957667
    ) |>
    dplyr::pull(value_as_concept_id) |> 
    expect_equal(9191)
  # Female 2 - PR - Positive
  cdm$measurement |>
    collect() |>
    dplyr::filter(
      person_id == 2,
      measurement_concept_id == 35948983
    ) |>
    dplyr::pull(value_as_concept_id) |> 
    expect_equal(9191)
  # Female 3 - PR/ER - Double negative
  cdm$measurement |>
    collect() |>
    dplyr::filter(
      person_id == 3,
      measurement_concept_id %in% c(35957667, 35948983)
    ) |>
    dplyr::pull(value_as_concept_id) |> 
    expect_equal(c(9189, 9189))
  # Female 4 - HER2 - positive
  cdm$measurement |>
    collect() |>
    dplyr::filter(
      person_id == 4,
      measurement_concept_id == 35955862
    ) |>
    dplyr::pull(value_as_concept_id) |> 
    expect_equal(c(9191))
  # Female 5 - HER2 - negative
  cdm$measurement |>
    collect() |>
    dplyr::filter(
      person_id == 5,
      measurement_concept_id == 35955862
    ) |>
    dplyr::pull(value_as_concept_id) |> 
    expect_equal(c(9189))
  # Female 6 - PR/ER/HER2 - triple negative
  cdm$measurement |>
    collect() |>
    dplyr::filter(
      person_id == 6,
      measurement_concept_id %in% c(35957667, 35948983, 35955862)
    ) |>
    dplyr::pull(value_as_concept_id) |> 
    expect_equal(c(9189, 9189, 9189))
})

# test_that("subtypes and stages together", {
#   skip_if(is.null(Sys.getenv("OPENAI_API_KEY")))
#   testName <- "subtypes_stages"
#   #------------------------------------------------
#   patientGenerator <- PatientGenerator::patientChat$new(
#     model = "gpt-5.6-luna"
#   )
#   patientGenerator$prompt({
#     "PERSON table:
#       - A population of 3 persons over 18 years old.
#       - The 6 persons have observation period from 2000 to 2024.
#       - The 6 persons are females with gender_concept_id = 8532.
#     CONDITION_OCCURRENCE table:
#     The patients from the PERSON table have occurrences of 1 types of cancer recorded during their respective observation periods:
#       - All three persons (3 females) have breast cancer with condition_concept_id: 4308306
#       - Everyone has condition_type_concept_id 32817
#     MEASUREMENT table:
#     Cancer stage information is recorded in this table through TNM categories.
#     The measurements occur withing 90 days after cancer index date (condition_occurrence):
#       - The first female person with breast cancer has a measurement record of:
#         - 'PGR (progesterone receptor) gene variant measurement' (concept ID: 35957667),
#         -  T1 (concept ID: 1633883), N0 (concept ID: 1634070) and a M0 (concept ID: 1634757)
#       - The second female person with breast cancer has a measurement record of:
#         - 'ESR1 (estrogen receptor 1) gene variant measurement' (concept ID: 35948983),
#         - T1mi (concept ID: 1633949), a N3a (concept ID: 1635496) and a M0 (concept ID: 1634757)
#       - The third female person with breast cancer has a measurement record of
#         - 'ERBB2 (erb-b2 receptor tyrosine kinase 2) gene variant measurement' (concept ID: 35955862),
#         - T4d (concept ID: 1635022), a N1 (concept ID: 1633651) and a M1 (concept ID: 1633974)
#     Output requirements:
#       - All patients in PERSON have an observation period.
#       - All conditions occurrences and measurement records of a patient must have happened during their observation period.
#       - Fill out the condition end date 2023-12-31 for everyone."
#   })
#   patientGenerator$save(testName)
# })
