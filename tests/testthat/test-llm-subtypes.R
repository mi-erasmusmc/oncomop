test_that("Subtypes six rules", {
  skip_if(is.null(Sys.getenv("OPENAI_API_KEY")))
  testName <- "subtypes_six_rules"
  #------------------------------------------------

  # patientGenerator <- PatientGenerator::patientChat$new(
  #   model = "gpt-6-astra"
  # )
  # patientGenerator$prompt({
  #   "PERSON table:
  #     - A population of 6 persons over 18 years old.
  #     - The 6 persons have observation period from 2000 to 2024.
  #     - The 6 persons are females with gender_concept_id = 8532.
  #   CONDITION_OCCURRENCE table:
  #   The patients from the PERSON table have occurrences of 1 types of cancer recorded during their respective observation periods:
  #     - All three persons (6 females) have breast cancer with condition_concept_id: 4308306
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
  # Female 2 - ER - Positive
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

test_that("test group", {
  skip_if(is.null(Sys.getenv("OPENAI_API_KEY")))
  testName <- "subtypes_groups"
  #------------------------------------------------
  # patientGenerator <- PatientGenerator::patientChat$new(
  #   model = "gpt-6-astra"
  # )
  # patientGenerator$prompt({
  #   "PERSON TABLE:
  #   A population of 26 persons over 18 years old.
  #   The 26 persons have an observation period from 2000 to 2024.
  #   The 26 persons are females with gender_concept_id = 8532.
  #   CONDITION_OCCURRENCE table:
  #   The patients from the PERSON table have occurrences of 1 type of cancer recorded during their respective observation periods:
  #   All 26 persons (26 females) have breast cancer with condition_concept_id: 4308306
  #   Everyone has condition_type_concept_id 32817
  #   MEASUREMENT table:
  #   Hormone receptor (ER, PR) and HER2 status is recorded in this table as gene variant measurements.
  #   All measurements occur within 90 days after the cancer index date (condition_start_date).
  #   Concepts used:
  #   ER: 'ESR1 (estrogen receptor 1) gene variant measurement' (concept ID: 35948983)
  #   PR: 'PGR (progesterone receptor) gene variant measurement' (concept ID: 35957667)
  #   HER2: 'ERBB2 (erb-b2 receptor tyrosine kinase 2) gene variant measurement' (concept ID: 35955862)
  #   value_as_concept_id: 9191 = positive, 9189 = negative
  #   Group A – only one test performed (each person has exactly one measurement record):
  #   The first female person with breast cancer (ER+) has a measurement record of:
  #   'ESR1 (estrogen receptor 1) gene variant measurement' (concept ID: 35948983) with a value_as_concept_id: 9191, which means positive.
  #   The second female person with breast cancer (ER-) has a measurement record of:
  #   'ESR1 (estrogen receptor 1) gene variant measurement' (concept ID: 35948983) with a value_as_concept_id: 9189, which means negative.
  #   The third female person with breast cancer (PR+) has a measurement record of:
  #   'PGR (progesterone receptor) gene variant measurement' (concept ID: 35957667) with a value_as_concept_id: 9191, which means positive.
  #   The fourth female person with breast cancer (PR-) has a measurement record of:
  #   'PGR (progesterone receptor) gene variant measurement' (concept ID: 35957667) with a value_as_concept_id: 9189, which means negative.
  #   The fifth female person with breast cancer (HER2+) has a measurement record of:
  #   'ERBB2 (erb-b2 receptor tyrosine kinase 2) gene variant measurement' (concept ID: 35955862) with a value_as_concept_id: 9191, which means positive.
  #   The sixth female person with breast cancer (HER2-) has a measurement record of:
  #   'ERBB2 (erb-b2 receptor tyrosine kinase 2) gene variant measurement' (concept ID: 35955862) with a value_as_concept_id: 9189, which means negative.
  #   Group B – all three tests performed (each person has exactly three measurement records, all on the same date):
  #   The seventh female person with breast cancer (ER+/PR+/HER2+, triple positive) has measurement records of:
  #   'ESR1 (estrogen receptor 1) gene variant measurement' (concept ID: 35948983) with a value_as_concept_id: 9191, which means positive.
  #   'PGR (progesterone receptor) gene variant measurement' (concept ID: 35957667) with a value_as_concept_id: 9191, which means positive.
  #   'ERBB2 (erb-b2 receptor tyrosine kinase 2) gene variant measurement' (concept ID: 35955862) with a value_as_concept_id: 9191, which means positive.
  #   The eighth female person with breast cancer (ER+/PR+/HER2-) has measurement records of:
  #   'ESR1 (estrogen receptor 1) gene variant measurement' (concept ID: 35948983) with a value_as_concept_id: 9191, which means positive.
  #   'PGR (progesterone receptor) gene variant measurement' (concept ID: 35957667) with a value_as_concept_id: 9191, which means positive.
  #   'ERBB2 (erb-b2 receptor tyrosine kinase 2) gene variant measurement' (concept ID: 35955862) with a value_as_concept_id: 9189, which means negative.
  #   The ninth female person with breast cancer (ER-/PR-/HER2+, HER2-enriched) has measurement records of:
  #   'ESR1 (estrogen receptor 1) gene variant measurement' (concept ID: 35948983) with a value_as_concept_id: 9189, which means negative.
  #   'PGR (progesterone receptor) gene variant measurement' (concept ID: 35957667) with a value_as_concept_id: 9189, which means negative.
  #   'ERBB2 (erb-b2 receptor tyrosine kinase 2) gene variant measurement' (concept ID: 35955862) with a value_as_concept_id: 9191, which means positive.
  #   The tenth female person with breast cancer (ER-/PR-/HER2-, triple negative) has measurement records of:
  #   'ESR1 (estrogen receptor 1) gene variant measurement' (concept ID: 35948983) with a value_as_concept_id: 9189, which means negative.
  #   'PGR (progesterone receptor) gene variant measurement' (concept ID: 35957667) with a value_as_concept_id: 9189, which means negative.
  #   'ERBB2 (erb-b2 receptor tyrosine kinase 2) gene variant measurement' (concept ID: 35955862) with a value_as_concept_id: 9189, which means negative.
  #   The eleventh female person with breast cancer (ER+/PR-/HER2+) has measurement records of:
  #   'ESR1 (estrogen receptor 1) gene variant measurement' (concept ID: 35948983) with a value_as_concept_id: 9191, which means positive.
  #   'PGR (progesterone receptor) gene variant measurement' (concept ID: 35957667) with a value_as_concept_id: 9189, which means negative.
  #   'ERBB2 (erb-b2 receptor tyrosine kinase 2) gene variant measurement' (concept ID: 35955862) with a value_as_concept_id: 9191, which means positive.
  #   The twelfth female person with breast cancer (ER+/PR-/HER2-) has measurement records of:
  #   'ESR1 (estrogen receptor 1) gene variant measurement' (concept ID: 35948983) with a value_as_concept_id: 9191, which means positive.
  #   'PGR (progesterone receptor) gene variant measurement' (concept ID: 35957667) with a value_as_concept_id: 9189, which means negative.
  #   'ERBB2 (erb-b2 receptor tyrosine kinase 2) gene variant measurement' (concept ID: 35955862) with a value_as_concept_id: 9189, which means negative.
  #   The thirteenth female person with breast cancer (ER-/PR+/HER2+) has measurement records of:
  #   'ESR1 (estrogen receptor 1) gene variant measurement' (concept ID: 35948983) with a value_as_concept_id: 9189, which means negative.
  #   'PGR (progesterone receptor) gene variant measurement' (concept ID: 35957667) with a value_as_concept_id: 9191, which means positive.
  #   'ERBB2 (erb-b2 receptor tyrosine kinase 2) gene variant measurement' (concept ID: 35955862) with a value_as_concept_id: 9191, which means positive.
  #   The fourteenth female person with breast cancer (ER-/PR+/HER2-) has measurement records of:
  #   'ESR1 (estrogen receptor 1) gene variant measurement' (concept ID: 35948983) with a value_as_concept_id: 9189, which means negative.
  #   'PGR (progesterone receptor) gene variant measurement' (concept ID: 35957667) with a value_as_concept_id: 9191, which means positive.
  #   'ERBB2 (erb-b2 receptor tyrosine kinase 2) gene variant measurement' (concept ID: 35955862) with a value_as_concept_id: 9189, which means negative.
  #   Group C – two tests performed (each person has exactly two measurement records, both on the same date):
  #   ER + PR (no HER2 test):
  #   The fifteenth female person with breast cancer (ER+/PR+) has measurement records of:
  #   'ESR1 (estrogen receptor 1) gene variant measurement' (concept ID: 35948983) with a value_as_concept_id: 9191, which means positive.
  #   'PGR (progesterone receptor) gene variant measurement' (concept ID: 35957667) with a value_as_concept_id: 9191, which means positive.
  #   The sixteenth female person with breast cancer (ER+/PR-) has measurement records of:
  #   'ESR1 (estrogen receptor 1) gene variant measurement' (concept ID: 35948983) with a value_as_concept_id: 9191, which means positive.
  #   'PGR (progesterone receptor) gene variant measurement' (concept ID: 35957667) with a value_as_concept_id: 9189, which means negative.
  #   The seventeenth female person with breast cancer (ER-/PR+) has measurement records of:
  #   'ESR1 (estrogen receptor 1) gene variant measurement' (concept ID: 35948983) with a value_as_concept_id: 9189, which means negative.
  #   'PGR (progesterone receptor) gene variant measurement' (concept ID: 35957667) with a value_as_concept_id: 9191, which means positive.
  #   The eighteenth female person with breast cancer (ER-/PR-) has measurement records of:
  #   'ESR1 (estrogen receptor 1) gene variant measurement' (concept ID: 35948983) with a value_as_concept_id: 9189, which means negative.
  #   'PGR (progesterone receptor) gene variant measurement' (concept ID: 35957667) with a value_as_concept_id: 9189, which means negative.
  #   ER + HER2 (no PR test):
  #   The nineteenth female person with breast cancer (ER+/HER2+) has measurement records of:
  #   'ESR1 (estrogen receptor 1) gene variant measurement' (concept ID: 35948983) with a value_as_concept_id: 9191, which means positive.
  #   'ERBB2 (erb-b2 receptor tyrosine kinase 2) gene variant measurement' (concept ID: 35955862) with a value_as_concept_id: 9191, which means positive.
  #   The twentieth female person with breast cancer (ER+/HER2-) has measurement records of:
  #   'ESR1 (estrogen receptor 1) gene variant measurement' (concept ID: 35948983) with a value_as_concept_id: 9191, which means positive.
  #   'ERBB2 (erb-b2 receptor tyrosine kinase 2) gene variant measurement' (concept ID: 35955862) with a value_as_concept_id: 9189, which means negative.
  #   The twenty-first female person with breast cancer (ER-/HER2+) has measurement records of:
  #   'ESR1 (estrogen receptor 1) gene variant measurement' (concept ID: 35948983) with a value_as_concept_id: 9189, which means negative.
  #   'ERBB2 (erb-b2 receptor tyrosine kinase 2) gene variant measurement' (concept ID: 35955862) with a value_as_concept_id: 9191, which means positive.
  #   The twenty-second female person with breast cancer (ER-/HER2-) has measurement records of:
  #   'ESR1 (estrogen receptor 1) gene variant measurement' (concept ID: 35948983) with a value_as_concept_id: 9189, which means negative.
  #   'ERBB2 (erb-b2 receptor tyrosine kinase 2) gene variant measurement' (concept ID: 35955862) with a value_as_concept_id: 9189, which means negative.
  #   PR + HER2 (no ER test):
  #   The twenty-third female person with breast cancer (PR+/HER2+) has measurement records of:
  #   'PGR (progesterone receptor) gene variant measurement' (concept ID: 35957667) with a value_as_concept_id: 9191, which means positive.
  #   'ERBB2 (erb-b2 receptor tyrosine kinase 2) gene variant measurement' (concept ID: 35955862) with a value_as_concept_id: 9191, which means positive.
  #   The twenty-fourth female person with breast cancer (PR+/HER2-) has measurement records of:
  #   'PGR (progesterone receptor) gene variant measurement' (concept ID: 35957667) with a value_as_concept_id: 9191, which means positive.
  #   'ERBB2 (erb-b2 receptor tyrosine kinase 2) gene variant measurement' (concept ID: 35955862) with a value_as_concept_id: 9189, which means negative.
  #   The twenty-fifth female person with breast cancer (PR-/HER2+) has measurement records of:
  #   'PGR (progesterone receptor) gene variant measurement' (concept ID: 35957667) with a value_as_concept_id: 9189, which means negative.
  #   'ERBB2 (erb-b2 receptor tyrosine kinase 2) gene variant measurement' (concept ID: 35955862) with a value_as_concept_id: 9191, which means positive.
  #   The twenty-sixth female person with breast cancer (PR-/HER2-) has measurement records of:
  #   'PGR (progesterone receptor) gene variant measurement' (concept ID: 35957667) with a value_as_concept_id: 9189, which means negative.
  #   'ERBB2 (erb-b2 receptor tyrosine kinase 2) gene variant measurement' (concept ID: 35955862) with a value_as_concept_id: 9189, which means negative.
  #   Output requirements:
  #   All patients in PERSON have an observation period.
  #   All condition occurrences and measurement records of a patient must have happened during their observation period.
  #   All measurement dates fall within 0–90 days after the patient's breast cancer condition_start_date.
  #   Persons in Group A have exactly one measurement record, persons in Group B have exactly three, and persons in Group C have exactly two.
  #   Fill out the condition end date 2023-12-31 for everyone."
  # })
  # patientGenerator$save(testName)
  cdm <- TestGenerator::patientsCDM(
    testName = testName,
    vocabulary = "v20260227_complete",
    cdmVersion = "5.4"
  )
  cdm$person |> 
    collect() |> 
    nrow() |> 
    expect_equal(26)
})
