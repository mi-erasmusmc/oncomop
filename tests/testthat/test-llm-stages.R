test_that("default rules multiple patients", {
  skip_if(is.null(Sys.getenv("OPENAI_API_KEY")))
  testName <- "default_rules_multiple_subjects"
  #------------------------------------------------
  
  # patientGenerator <- PatientGenerator::patientChat$new(
  #   model = "gpt-5.6-luna"
  # )
  # patientGenerator$prompt({
  #   "PERSON table:
  #     - A population of 3 persons over 18 years old.
  #     - The 3 persons have observation period from 2000 to 2024.
  #     - The 3 persons are females with gender_concept_id = 8532.  
  #   CONDITION_OCCURRENCE table:
  #   The patients from the PERSON table have occurrences of 1 types of cancer recorded during their respective observation periods:
  #     - All three persons (3 females) have breast cancer with condition_concept_id: 4308306
  #     - Everyone has condition_type_concept_id 32817
  #   MEASUREMENT table:
  #   Cancer stage information is recorded in this table through TNM categories.
  #   The measurements occurr on the cancer index date (condition_occurrence):
  #     - The first female person with breast cancer has a measurement record of:
  #       - T1 (concept ID: 1633883), N0 (concept ID: 1634070) and a M0 (concept ID: 1634757)
  #     - The second female person with breast cancer has a measurement record of:
  #       - T1mi (concept ID: 1633949), a N3a (concept ID: 1635496) and a M0 (concept ID: 1634757)
  #     - The third female person with breast cancer has a measurement record of
  #       - T4d (concept ID: 1635022), a N1 (concept ID: 1633651) and a M1 (concept ID: 1633974)
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
    expect_equal(3)
  cdm$measurement |> 
    collect() |>  
    nrow() |> 
    expect_equal(9)
  cdm$measurement |> 
    collect() |> 
    pull(measurement_concept_id) |> 
    unique() |> 
    sort() |> 
    expect_equal(
      c(1633651L, 1633883L, 1633949L,
        1633974L, 1634070L, 1634757L,
         1635022L, 1635496L))
})

test_that("default rules single patient", {
  skip_if(is.null(Sys.getenv("OPENAI_API_KEY")))
  testName <- "default_rules_single_subject"
  # patientGenerator <- PatientGenerator::patientChat$new(
  #   model = "gpt-5.6-luna"
  # )
  # patientGenerator$prompt({
  #   "PERSON table:
  #     - A population of 1 person over 18 years old.
  #     - The 1 person have observation period from 2000 to 2024.
  #     - The 1 person is females with gender_concept_id = 8532.  
  #   CONDITION_OCCURRENCE table:
  #   The patients from the PERSON table have occurrences of 7 different types of cancer recorded during their respective observation periods:
  #     - Patient 1 (1 female) have breast cancer with condition_concept_id: 4308306
  #     - Everyone has condition_type_concept_id 32817
  #   MEASUREMENT table:
  #   Cancer stage information is recorded in this table through TNM categories.
  #     - The 1 female person with breast cancer has a T1 (concept ID: 1633883), a N0 (concept ID: 1634070) and a M0 (concept ID: 1634757)
  #   Output requirements:
  #     - All patients in PERSON have an observation period.
  #     - All conditions occurrences and measurement records of a patient must have happened during their observation period.
  #     - Fill out the condition end date 2023-12-31 for everyone."
  # })
  # patientGenerator$save(testName)
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
    expect_equal(1)
  cdm$measurement |> 
    collect() |> 
    pull(measurement_concept_id) |> 
    sort() |> 
    expect_equal(
      c(1633883L, 1634070L, 1634757L)
    )
})


test_that("Create test patients baseline", {
  skip_if(is.null(Sys.getenv("OPENAI_API_KEY")))
  testName <- "stages_baseline"
  # patientGenerator <- PatientGenerator::patientChat$new(
  #   model = "gpt-5.6-luna"
  # )
  # patientGenerator$prompt({
  #   "PERSON table:
  #     - A population of 4 persons all over 18 years old.
  #     - All persons have observation period from 2000 to 2024.
  #     - 2 persons are females with gender_concept_id = 8532.
  #     - 2 persons are males with gender_concept_id = 8507.  
  #   CONDITION_OCCURRENCE table:
  #   The patients from the PERSON table have occurrences of 7 different types of cancer recorded during their respective observation periods:
  #     - 2 patients have colorectal cancer with condition_concept_id: 40481902    
  #     - 1 patients (1 female) have breast cancer with condition_concept_id: 36556994
  #     - 1 patients (1 male) have prostate cancer with condition_concept_id: 4163261
  #     - Everyone has condition_type_concept_id 32817
  #   MEASUREMENT table:
  #   Cancer stage information is recorded in this table through TNM categories.
  #     - The 2 persons with colorectal cancer has a Ta (concept ID: 1634394), a N0 (concept ID: 1633720) and a M0 (concept ID: 1633829).
  #     - The 1 female with breast cancer has a T1 (concept ID: 1633549), a N0 (concept ID: 1633720) and a M0 (concept ID: 1633829).
  #     - The 1 male with prostate cancer has a T2a (concept ID: 1635532), a N0 (concept ID: 1633720) and a M0 (concept ID: 1633829).
  #   Output requirements:
  #     - All patients in PERSON have an observation period.
  #     - All conditions occurrences and measurement records of a patient must have happened during their observation period.
  #     - Fill out the condition end date 2023-12-31 for everyone."
  # })
  # patientGenerator$save(testName)
  cdm <- TestGenerator::patientsCDM(
    testName = "stages_baseline",
    vocabulary = "v20260227_complete",
    cdmVersion = "5.4"
  )
  cdm <- createCancerCohorts(
    cdm = cdm,
    path = "cancer_cohorts",
    name = "cancer_cohorts"
  )

  # test 34 persons in population [(5 persons x 7 cancers) - 1]
  tot_person <- cdm$person |>
    dplyr::collect() |>
    nrow()
  expect_equal(tot_person, 4)

  # integrity checks for gender
  n_females <- cdm$person |>
    dplyr::collect() |>
    dplyr::filter(gender_concept_id == 8532) |>
    nrow()
  expect_equal(n_females, 2)

  n_males <- cdm$person |>
    dplyr::collect() |>
    dplyr::filter(gender_concept_id == 8507) |>
    nrow()
  expect_equal(n_males, 2)

  # test 2 persons with colorectal cancer
  n_persons_colorectal <- cdm$cancer_cohorts |>
    PatientProfiles::addCohortName() |> 
    dplyr::count(cohort_name) |>
    dplyr::filter(cohort_name == "colorectal_cancer") |>
    dplyr::pull(n)
  expect_equal(n_persons_colorectal, 2)

  # test 1 persons in prostate cancer cohort
  n_persons_prostate <- cdm$cancer_cohorts |>
    PatientProfiles::addCohortName() |> 
    PatientProfiles::addSex() |> 
    dplyr::count(
      cohort_name,
      sex
    ) |>
    dplyr::filter(
      cohort_name == "prostate_cancer",
      sex == "Male"
    ) |>
    dplyr::pull(n)
  expect_equal(n_persons_prostate, 1)

  # test 1 persons in prostate cancer cohort
  n_persons_prostate <- cdm$cancer_cohorts |>
    PatientProfiles::addCohortName() |> 
     PatientProfiles::addSex() |> 
    dplyr::count(
      cohort_name,
      sex
    ) |>
    dplyr::count(cohort_name) |>
    dplyr::filter(cohort_name == "prostate_cancer") |>
    dplyr::pull(n)
  expect_equal(n_persons_prostate, 1)

  # test 1 persons in breast cancer cohort
  n_persons_breast <- cdm$cancer_cohorts |>
    PatientProfiles::addCohortName() |> 
     PatientProfiles::addSex() |> 
    dplyr::count(
      cohort_name,
      sex
    ) |>
    dplyr::count(cohort_name) |>
    dplyr::filter(cohort_name == "breast_cancer") |>
    dplyr::pull(n)
  expect_equal(n_persons_prostate, 1)

  # test 12 total measurements (3 measurements x 4 persons)
  tot_measurements <- cdm$measurement |>
    dplyr::collect() |>
    nrow()
  expect_equal(tot_measurements, 12)

  # test 3 measurements for each person
  n_measurements_per_person <- cdm$measurement |>
    dplyr::count(person_id) |>
    dplyr::pull(n) |>
    unique()
  expect_equal(n_measurements_per_person, 3)

  # integrity checks for condition and measurement dates
  invalid_conditions <- cdm$condition_occurrence |>
    dplyr::inner_join(
      cdm$observation_period,
      by = "person_id"
    ) |>
    dplyr::filter(
      condition_start_date < observation_period_start_date |
        condition_end_date > observation_period_end_date
    ) |> dplyr::collect() |>
    nrow()
  expect_equal(invalid_conditions, 0)

  invalid_measurements <- cdm$measurement |>
    dplyr::inner_join(
      cdm$observation_period,
      by = "person_id"
    ) |>
    dplyr::filter(
      measurement_date  < observation_period_start_date |
        measurement_date  > observation_period_end_date
    ) |> dplyr::collect() |>
    nrow()
  expect_equal(invalid_measurements, 0)

})

test_that("Create test patients with TNM measurements for staging - UICC 7th edition", {

  # Reference files:
  # - extras/uicc_tnm_anatomic_stage_mapping 1.csv: mapping of TNM combinations to UICC stages
  # - extras/universal_tnm_options 1.csv: mapping of each T/N/M to its UICC concept ids

  # For every cancer type creates 5 test patients, one per stage:
  # - a patient with stage 0 cancer (or substages)
  # - a patient with stage I cancer (or substages)
  # - a patient with stage II cancer (or substages)
  # - a patient with stage III cancer (or substages)
  # - a patient with stage IV cancer (or substages)
  # Stage information is NOT explicit, but is rather represented by a triplet of
  # T/N/M measurements for each patient corresponding to a specific summary stage.

  # Notes:
  # 1) Prostate cancer doesn't have a stage 0, so there are only 4 persons with stages I-IV -> total patients: 34
  # 2) Melanoma of skin has only pathological TNM categories in our mapping file
  # 4) M0 and MX are not valid pathological categories, so we must use base or clinical codes for them

  # skip_if(is.null(Sys.getenv("OPENAI_API_KEY")))

  # patientGenerator <- PatientGenerator::patientChat$new()
  #
  # patientGenerator$prompt({
  #   "PERSON table:
  #     - A population of 34 persons all over 18 years old.
  #     - All persons have observation period from 2000 to 2024.
  #     - 17 persons are females with gender_concept_id = 8532.
  #     - 17 persons are males with gender_concept_id = 8507.
  #
  #   CONDITION_OCCURRENCE table:
  #   The patients from the PERSON table have occurrences of 7 different types of cancer recorded during their respective observation periods:
  #     - 5 patients have bladder cancer with condition_concept_id: 196360
  #     - 5 patients (all females) have breast cancer with condition_concept_id: 36556994
  #     - 5 patients have colorectal cancer with condition_concept_id: 40481902
  #     - 5 patients have lung cancer with condition_concept_id: 36535703
  #     - 5 patients have oesophageal cancer with condition_concept_id: 4181343
  #     - 4 patients (all males) have prostate cancer with condition_concept_id: 4163261
  #     - 5 patients have melanoma of skin with condition_concept_id: 141232
  #     - Everyone has condition_type_concept_id 32817
  #
  #   MEASUREMENT table:
  #   Cancer stage information is recorded in this table through TNM categories.
  #
  #   The persons with **bladder** cancer have the following measurements:
  #   - Person 1 has a Ta (concept ID: 1634394), a N0 (concept ID: 1633720) and a M0 (concept ID: 1633829).
  #   - Person 2 has a T1 (concept ID: 1633549), a N0 (concept ID: 1633720) and a M0 (concept ID: 1633829).
  #   - Person 3 has a T2a (concept ID: 1635532), a N0 (concept ID: 1633720) and a M0 (concept ID: 1633829).
  #   - Person 4 has a T3 (concept ID: 1635854), a N0 (concept ID: 1633720) and a M0 (concept ID: 1633829).
  #   - Person 5 has a T3 (concept ID: 1635854), a N1 (concept ID: 1635103) and a M1 (concept ID: 1633696).
  #
  #   The persons with **breast** cancer have the following measurements:
  #   - Person 1 has a Tis (concept ID: 1633798), a N0 (concept ID: 1633720) and a M0 (concept ID: 1633829).
  #   - Person 2 has a T1a (concept ID: 1633764), a N0 (concept ID: 1633720) and a M0 (concept ID: 1633829).
  #   - Person 3 has a T2 (concept ID: 1634506), a N1 (concept ID: 1635103) and a M0 (concept ID: 1633829).
  #   - Person 4 has a T1mi (concept ID: 1634186), a N3b (concept ID: 1633277) and a M0 (concept ID: 1633829).
  #   - Person 5 has a T1 (concept ID: 1633549), a N1a (concept ID: 1634098) and a M1 (concept ID: 1633696).
  #
  #   The persons with **colorectal** cancer have the following measurements:
  #   - Person 1 has a Tis (concept ID: 1633798), a N0 (concept ID: 1633720) and a M0 (concept ID: 1633829).
  #   - Person 2 has a T2 (concept ID: 1634506), a N0 (concept ID: 1633720) and a M0 (concept ID: 1633829).
  #   - Person 3 has a T3 (concept ID: 1635854), a N0 (concept ID: 1633720) and a M0 (concept ID: 1633829).
  #   - Person 4 has a T1 (concept ID: 1633549), a N2 (concept ID: 1634240) and a M0 (concept ID: 1633829).
  #   - Person 5 has a T4a (concept ID: 1634724), a N2a (concept ID: 1633840) and a M1 (concept ID: 1633696).
  #
  #   The persons with **lung** cancer have the following measurements:
  #   - Person 1 has a Tis (concept ID: 1633798), a N0 (concept ID: 1633720) and a M0 (concept ID: 1633829).
  #   - Person 2 has a T1b (concept ID: 1634319), a N0 (concept ID: 1633720) and a M0 (concept ID: 1633829).
  #   - Person 3 has a T2b (concept ID: 1635301), a N0 (concept ID: 1633720) and a M0 (concept ID: 1633829).
  #   - Person 4 has a T4 (concept ID: 1634576), a N0 (concept ID: 1633720) and a M0 (concept ID: 1633829).
  #   - Person 5 has a TX (concept ID: 1633390), a N3 (concept ID: 1633684) and a M1 (concept ID: 1633696).
  #
  #   The persons with **oesophageal** cancer have the following measurements:
  #   - Person 1 has a Tis (concept ID: 1633798), a N0 (concept ID: 1633720) and a M0 (concept ID: 1633829).
  #   - Person 2 has a T1 (concept ID: 1633549), a N0 (concept ID: 1633720) and a M0 (concept ID: 1633829).
  #   - Person 3 has a T1 (concept ID: 1633549), a N1 (concept ID: 1635103) and a M0 (concept ID: 1633829).
  #   - Person 4 has a T3 (concept ID: 1635854), a N1 (concept ID: 1635103) and a M0 (concept ID: 1633829).
  #   - Person 5 has a T2 (concept ID: 1634506), a N0 (concept ID: 1633720) and a M1 (concept ID: 1633696).
  #
  #   The persons with **prostate** cancer have the following measurements:
  #   - Person 1 has a T1 (concept ID: 1633549), a N0 (concept ID: 1633720) and a M0 (concept ID: 1633829).
  #   - Person 2 has a T2c (concept ID: 1635192), a N0 (concept ID: 1633720) and a M0 (concept ID: 1633829).
  #   - Person 3 has a T3b (concept ID: 1635777), a N0 (concept ID: 1633720) and a M0 (concept ID: 1633829).
  #   - Person 4 has a T2 (concept ID: 1634506), a N1 (concept ID: 1635103) and a M1c (concept ID: 1635843).
  #
  #   The persons with **melanoma of skin** cancer have the following measurements:
  #   - Person 1 has a Tis (concept ID: 1634116), a N0 (concept ID: 1633726) and a M0 (concept ID: 1633468).
  #   - Person 2 has a T1 (concept ID: 1635422), a N0 (concept ID: 1633726) and a M0 (concept ID: 1633468).
  #   - Person 3 has a T2b (concept ID: 1635038), a N0 (concept ID: 1633726) and a M0 (concept ID: 1633468).
  #   - Person 4 has a T1b (concept ID: 1634236), a N1 (concept ID: 1634245) and a M0 (concept ID: 1633468).
  #   - Person 5 has a T1 (concept ID: 1635422), a N1 (concept ID: 1634245) and a M1 (concept ID: 1635336).
  #
  #   Output requirements:
  #   - All patients in PERSON have an observation period.
  #   - All conditions occurrences and measurement records of a patient must have happened during their observation period.
  #   - Fill out the condition end date 2023-12-31 for everyone."
  # })

  # patientGenerator$save("test_patients_staging_uicc7")

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

  # test 34 persons in population [(5 persons x 7 cancers) - 1]
  tot_person <- cdm$person |>
    dplyr::collect() |>
    nrow()
  expect_equal(tot_person, 34)

  # test 102 total measurements (3 measurements x 34 persons)
  tot_measurements <- cdm$measurement |>
    dplyr::collect() |>
    nrow()
  expect_equal(tot_measurements, 102)

  # test 5 people in each cancer cohort (except prostate_cancer)
  n_persons_per_cohort <- cdm$cancer_cohorts |>
    dplyr::count(cohort_definition_id) |>
    dplyr::filter(cohort_definition_id != 8) |>
    dplyr::pull(n) |>
    unique()
  expect_equal(n_persons_per_cohort, 5)

  # test 4 persons in prostate cancer cohort
  n_persons_prostate <- cdm$cancer_cohorts |>
    dplyr::count(cohort_definition_id) |>
    dplyr::filter(cohort_definition_id == 8) |>
    dplyr::pull(n)
  expect_equal(n_persons_prostate, 4)

  # test 3 measurements for each person
  n_measurements_per_person <- cdm$measurement |>
    dplyr::count(person_id) |>
    dplyr::pull(n) |>
    unique()
  expect_equal(n_measurements_per_person, 3)

  # integrity checks for condition and measurement dates
  invalid_conditions <- cdm$condition_occurrence |>
    dplyr::inner_join(
      cdm$observation_period,
      by = "person_id"
    ) |>
    dplyr::filter(
      condition_start_date < observation_period_start_date |
        condition_end_date > observation_period_end_date
    ) |> dplyr::collect() |>
    nrow()
  expect_equal(invalid_conditions, 0)

  invalid_measurements <- cdm$measurement |>
    dplyr::inner_join(
      cdm$observation_period,
      by = "person_id"
    ) |>
    dplyr::filter(
      measurement_date  < observation_period_start_date |
        measurement_date  > observation_period_end_date
    ) |> dplyr::collect() |>
    nrow()
  expect_equal(invalid_measurements, 0)

  # integrity checks for gender
  n_females <- cdm$person |>
    dplyr::collect() |>
    dplyr::filter(gender_concept_id == 8532) |>
    nrow()
  expect_equal(n_females, 17)

  n_males <- cdm$person |>
    dplyr::collect() |>
    dplyr::filter(gender_concept_id == 8507) |>
    nrow()
  expect_equal(n_males, 17)

  breast_cohort_id <- cdm$cancer_cohorts |>
    omopgenerics::settings() |>
    dplyr::filter(stringr::str_detect(cohort_name, "breast")) |>
    dplyr::pull(cohort_definition_id)

  breast_gender <- cdm$cancer_cohorts |>
    PatientProfiles::addSex() |>
    dplyr::filter(cohort_definition_id == breast_cohort_id) |>
    dplyr::pull(sex) |>
    unique()
  expect_equal(breast_gender, "Female")

  prostate_cohort_id <- cdm$cancer_cohorts |>
    omopgenerics::settings() |>
    dplyr::filter(stringr::str_detect(cohort_name, "prostate")) |>
    dplyr::pull(cohort_definition_id)

  prostate_gender <- cdm$cancer_cohorts |>
    PatientProfiles::addSex() |>
    dplyr::filter(cohort_definition_id == prostate_cohort_id) |>
    dplyr::pull(sex) |>
    unique()
  expect_equal(prostate_gender, "Male")
})


test_that("Create test patients with TNM measurements for staging - UICC 8th edition", {

  # For every cancer type creates 5 test patients, one per stage:
  # - a patient with stage 0 cancer
  # - a patient with stage I cancer (or substages)
  # - a patient with stage II cancer (or substages)
  # - a patient with stage III cancer (or substages)
  # - a patient with stage IV cancer (or substages)
  # Stage information is NOT explicit, but is rather represented by a triplet of
  # T/N/M measurements for each patient corresponding to a specific summary stage.

  # Notes:
  # 1) Prostate cancer doesn't have a stage 0, so there are 4 persons with stages I-IV -> total patients: 34
  # 2) Prostate cancer has only clinical categories in our mapping file
  # 3) M0 and MX are not valid pathological categories, so we have to use base or clinical codes for them
  # 4) Oesophageal cancer has clinical TNMs and pathological TNMs in the mapping file but not generic
  # 5) Melanoma of skin has clinical TNMs and pathological TNMs in the mapping file but not generic

  # skip_if(is.null(Sys.getenv("OPENAI_API_KEY")))
  #
  # patientGenerator <- PatientGenerator::patientChat$new()
  #
  # patientGenerator$prompt({
  #   "PERSON table:
  #     - A population of 34 persons all over 18 years old.
  #     - All persons have observation period from 2000 to 2024.
  #     - 17 persons are females with gender_concept_id = 8532.
  #     - 17 persons are males with gender_concept_id = 8507.
  #
  #   CONDITION_OCCURRENCE table:
  #   The patients from the PERSON table have occurrences of 7 different types of cancer recorded during their respective observation periods:
  #     - 5 patients have bladder cancer with condition_concept_id: 196360
  #     - 5 patients have breast cancer with condition_concept_id: 36556994
  #     - 5 patients have colorectal cancer with condition_concept_id: 40481902
  #     - 5 patients have lung cancer with condition_concept_id: 36535703
  #     - 5 patients have oesophageal cancer with condition_concept_id: 4181343
  #     - 4 patients have prostate cancer with condition_concept_id: 4163261
  #     - 5 patients have melanoma of skin with condition_concept_id: 141232
  #     - Everyone has condition_type_concept_id 32817
  #
  #   MEASUREMENT table:
  #   Cancer stage information is recorded in this table through TNM categories.
  #
  #   The persons with **bladder** cancer have the following measurements:
  #   - Person 1 has a Ta (concept ID: 1634071), a N0 (concept ID: 1634780) and a M0 (concept ID: 1633299).
  #   - Person 2 has a T1 (concept ID: 1635793), a N0 (concept ID: 1634780) and a M0 (concept ID: 1633299).
  #   - Person 3 has a T2a (concept ID: 1635181), a N0 (concept ID: 1634780) and a M0 (concept ID: 1633299).
  #   - Person 4 has a T2 (concept ID: 1633339), a N3 (concept ID: 1635585) and a M0 (concept ID: 1633299).
  #   - Person 5 has a Tis (concept ID: 1634720), a N1 (concept ID: 1633688) and a M1 (concept ID: 1633498).
  #
  #   The persons with **breast** cancer have the following measurements:
  #   - Person 1 has a Tis (concept ID: 1634720), a N0 (concept ID: 1634780) and a M0 (concept ID: 1633299).
  #   - Person 2 has a T1mi (concept ID: 1635751), a N0 (concept ID: 1634780) and a M0 (concept ID: 1633299).
  #   - Person 3 has a T3 (concept ID: 1633528), a N0 (concept ID: 1634780) and a M0 (concept ID: 1633299).
  #   - Person 4 has a T2 (concept ID: 1633339), a N3a (concept ID: 1634183) and a M0 (concept ID: 1633299).
  #   - Person 5 has a T1 (concept ID: 1635793), a N1 (concept ID: 1633688) and a M1 (concept ID: 1633498).
  #
  #   The persons with **colorectal** cancer have the following measurements:
  #   - Person 1 has a Tis (concept ID: 1634720), a N0 (concept ID: 1634780) and a M0 (concept ID: 1633299).
  #   - Person 2 has a T1 (concept ID: 1635793), a N0 (concept ID: 1634780) and a M0 (concept ID: 1633299).
  #   - Person 3 has a T3 (concept ID: 1633528), a N0 (concept ID: 1634780) and a M0 (concept ID: 1633299).
  #   - Person 4 has a T1 (concept ID: 1635793), a N2 (concept ID: 1635585) and a M0 (concept ID: 1633299).
  #   - Person 5 has a T1 (concept ID: 1635793), a N1a (concept ID: 1635731) and a M1 (concept ID: 1633498).
  #
  #   The persons with **lung** cancer have the following measurements:
  #   - Person 1 has a Tis (concept ID: 1634720), a N0 (concept ID: 1634780) and a M0 (concept ID: 1633299).
  #   - Person 2 has a T1c (concept ID: 1634817), a N0 (concept ID: 1634780) and a M0 (concept ID: 1633299).
  #   - Person 3 has a T2b (concept ID: 1634429), a N0 (concept ID: 1634780) and a M0 (concept ID: 1633299).
  #   - Person 4 has a T4 (concept ID: 1635242), a N3 (concept ID: 1634147) and a M0 (concept ID: 1633299).
  #   - Person 5 has a T4 (concept ID: 1635242), a N0 (concept ID: 1634780) and a M1 (concept ID: 1633498).
  #
  #   The persons with **oesophageal** cancer have the following measurements:
  #   - Person 1 has a clinical Tis (concept ID: 1633737), a N0 (concept ID: 1634070) and a M0 (concept ID: 1634757).
  #   - Person 2 has a pathological T1a (concept ID: 1633374), a N0 (concept ID: 1635560) and a M0 (concept ID: 1634757).
  #   - Person 3 has a clinical T2 (concept ID: 1634651), a N0 (concept ID: 1634070) and a M0 (concept ID: 1634757).
  #   - Person 4 has a pathological T1a (concept ID: 1633374), a N2 (concept ID: 1633336) and a M0 (concept ID: 1634757).
  #   - Person 5 has a clinical Tis (concept ID: 1633737), a N3 (concept ID: 1633854) and a M1 (concept ID: 1633974).
  #
  #   The persons with **prostate** cancer have the following measurements:
  #   - Person 1 has a T1 (concept ID: 1633883), a N0 (concept ID: 1634070) and a M0 (concept ID: 1634757).
  #   - Person 2 has a T2 (concept ID: 1634651), a N0 (concept ID: 1634070) and a M0 (concept ID: 1634757).
  #   - Person 3 has a T3b (concept ID: 1635813), a N0 (concept ID: 1634070) and a M0 (concept ID: 1634757).
  #   - Person 4 has a T4 (concept ID: 1634973), a N0 (concept ID: 1634070) and a M1c (concept ID: 1633784).
  #
  #   The persons with **melanoma of skin** cancer have the following measurements:
  #   - Person 1 has a pathlogical Tis (concept ID: 1633920), a N0 (concept ID: 1635560) and a M0 (concept ID: 1634757).
  #   - Person 2 has a clinical T2a (concept ID: 1635635), a N0 (concept ID: 1634070) and a M0 (concept ID: 1634757).
  #   - Person 3 has a pathological T3a (concept ID: 1635171), a N0 (concept ID: 1635560) and a M0 (concept ID: 1634757).
  #   - Person 4 has a clinical T1a (concept ID: 1635229), a N1 (concept ID: 1633651) and a M0 (concept ID: 1634757).
  #   - Person 5 has a pathological T0 (concept ID: 1634635), a N1 (concept ID: 1633659) and a M1 (concept ID: 1634891).
  #
  #   Output requirements:
  #   - All patients in PERSON have an observation period.
  #   - All conditions occurrences and measurement records of a patient must have happened during their observation period.
  #   - Fill out the condition end date 2023-12-31 for everyone."
  # })
  #
  # patientGenerator$save("test_patients_staging_uicc8")

  cdm <- TestGenerator::patientsCDM(
    testName = "test_patients_staging_uicc8",
    vocabulary = "v20260227_complete",
    cdmVersion = "5.4"
  )

  cdm <- createCancerCohorts(
    cdm = cdm,
    path = "cancer_cohorts",
    name = "cancer_cohorts"
  )

  # test 34 persons in population [(5 persons x 7 cancers) - 1]
  tot_person <- cdm$person |>
    dplyr::collect() |>
    nrow()
  expect_equal(tot_person, 34)

  # test 102 total measurements (3 measurements x 34 persons)
  tot_measurements <- cdm$measurement |>
    dplyr::collect() |>
    nrow()
  expect_equal(tot_measurements, 102)

  # test 5 people in each cancer cohort (except prostate_cancer)
  n_persons_per_cohort <- cdm$cancer_cohorts |>
    dplyr::count(cohort_definition_id) |>
    dplyr::filter(cohort_definition_id != 8) |>
    dplyr::pull(n) |>
    unique()
  expect_equal(n_persons_per_cohort, 5)

  # test 4 persons in prostate cancer cohort
  n_persons_prostate <- cdm$cancer_cohorts |>
    dplyr::count(cohort_definition_id) |>
    dplyr::filter(cohort_definition_id == 8) |>
    dplyr::pull(n)
  expect_equal(n_persons_prostate, 4)

  # test 3 measurements for each person
  n_measurements_per_person <- cdm$measurement |>
    dplyr::count(person_id) |>
    dplyr::pull(n) |>
    unique()
  expect_equal(n_measurements_per_person, 3)

  # integrity checks for condition and measurement dates
  invalid_conditions <- cdm$condition_occurrence |>
    dplyr::inner_join(
      cdm$observation_period,
      by = "person_id"
    ) |>
    dplyr::filter(
      condition_start_date < observation_period_start_date |
        condition_end_date > observation_period_end_date
    ) |> dplyr::collect() |>
    nrow()
  expect_equal(invalid_conditions, 0)

  invalid_measurements <- cdm$measurement |>
    dplyr::inner_join(
      cdm$observation_period,
      by = "person_id"
    ) |>
    dplyr::filter(
      measurement_date  < observation_period_start_date |
        measurement_date  > observation_period_end_date
    ) |> dplyr::collect() |>
    nrow()
  expect_equal(invalid_measurements, 0)

  # integrity checks for gender
  n_females <- cdm$person |>
    dplyr::collect() |>
    dplyr::filter(gender_concept_id == 8532) |>
    nrow()
  expect_equal(n_females, 17)

  n_males <- cdm$person |>
    dplyr::collect() |>
    dplyr::filter(gender_concept_id == 8507) |>
    nrow()
  expect_equal(n_males, 17)

  breast_cohort_id <- cdm$cancer_cohorts |>
    omopgenerics::settings() |>
    dplyr::filter(stringr::str_detect(cohort_name, "breast")) |>
    dplyr::pull(cohort_definition_id)

  breast_gender <- cdm$cancer_cohorts |>
    PatientProfiles::addSex() |>
    dplyr::filter(cohort_definition_id == breast_cohort_id) |>
    dplyr::pull(sex) |>
    unique()
  expect_equal(breast_gender, "Female")

  prostate_cohort_id <- cdm$cancer_cohorts |>
    omopgenerics::settings() |>
    dplyr::filter(stringr::str_detect(cohort_name, "prostate")) |>
    dplyr::pull(cohort_definition_id)

  prostate_gender <- cdm$cancer_cohorts |>
    PatientProfiles::addSex() |>
    dplyr::filter(cohort_definition_id == prostate_cohort_id) |>
    dplyr::pull(sex) |>
    unique()
  expect_equal(prostate_gender, "Male")
})


test_that("Create test patients with TNM measurements UICC 7th and 8th edition", {

  # This prompt creates a more general set of test patients

  # For every cancer type creates 10 test patients:
  # - 1 patient per stage (5 stages) per edition (2 editions).
  # Stage information is NOT explicit, but is rather represented by a triplet of T/N/M measurements
  # (from one of the two editions) for each patient, corresponding to a specific summary stage.
  # For every cancer, we will have:
  # - a patient with stage 0 cancer 7th edition
  # - a patient with stage I cancer (or substages) 7th edition
  # - a patient with stage II cancer (or substages) 7th edition
  # - a patient with stage III cancer (or substages) 7th edition
  # - a patient with stage IV cancer (or substages) 7th edition
  # - a patient with stage 0 cancer 8th edition
  # - a patient with stage I cancer (or substages) 8th edition
  # - a patient with stage II cancer (or substages) 8th edition
  # - a patient with stage III cancer (or substages) 8th edition
  # - a patient with stage IV cancer (or substages) 8th edition

  # Notes:
  # 1) Prostate cancer doesn't have a stage 0, so there are 4 persons with stages I-IV -> total patients: 68
  # 2) Prostate cancer has only clinical categories in our mapping file
  # 3) M0 and MX are not valid pathological categories, so we have to use base or clinical codes for them
  # 4) Oesophageal cancer has clinical TNMs and pathological TNMs in the mapping file but not generic
  # 5) Melanoma of skin has clinical TNMs and pathological TNMs in the mapping file but not generic

  # skip_if(is.null(Sys.getenv("OPENAI_API_KEY")))
  #
  # patientGenerator <- PatientGenerator::patientChat$new()
  #
  # patientGenerator$prompt({
  #   "PERSON table:
  #     - A population of 68 persons all over 18 years old.
  #     - All persons have observation period from 2000 to 2024.
  #     - 34 persons are females with gender_concept_id = 8532.
  #     - 34 persons are males with gender_concept_id = 8507.
  #
  #   CONDITION_OCCURRENCE table:
  #   The patients from the PERSON table have occurrences of 7 different types of cancer recorded during their respective observation periods:
  #     - 10 patients have bladder cancer with condition_concept_id: 196360.
  #     - 10 patients have breast cancer with condition_concept_id: 36556994 (all of them must be female with gender_concept_id = 8532).
  #     - 10 patients have colorectal cancer with condition_concept_id: 40481902.
  #     - 10 patients have lung cancer with condition_concept_id: 36535703.
  #     - 10 patients have oesophageal cancer with condition_concept_id: 4181343.
  #     - 8 patients have prostate cancer with condition_concept_id: 4163261 (all of them must be male with gender_concept_id = 8507).
  #     - 10 patients have melanoma of skin with condition_concept_id: 141232.
  #     - Everyone has condition_type_concept_id 32817.
  #
  #   MEASUREMENT table:
  #   Cancer stage information is recorded in this table through TNM categories.
  #
  #   The persons with **bladder** cancer have the following measurements:
  #   - One person has a Ta (concept ID: 1634394), a N0 (concept ID: 1633720) and a M0 (concept ID: 1633829) on different dates.
  #   - One person has a T1 (concept ID: 1633549), a N0 (concept ID: 1633720) and a M0 (concept ID: 1633829) on different dates..
  #   - One person has a T2a (concept ID: 1635532), a N0 (concept ID: 1633720) and a M0 (concept ID: 1633829) on different dates.
  #   - One person has a T3 (concept ID: 1635854), a N0 (concept ID: 1633720) and a M0 (concept ID: 1633829) on different dates.
  #   - One person has a T3 (concept ID: 1635854), a N1 (concept ID: 1635103) and a M1 (concept ID: 1633696) on different dates.
  #   - One person has a Ta (concept ID: 1634071), a N0 (concept ID: 1634780) and a M0 (concept ID: 1633299) on different dates.
  #   - One person has a T1 (concept ID: 1635793), a N0 (concept ID: 1634780) and a M0 (concept ID: 1633299) on different dates.
  #   - One person has a T2a (concept ID: 1635181), a N0 (concept ID: 1634780) and a M0 (concept ID: 1633299) on different dates.
  #   - One person has a T2 (concept ID: 1633339), a N3 (concept ID: 1635585) and a M0 (concept ID: 1633299) on different dates.
  #   - One person has a Tis (concept ID: 1634720), a N1 (concept ID: 1633688) and a M1 (concept ID: 1633498) on different dates.
  #
  #   The persons with **breast** cancer have the following measurements:
  #   - One person has a Tis (concept ID: 1633798), a N0 (concept ID: 1633720) and a M0 (concept ID: 1633829) on different dates.
  #   - One person has a T1a (concept ID: 1633764), a N0 (concept ID: 1633720) and a M0 (concept ID: 1633829) on different dates.
  #   - One person has a T2 (concept ID: 1634506), a N1 (concept ID: 1635103) and a M0 (concept ID: 1633829) on different dates.
  #   - One person has a T1mi (concept ID: 1634186), a N3b (concept ID: 1633277) and a M0 (concept ID: 1633829) on different dates.
  #   - One person has a T1 (concept ID: 1633549), a N1a (concept ID: 1634098) and a M1 (concept ID: 1633696) on different dates.
  #   - One person has a Tis (concept ID: 1634720), a N0 (concept ID: 1634780) and a M0 (concept ID: 1633299) on different dates.
  #   - One person has a T1mi (concept ID: 1635751), a N0 (concept ID: 1634780) and a M0 (concept ID: 1633299) on different dates.
  #   - One person has a T3 (concept ID: 1633528), a N0 (concept ID: 1634780) and a M0 (concept ID: 1633299) on different dates.
  #   - One person has a T2 (concept ID: 1633339), a N3a (concept ID: 1634183) and a M0 (concept ID: 1633299) on different dates.
  #   - One person has a T1 (concept ID: 1635793), a N1 (concept ID: 1633688) and a M1 (concept ID: 1633498) on different dates.
  #
  #   The persons with **colorectal** cancer have the following measurements:
  #   - One person has a Tis (concept ID: 1633798), a N0 (concept ID: 1633720) and a M0 (concept ID: 1633829) on different dates.
  #   - One person has a T2 (concept ID: 1634506), a N0 (concept ID: 1633720) and a M0 (concept ID: 1633829) on different dates.
  #   - One person has a T3 (concept ID: 1635854), a N0 (concept ID: 1633720) and a M0 (concept ID: 1633829) on different dates.
  #   - One person has a T1 (concept ID: 1633549), a N2 (concept ID: 1634240) and a M0 (concept ID: 1633829) on different dates.
  #   - One person has a T4a (concept ID: 1634724), a N2a (concept ID: 1633840) and a M1 (concept ID: 1633696) on different dates.
  #   - One person has a Tis (concept ID: 1634720), a N0 (concept ID: 1634780) and a M0 (concept ID: 1633299) on different dates.
  #   - One person has a T1 (concept ID: 1635793), a N0 (concept ID: 1634780) and a M0 (concept ID: 1633299) on different dates.
  #   - One person has a T3 (concept ID: 1633528), a N0 (concept ID: 1634780) and a M0 (concept ID: 1633299) on different dates.
  #   - One person has a T1 (concept ID: 1635793), a N2 (concept ID: 1635585) and a M0 (concept ID: 1633299) on different dates.
  #   - One person has a T1 (concept ID: 1635793), a N1a (concept ID: 1635731) and a M1 (concept ID: 1633498) on different dates.
  #
  #   The persons with **lung** cancer have the following measurements:
  #   - One person has a Tis (concept ID: 1633798), a N0 (concept ID: 1633720) and a M0 (concept ID: 1633829) on different dates.
  #   - One person has a T1b (concept ID: 1634319), a N0 (concept ID: 1633720) and a M0 (concept ID: 1633829) on different dates.
  #   - One person has a T2b (concept ID: 1635301), a N0 (concept ID: 1633720) and a M0 (concept ID: 1633829) on different dates.
  #   - One person has a T4 (concept ID: 1634576), a N0 (concept ID: 1633720) and a M0 (concept ID: 1633829) on different dates.
  #   - One person has a TX (concept ID: 1633390), a N3 (concept ID: 1633684) and a M1 (concept ID: 1633696) on different dates.
  #   - One person has a Tis (concept ID: 1634720), a N0 (concept ID: 1634780) and a M0 (concept ID: 1633299) on different dates.
  #   - One person has a T1c (concept ID: 1634817), a N0 (concept ID: 1634780) and a M0 (concept ID: 1633299) on different dates.
  #   - One person has a T2b (concept ID: 1634429), a N0 (concept ID: 1634780) and a M0 (concept ID: 1633299) on different dates.
  #   - One person has a T4 (concept ID: 1635242), a N3 (concept ID: 1634147) and a M0 (concept ID: 1633299) on different dates.
  #   - One person has a T4 (concept ID: 1635242), a N0 (concept ID: 1634780) and a M1 (concept ID: 1633498) on different dates.
  #
  #   The persons with **oesophageal** cancer have the following measurements:
  #   - One person has a Tis (concept ID: 1633798), a N0 (concept ID: 1633720) and a M0 (concept ID: 1633829) on different dates.
  #   - One person has a T1 (concept ID: 1633549), a N0 (concept ID: 1633720) and a M0 (concept ID: 1633829) on different dates.
  #   - One person has a T1 (concept ID: 1633549), a N1 (concept ID: 1635103) and a M0 (concept ID: 1633829) on different dates.
  #   - One person has a T3 (concept ID: 1635854), a N1 (concept ID: 1635103) and a M0 (concept ID: 1633829) on different dates.
  #   - One person has a T2 (concept ID: 1634506), a N0 (concept ID: 1633720) and a M1 (concept ID: 1633696) on different dates.
  #   - One person has a clinical Tis (concept ID: 1633737), a N0 (concept ID: 1634070) and a M0 (concept ID: 1634757) on different dates.
  #   - One person has a pathological T1a (concept ID: 1633374), a N0 (concept ID: 1635560) and a M0 (concept ID: 1634757) on different dates.
  #   - One person has a clinical T2 (concept ID: 1634651), a N0 (concept ID: 1634070) and a M0 (concept ID: 1634757) on different dates.
  #   - One person has a pathological T1a (concept ID: 1633374), a N2 (concept ID: 1633336) and a M0 (concept ID: 1634757) on different dates.
  #   - One person has a clinical Tis (concept ID: 1633737), a N3 (concept ID: 1633854) and a M1 (concept ID: 1633974) on different dates.
  #
  #   The persons with **prostate** cancer have the following measurements:
  #   - One person has a T1 (concept ID: 1633549), a N0 (concept ID: 1633720) and a M0 (concept ID: 1633829) on different dates.
  #   - One person has a T2c (concept ID: 1635192), a N0 (concept ID: 1633720) and a M0 (concept ID: 1633829) on different dates.
  #   - One person has a T3b (concept ID: 1635777), a N0 (concept ID: 1633720) and a M0 (concept ID: 1633829) on different dates.
  #   - One person has a T2 (concept ID: 1634506), a N1 (concept ID: 1635103) and a M1c (concept ID: 1635843) on different dates.
  #   - One person has a T1 (concept ID: 1633883), a N0 (concept ID: 1634070) and a M0 (concept ID: 1634757) on different dates.
  #   - One person has a T2 (concept ID: 1634651), a N0 (concept ID: 1634070) and a M0 (concept ID: 1634757) on different dates.
  #   - One person has a T3b (concept ID: 1635813), a N0 (concept ID: 1634070) and a M0 (concept ID: 1634757) on different dates.
  #   - One person has a T4 (concept ID: 1634973), a N0 (concept ID: 1634070) and a M1c (concept ID: 1633784) on different dates.
  #
  #   The persons with **melanoma of skin** cancer have the following measurements:
  #   - One person has a Tis (concept ID: 1634116), a N0 (concept ID: 1633726) and a M0 (concept ID: 1633468) on different dates.
  #   - One person has a T1 (concept ID: 1635422), a N0 (concept ID: 1633726) and a M0 (concept ID: 1633468) on different dates.
  #   - One person has a T2b (concept ID: 1635038), a N0 (concept ID: 1633726) and a M0 (concept ID: 1633468) on different dates.
  #   - One person has a T1b (concept ID: 1634236), a N1 (concept ID: 1634245) and a M0 (concept ID: 1633468) on different dates.
  #   - One person has a T1 (concept ID: 1635422), a N1 (concept ID: 1634245) and a M1 (concept ID: 1635336) on different dates.
  #   - One person has a pathlogical Tis (concept ID: 1633920), a N0 (concept ID: 1635560) and a M0 (concept ID: 1634757) on different dates.
  #   - One person has a clinical T2a (concept ID: 1635635), a N0 (concept ID: 1634070) and a M0 (concept ID: 1634757) on different dates.
  #   - One person has a pathological T3a (concept ID: 1635171), a N0 (concept ID: 1635560) and a M0 (concept ID: 1634757) on different dates.
  #   - One person has a clinical T1a (concept ID: 1635229), a N1 (concept ID: 1633651) and a M0 (concept ID: 1634757) on different dates.
  #   - One person has a pathological T0 (concept ID: 1634635), a N1 (concept ID: 1633659) and a M1 (concept ID: 1634891) on different dates.
  #
  #   Output requirements:
  #   - All patients in PERSON have an observation period.
  #   - All conditions occurrence and measurement dates of a patient must have happened within the range of their observation period.
  #   - Set the measurement date for the T/N/M measurements of 34 patients in a range of (-90, 0) days from index date (the date of cancer diagnosis).
  #   - Set the measurement date for the T/N/M measurements of 34 patients in a range of (0, 30) days from index date (the date of cancer diagnosis).
  #   - Fill out the condition end date 2023-12-31 for everyone."
  # })
  #
  # patientGenerator$save("test_patients_staging_general")

  cdm <- TestGenerator::patientsCDM(
    testName = "test_patients_staging_general",
    vocabulary = "v20260227_complete",
    cdmVersion = "5.4"
  )

  cdm <- createCancerCohorts(
    cdm = cdm,
    path = "cancer_cohorts",
    name = "cancer_cohorts"
  )

  # test 68 persons in population [(10 persons x 7 cancer types) - 2]
  tot_person <- cdm$person |>
    dplyr::collect() |>
    nrow()
  expect_equal(tot_person, 68)

  # test 204 total measurements (3 measurements x 68 persons)
  tot_measurements <- cdm$measurement |>
    dplyr::collect() |>
    nrow()
  expect_equal(tot_measurements, 204)

  # test 78 subjects in cancer cohorts [(10 persons x 8 cancer cohorts) - 2]
  # MEMO: lung_cancer_broad and lung_cancer_optimal
  tot_subjects_cohort <- cdm$cancer_cohorts |>
    dplyr::collect() |>
    nrow()
  expect_equal(tot_subjects_cohort, 78)

  # test 10 people in each cancer cohort (except prostate_cancer)
  n_persons_per_cohort <- cdm$cancer_cohorts |>
    dplyr::count(cohort_definition_id) |>
    dplyr::filter(cohort_definition_id != 8) |>
    dplyr::pull(n) |>
    unique()
  expect_equal(n_persons_per_cohort, 10)

  # test 8 persons in prostate cancer cohort
  n_persons_prostate <- cdm$cancer_cohorts |>
    dplyr::count(cohort_definition_id) |>
    dplyr::filter(cohort_definition_id == 8) |>
    dplyr::pull(n)
  expect_equal(n_persons_prostate, 8)

  # test 3 measurements for each person
  n_measurements_per_person <- cdm$measurement |>
    dplyr::count(person_id) |>
    dplyr::pull(n) |>
    unique()
  expect_equal(n_measurements_per_person, 3)

  # integrity checks for condition and measurement dates
  invalid_conditions <- cdm$condition_occurrence |>
    dplyr::inner_join(
      cdm$observation_period,
      by = "person_id"
    ) |>
    dplyr::filter(
      condition_start_date < observation_period_start_date |
        condition_end_date > observation_period_end_date
    ) |> dplyr::collect() |>
    nrow()
  expect_equal(invalid_conditions, 0)

  invalid_measurements <- cdm$measurement |>
    dplyr::inner_join(
      cdm$observation_period,
      by = "person_id"
    ) |>
    dplyr::filter(
      measurement_date  < observation_period_start_date |
        measurement_date  > observation_period_end_date
    ) |> dplyr::collect() |>
    nrow()
  expect_equal(invalid_measurements, 0)

  # integrity checks for gender
  n_females <- cdm$person |>
    dplyr::collect() |>
    dplyr::filter(gender_concept_id == 8532) |>
    nrow()
  expect_equal(n_females, 34)

  n_males <- cdm$person |>
    dplyr::collect() |>
    dplyr::filter(gender_concept_id == 8507) |>
    nrow()
  expect_equal(n_males, 34)

  breast_cohort_id <- cdm$cancer_cohorts |>
    omopgenerics::settings() |>
    dplyr::filter(stringr::str_detect(cohort_name, "breast")) |>
    dplyr::pull(cohort_definition_id)

  breast_gender <- cdm$cancer_cohorts |>
    PatientProfiles::addSex() |>
    dplyr::filter(cohort_definition_id == breast_cohort_id) |>
    dplyr::pull(sex) |>
    unique()
  expect_equal(breast_gender, "Female")

  prostate_cohort_id <- cdm$cancer_cohorts |>
    omopgenerics::settings() |>
    dplyr::filter(stringr::str_detect(cohort_name, "prostate")) |>
    dplyr::pull(cohort_definition_id)

  prostate_gender <- cdm$cancer_cohorts |>
    PatientProfiles::addSex() |>
    dplyr::filter(cohort_definition_id == prostate_cohort_id) |>
    dplyr::pull(sex) |>
    unique()
  expect_equal(prostate_gender, "Male")
})
