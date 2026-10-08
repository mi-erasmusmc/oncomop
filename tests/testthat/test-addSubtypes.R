test_that("addSubtype to correct breast cancer cohort", {
  testName <- "subtypes_groups"
  cdm <- TestGenerator::patientsCDM(
    testName = testName,
    vocabulary = "v20260227_complete",
    cdmVersion = "5.4"
  ) |>
    createCancerCohorts(
      path = "cancer_cohorts",
      name = "cancer_cohorts"
    )
  expect_no_error(
    cdm$breast_cancer_subtypes <- cdm$cancer_cohorts |> 
      addSubtype(
        cdm = cdm,
        cancer = "breast",
        window = list(c(-90, 90)),
        name = "breast_cancer_subtypes",
        showIntersect = TRUE
      ) 
  )
  cdm$breast_cancer_subtypes |> 
    PatientProfiles::summariseResult() |> 
    dplyr::select(
      variable_name,
      estimate_name,
      estimate_value
    ) |> 
      dplyr::filter(
        variable_name %in% c("esr1_pgr_negative",
        "esr1_pgr_positive", "her2_positive",
        "triple_negative"),
        estimate_name == "count"
      ) |> 
        dplyr::pull("estimate_value") |> 
        sort() |> 
        expect_identical(
          c("1", "15", "3", "9")
        )
})

test_that("create subtypeCodelist correctly", {
  testName <- "subtypes_six_rules"
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
    c("erbb2",
      "esr1", 
      "pgr"
    )
  )
})

test_that("extractConceptName correctly", {
  testName <- "subtypes_six_rules"
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
    c("pgr",
      "esr1", 
      "erbb2"
    )
  )
})

test_that("subtypeIntersection correct breast cancer cohort", {
  testName <- "subtypes_six_rules"
  cdm <- TestGenerator::patientsCDM(
    testName = testName,
    vocabulary = "v20260227_complete",
    cdmVersion = "5.4"
  ) |>
    createCancerCohorts(
      path = "cancer_cohorts",
      name = "cancer_cohorts"
    ) 
  codelist <- subtypeCodelist(
    c(35957667L, 35948983L, 35955862L),
    cdm
  )
  window <- list(c(-90, 90))
  ruleset <- readSubtypeRDS("mapping")
  subtypes <- cdm$cancer_cohorts |>
    addSubtypeIntersect(
      conceptSet = codelist,
      indexDate = "cohort_start_date",
      censorDate = NULL,
      window = window,
      targetDate = "event_start_date",
      order = "first",
      inObservation = TRUE,
      nameStyle = "{concept_name}",
      name = NULL
    ) |> 
    dplyr::collect()

  subtypes |> 
    dplyr::filter(
      subject_id == 1 
    ) |> 
      select(
        "pgr", 
        "erbb2", 
        "esr1"
      ) |> 
          unlist(use.names = FALSE) |> 
          sort() |> 
          expect_equal(9191) # positive PGR
  
  subtypes |> 
    dplyr::filter(
      subject_id == 2 
    ) |> 
      select(
        "pgr", 
        "erbb2", 
        "esr1"
      ) |> 
          unlist(use.names = FALSE) |> 
          sort() |> 
          expect_equal(9191) # positive ESR1

  subtypes |> 
    dplyr::filter(
      subject_id == 3 
    ) |> 
      select(
        "pgr", 
        "erbb2", 
        "esr1"
      ) |> 
          unlist(use.names = FALSE) |> 
          sort() |> 
          expect_equal(c(9189, 9189)) # positive PGR and ESR1

  subtypes |> 
    dplyr::filter(
      subject_id == 4 
    ) |> 
      select(
        "pgr", 
        "erbb2", 
        "esr1"
      ) |> 
          unlist(use.names = FALSE) |> 
          sort() |> 
          expect_equal(c(9191)) # positive HER2

  subtypes |> 
    dplyr::filter(
      subject_id == 5 
    ) |> 
      select(
        "pgr", 
        "erbb2", 
        "esr1"
      ) |> 
          unlist(use.names = FALSE) |> 
          sort() |> 
          expect_equal(c(9189)) # negative HER2

  subtypes |> 
    dplyr::filter(
      subject_id == 6 
    ) |> 
      select(
        "pgr", 
        "erbb2", 
        "esr1"
      ) |> 
          unlist(use.names = FALSE) |> 
          sort() |> 
          expect_equal(c(9189, 9189, 9189)) # negative HER2

})

test_that("subtypeIntersection patients in subtype_groups", {
  testName <- "subtypes_groups"
  cdm <- TestGenerator::patientsCDM(
    testName = testName,
    vocabulary = "v20260227_complete",
    cdmVersion = "5.4"
  ) |>
    createCancerCohorts(
      path = "cancer_cohorts",
      name = "cancer_cohorts"
    ) 
  codelist <- subtypeCodelist(
    c(35957667L, 35948983L, 35955862L),
    cdm
  )
  window <- list(c(-90, 90))
  ruleset <- readSubtypeRDS("mapping")
  subtypes <- cdm$cancer_cohorts |>
    addSubtypeIntersect(
      conceptSet = codelist,
      indexDate = "cohort_start_date",
      censorDate = NULL,
      window = window,
      targetDate = "event_start_date",
      order = "first",
      inObservation = TRUE,
      nameStyle = "{concept_name}",
      name = NULL
    ) |> 
    dplyr::collect()

  subtypes |> 
    dplyr::filter(
      subject_id == 1 
    ) |> 
      select(
        "pgr", 
        "erbb2", 
        "esr1"
      ) |> 
          unlist(use.names = FALSE) |> 
          sort() |> 
          expect_equal(9191) # positive PGR
  
  subtypes |> 
    dplyr::filter(
      subject_id == 2 
    ) |> 
      select(
        "pgr", 
        "erbb2", 
        "esr1"
      ) |> 
          unlist(use.names = FALSE) |> 
          sort() |> 
          expect_equal(9189) # positive ESR1

  subtypes |> 
    dplyr::filter(
      subject_id == 3 
    ) |> 
      select(
        "pgr", 
        "erbb2", 
        "esr1"
      ) |> 
          unlist(use.names = FALSE) |> 
          sort() |> 
          expect_equal(c(9191)) # positive PGR and ESR1

  subtypes |> 
    dplyr::filter(
      subject_id == 4 
    ) |> 
      select(
        "pgr", 
        "erbb2", 
        "esr1"
      ) |> 
          unlist(use.names = FALSE) |> 
          sort() |> 
          expect_equal(c(9189)) # positive HER2

  subtypes |> 
    dplyr::filter(
      subject_id == 5 
    ) |> 
      select(
        "pgr", 
        "erbb2", 
        "esr1"
      ) |> 
          unlist(use.names = FALSE) |> 
          sort() |> 
          expect_equal(c(9191)) # negative HER2

  subtypes |> 
    dplyr::filter(
      subject_id == 6 
    ) |> 
      select(
        "pgr", 
        "erbb2", 
        "esr1"
      ) |> 
          unlist(use.names = FALSE) |> 
          sort() |> 
          expect_equal(c(9189)) # negative HER2

})

test_that(".mapSubtypeRules correct six rules", {
  testName <- "subtypes_six_rules"
  cdm <- TestGenerator::patientsCDM(
    testName = testName,
    vocabulary = "v20260227_complete",
    cdmVersion = "5.4"
  ) |>
    createCancerCohorts(
      path = "cancer_cohorts",
      name = "cancer_cohorts"
    ) 
  codelist <- subtypeCodelist(
    c(35957667L, 35948983L, 35955862L),
    cdm
  )
  subtypes_flags <- cdm$cancer_cohorts |>
    addSubtypeIntersect(
      conceptSet = codelist,
      indexDate = "cohort_start_date",
      censorDate = NULL,
      window = list(c(-90, 90)),
      targetDate = "event_start_date",
      order = "first",
      inObservation = TRUE,
      nameStyle = "{concept_name}",
      name = NULL
    ) |> 
    .mapSubtypeRules(
      cdm,
      name = "breast_cancer_cohorts"
    ) |> 
    dplyr::collect() |> 
    dplyr::arrange(subject_id) 
  subtypes_flags |>
    pull(esr1_pgr_positive) |>
    sum() |> 
    expect_equal(2)
  subtypes_flags |>
    pull(esr1_pgr_negative) |>
    sum() |> 
    expect_equal(2)
  subtypes_flags |>
    pull(her2_positive) |>
    sum() |> 
    expect_equal(2)
  subtypes_flags |>
    pull(triple_negative) |>
    sum() |> 
    expect_equal(1)

})

test_that(".mapSubtypeRules correct six rules", {
  testName <- "subtypes_groups"
  cdm <- TestGenerator::patientsCDM(
    testName = testName,
    vocabulary = "v20260227_complete",
    cdmVersion = "5.4"
  ) |>
    createCancerCohorts(
      path = "cancer_cohorts",
      name = "cancer_cohorts"
    ) 
  codelist <- subtypeCodelist(
    c(35957667L, 35948983L, 35955862L),
    cdm
  )
  cdm$cancer_cohorts |>
    addSubtypeIntersect(
      conceptSet = codelist,
      indexDate = "cohort_start_date",
      censorDate = NULL,
      window = list(c(-90, 90)),
      targetDate = "event_start_date",
      order = "first",
      inObservation = TRUE,
      nameStyle = "{concept_name}",
      name = NULL
    ) |> 
    .mapSubtypeRules(
      cdm,
      name = "breast_cancer_cohorts"
    ) |> 
    dplyr::collect() |> 
    dplyr::arrange(subject_id) |> 
    dplyr::select(
      -pgr, -erbb2, -esr1
    ) |> 
    PatientProfiles::summariseResult() |> 
    dplyr::select(
      variable_name,
      estimate_name,
      estimate_value
    ) |> 
      dplyr::filter(
        variable_name %in% c("esr1_pgr_negative",
        "esr1_pgr_positive", "her2_positive",
        "triple_negative"),
        estimate_name == "count"
      ) |> 
        dplyr::pull("estimate_value") |> 
        sort() |> 
        expect_identical(
          c("1", "15", "3", "9")
        )
})
