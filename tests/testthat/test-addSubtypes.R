test_that("addSubtype to correct breast cancer cohort", {
  testName <- "subtypes_six_rules"
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
    c("erbb2_erb-b2_receptor_tyrosine_kinase_2_gene_variant_measurement",
      "esr1_estrogen_receptor_1_gene_variant_measurement", 
      "pgr_progesterone_receptor_gene_variant_measurement"
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
    c("pgr_progesterone_receptor_gene_variant_measurement",
      "esr1_estrogen_receptor_1_gene_variant_measurement", 
      "erbb2_erb-b2_receptor_tyrosine_kinase_2_gene_variant_measurement"
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
        "pgr_progesterone_receptor_gene_variant_measurement", 
        "erbb2_erb_b2_receptor_tyrosine_kinase_2_gene_variant_measurement", 
        "esr1_estrogen_receptor_1_gene_variant_measurement"
      ) |> 
          unlist(use.names = FALSE) |> 
          sort() |> 
          expect_equal(9191) # positive PGR
  
  subtypes |> 
    dplyr::filter(
      subject_id == 2 
    ) |> 
      select(
        "pgr_progesterone_receptor_gene_variant_measurement", 
        "erbb2_erb_b2_receptor_tyrosine_kinase_2_gene_variant_measurement", 
        "esr1_estrogen_receptor_1_gene_variant_measurement"
      ) |> 
          unlist(use.names = FALSE) |> 
          sort() |> 
          expect_equal(9191) # positive ESR1

  subtypes |> 
    dplyr::filter(
      subject_id == 3 
    ) |> 
      select(
        "pgr_progesterone_receptor_gene_variant_measurement", 
        "erbb2_erb_b2_receptor_tyrosine_kinase_2_gene_variant_measurement", 
        "esr1_estrogen_receptor_1_gene_variant_measurement"
      ) |> 
          unlist(use.names = FALSE) |> 
          sort() |> 
          expect_equal(c(9189, 9189)) # positive PGR and ESR1

  subtypes |> 
    dplyr::filter(
      subject_id == 4 
    ) |> 
      select(
        "pgr_progesterone_receptor_gene_variant_measurement", 
        "erbb2_erb_b2_receptor_tyrosine_kinase_2_gene_variant_measurement", 
        "esr1_estrogen_receptor_1_gene_variant_measurement"
      ) |> 
          unlist(use.names = FALSE) |> 
          sort() |> 
          expect_equal(c(9191)) # positive HER2

  subtypes |> 
    dplyr::filter(
      subject_id == 5 
    ) |> 
      select(
        "pgr_progesterone_receptor_gene_variant_measurement", 
        "erbb2_erb_b2_receptor_tyrosine_kinase_2_gene_variant_measurement", 
        "esr1_estrogen_receptor_1_gene_variant_measurement"
      ) |> 
          unlist(use.names = FALSE) |> 
          sort() |> 
          expect_equal(c(9189)) # negative HER2

  subtypes |> 
    dplyr::filter(
      subject_id == 6 
    ) |> 
      select(
        "pgr_progesterone_receptor_gene_variant_measurement", 
        "erbb2_erb_b2_receptor_tyrosine_kinase_2_gene_variant_measurement", 
        "esr1_estrogen_receptor_1_gene_variant_measurement"
      ) |> 
          unlist(use.names = FALSE) |> 
          sort() |> 
          expect_equal(c(9189, 9189, 9189)) # negative HER2

})
