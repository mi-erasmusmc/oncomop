#' Create cancer cohorts based on phenotype specifications
#'
#' The function `createCancerCohorts` relies on the `CohortConstructor` package to generate cohorts
#' based on one or more concept sets and additional phenotype specifications.
#' In this study there are seven cancer types of interest: bladder cancer (1937),
#' breast cancer (4415), colorectal cancer (1444), esophageal cancer (1414),
#' lung cancer (1413), prostate cancer (1489) and skin melanoma (1451).
#' To each cancer type are applied the following phenotype specifications:
#' - Study period: from 01-01-2010 to the end of observation period.
#' - Inclusion criteria:
#'   1. The date of the first diagnosis of cancer must fall within the study period.
#'   2. The patient's age must be \eqn{\geq 18} at the time of cancer diagnosis.
#' - Exclusion criteria:
#'   1. Exclude patients with death before and at index date.
#'   2. Exclude males from patients with breast cancer.
#'   3. Exclude females from patients with prostate cancer.
#'
#' @param cdm A cdm instance.
#' @param path The character name of the folder where the concept
#' sets are saved within the study package, default `"concept_sets"`.
#' @param name The character name of the new cohort table to be created.
#' @param cancer A character vector of cancer types to include in the cohort, default `"all"`. Other possible values are
#' `"bladder_cancer"`, `"breast_cancer"`, `"colorectal_cancer"`, `"lung_cancer"`, `"melanoma_of_skin"`, `"oesophageal_cancer"`, `"prostate_cancer"`.
#'
#' @importFrom ParallelLogger logInfo
#' @importFrom glue glue
#' @importFrom checkmate assertDirectoryExists
#' @importFrom omopgenerics importConceptSetExpression settings
#' @importFrom CodelistGenerator asCodelist
#' @importFrom CohortConstructor conceptCohort requireIsFirstEntry requireAge requireInDateRange exitAtObservationEnd requireTableIntersect requireSex
#' @importFrom dplyr filter pull
#' @importFrom stringr str_detect
#'
#' @returns A cdm instance including with the newly created cohort table.
#' @export
createCancerCohorts <- function(
    cdm,
    path = "cancer_cohorts",
    name,
    cancer = "all"
    ) {

  pathToCohortJsonFiles <- system.file(
    "concept_sets",
    path,
    package = "oncomop"
    )

  checkmate::assertDirectoryExists(pathToCohortJsonFiles)

  ParallelLogger::logInfo(
    glue::glue(
      "Creating codelist from {pathToCohortJsonFiles}"
      )
    )

  codelist <- omopgenerics::importConceptSetExpression(
    path = pathToCohortJsonFiles,
    type = "json")

  available_cancers <- names(codelist)

  checkmate::assertCharacter(cancer, min.len = 1, any.missing = FALSE)

  # Validate selected cancer types and create codelist accordingly
  if (identical(cancer, "all")) {

    codelist <- codelist |>
      CodelistGenerator::asCodelist(cdm)

  } else if (length(setdiff(cancer, available_cancers)) > 0) {

    cli::cli_abort(c(
      "!" = "Unknown cancer type(s): {cancer}",
      "i" = "Available cancer types are:", paste0("- ", available_cancers),
      "x" = "Your selected cancer type is either misspelled or unavailable."
    ))

  } else {

    codelist <- codelist[cancer] |>
      CodelistGenerator::asCodelist(cdm)

  }

  # Diagnosis of one of the selected types of cancer
  cdm[[name]] <- CohortConstructor::conceptCohort(
    cdm,
    conceptSet = codelist,
    name = name,
    exit = "event_end_date",
    overlap = "merge",
    table = NULL,
    useRecordsBeforeObservation = FALSE,
    useSourceFields = FALSE,
    subsetCohort = NULL,
    subsetCohortId = NULL
    ) |>
    # First record in patient's history
    CohortConstructor::requireIsFirstEntry() |>
    # Age ≥18 years at the date of cancer diagnosis
    CohortConstructor::requireAge(
      indexDate = "cohort_start_date",
      ageRange = list(c(18, 150))
    ) |>
    # Date of the first diagnosis of a selected type
    # of cancer during the study period
    CohortConstructor::requireInDateRange(
      indexDate = "cohort_start_date",
      dateRange = as.Date(c("2010-01-01", NA))
      ) |>
    # Study period ends at end of observation period
    CohortConstructor::exitAtObservationEnd() |>
    # Exclude patients with death before index date
    CohortConstructor::requireTableIntersect(
      tableName = "death",
      window = c(-Inf, -1),
      intersections = 0
      ) |>
    # Exclude patients with death on index date
    CohortConstructor::requireTableIntersect(
      tableName = "death",
      window = c(0, 0),
      intersections = 0
    )

  # Impose sex requirements for specific cohorts
  breast_cancer_ids <- omopgenerics::settings(cdm[[name]]) |>
    dplyr::filter(stringr::str_detect(cohort_name, "breast_cancer")) |>
    dplyr::pull(cohort_definition_id)

  prostate_cancer_ids <- omopgenerics::settings(cdm[[name]]) |>
    dplyr::filter(stringr::str_detect(cohort_name, "prostate_cancer")) |>
    dplyr::pull(cohort_definition_id)

  if (length(breast_cancer_ids) > 0) {
    cdm[[name]] <- cdm[[name]] |>
      CohortConstructor::requireSex(
        sex = "Female",
        cohortId = breast_cancer_ids,
        name = name
      )
  }

  if (length(prostate_cancer_ids) > 0) {
    cdm[[name]] <- cdm[[name]] |>
      CohortConstructor::requireSex(
        sex = "Male",
        cohortId = prostate_cancer_ids,
        name = name
      )
  }

  return(cdm)

}

#' Creates cancer-related characteristics cohorts
#'
#' The function `createCharacteristicsCohorts` relies on the `CohortConstructor` package to generate cohorts
#' based on one or more concept sets and additional phenotype specifications.
#' In this study the cancer-related characteristics that will be analysed are: cancer-related,
#' characteristics (i.e. cancer stage and grade, cancer-specific biomarkers, performance status,
#' cancer treatments, and laboratory tests and procedures)
#'
#' @param cdm A cdm instance.
#' @param path The character name of the folder where the concept
#' sets are saved within the study package, default `"concept_sets"`.
#' @param name The character name of the new cohort table to be created.
#' @param characteristics A character vector of cancer-related characteristics to include in the cohort, default `"all"`. Other possible values are
#' "biomarkers", "cancer_cohorts_deck", "cancer_progression", "grade", "radiotherapy", "stage", "surgery" and "treatments_procedures"
#'
#' @importFrom ParallelLogger logInfo
#' @importFrom glue glue
#' @importFrom checkmate assertDirectoryExists
#' @importFrom omopgenerics importConceptSetExpression settings
#' @importFrom CodelistGenerator asCodelist
#' @importFrom CohortConstructor conceptCohort requireIsFirstEntry requireAge requireInDateRange exitAtObservationEnd requireTableIntersect requireSex
#' @importFrom dplyr filter pull
#' @importFrom stringr str_detect
#'
#' @returns A cdm instance including with the newly created cohort table.
#' @export
createCharacteristicsCohorts <- function(
    cdm,
    path = "concept_sets",
    name = "cancer_characteristics",
    characteristics = "all"
    ) {

  ParallelLogger::logInfo(
    "Creating characteristics cohorts"
  )
  pathToCohortJsonFiles <- system.file(
    path,
    package = "P4C5006"
    )

  checkmate::assertDirectoryExists(pathToCohortJsonFiles)

  all_characteristics <- list.files(
    pathToCohortJsonFiles
  ) |>
    stringr::str_subset(
      pattern = "cancer_cohorts",
      negate = TRUE
    )

  if (identical(characteristics, "all")) {

    characteristics <- all_characteristics

  } else {

    missing_characteristics <- setdiff(characteristics, all_characteristics)
    if (length(missing_characteristics) > 0) {
      cli::cli_abort(c(
        "!" = "Unknown cancer-related characteristic(s): {missing_characteristics}",
        "i" = "Available cancer-related characteristic(s) are:\n{paste0('- ', all_characteristics, collapse = '\n')}",
        "x" = "Your selected cancer-related characteristic(s) is either misspelled or unavailable."
      ))
    }

  }

  result_codelist <- list()

  for (i in seq_along(characteristics)) {
    ParallelLogger::logInfo(
      glue::glue(
        "Creating codelist from {pathToCohortJsonFiles}"
      )
    )

    pathToCohortJsonFiles <- system.file(
      "concept_sets",
      characteristics[i],
      package = "P4C5006"
    )

    characteristics_codelist <- omopgenerics::importConceptSetExpression(
      path = pathToCohortJsonFiles,
      type = "json"
    ) |>
      CodelistGenerator::asCodelist(cdm)

    if (length(characteristics_codelist) == 0) {
      skip
    } else {
      result_codelist[characteristics[i]] <- characteristics_codelist
    }

  }

  characteristics_concept_sets <- omopgenerics::newCodelist(result_codelist)

  # Diagnosis of one of the selected types of cancer
  cdm[[name]] <- CohortConstructor::conceptCohort(
    cdm,
    conceptSet = characteristics_concept_sets,
    name = name,
    exit = "event_end_date",
    overlap = "merge",
    table = NULL,
    useRecordsBeforeObservation = FALSE,
    useSourceFields = FALSE,
    subsetCohort = NULL,
    subsetCohortId = NULL
    )

  return(cdm)

}
