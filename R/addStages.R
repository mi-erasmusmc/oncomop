#' `addStages()` to a cohort
#'
#' It uses a codelist to date intersect with a cancer cohort.
#' Imposes a predefined or custom set of rules to identify
#' summary stages.
#'
#' @param cohort A cohort table with cancer patients from a
#' cdm reference object.
#' @param cdm A cdm reference object.
#' @param cancer A character string specifying the affected site (e.g., `"breast"`). 
#'   Must be a valid value from `supportedCancerSites()`.
#' @param window A list of numeric vectors defining the date windows for 
#'   looking up stage codes.
#' @param edition A character string specifying the edition (e.g., `"8th"`). 
#'   Must be a valid value from `supportedEdition()`.
#' @param type A character string specifying the stage rule type 
#'   (e.g., `"base"`). Must be a valid value from `supportedType()`.
#' @param order A character string, either `"first"` or `"last"`. If more than 
#'   one code intersected, the order defines which code to intersect in the window.
#' @param showIntersect A logical value. If `TRUE`, the cohort will include 
#'   the date intersects for each matching code. If `FALSE`, only the 
#'   final `cancer_stage` column is returned. Default is `FALSE`.
#' @importFrom omopgenerics validateCohortArgument validateCdmArgument assertList newCodelist
#' @importFrom checkmate assertChoice assertLogical assertFileExists assertTRUE assertDataFrame
#' @importFrom dplyr filter pull rowwise select_if mutate pick select
#' @importFrom PatientProfiles addConceptIntersectDate
#' @importFrom tidyselect any_of
#' @importFrom stringr str_detect
#' @returns A cohort table containing the identified cancer stages. 
#'   If `showIntersect` is `FALSE`, the returned table contains only: 
#'   `cohort_definition_id`, `subject_id`, `cohort_start_date`, 
#'   `cohort_end_date`, and `cancer_stage`.
#' @export
addStages <- function(
  cohort,
  cdm,
  cancer,
  window = list(c(0,0)),
  edition = "8th",
  type = "base",
  order = "last",
  showIntersect = FALSE
) {

  # Assert parameters ---------------------------------
  omopgenerics::validateCohortArgument(cohort)
  omopgenerics::validateCdmArgument(cdm) 
  checkmate::assertChoice(cancer, supportedCancerSites())
  omopgenerics::assertList(window)
  checkmate::assertChoice(
    edition,
    c("unspecified", supportedEdition())
  )
  checkmate::assertChoice(type, supportedType())
  checkmate::assertChoice(order, c("first", "last"))
  checkmate::assertLogical(showIntersect)

  # Extract codelist for intersection -----------------
  tnm_codelist <- readStagesRDS("concepts") |>
    createTNMCodelist(
      .edition = edition,
      .type = type
    )

  # Extract ruleset -----------------------------------
  ruleset <- readStagesRDS("mapping") |>
    extractStageRuleset(
      .cancer = cancer,
      .edition = edition,
      .type = "base"
    )

  # Intersect cohorts ---------------------------------
  cancer_stage_cohort <- cohort |>
    .addStageRules(
      conceptSet = tnm_codelist,
      indexDate = "cohort_start_date",
      censorDate = NULL,
      window = window,
      targetDate = "event_start_date",
      order = order,
      inObservation = TRUE,
      nameStyle = "{concept_name}",
      name = NULL,
      ruleset = ruleset
    )

  # Leave intersections in the cohort or not -----------
  if (isFALSE(showIntersect)) {
    return(
      cancer_stage_cohort |>
        dplyr::select(
          cohort_definition_id,
          subject_id,
          cohort_start_date,
          cohort_end_date,
          cancer_stage
        )
    )
  } else {
    return(cancer_stage_cohort)
  }
}

#' Create a TNM codelist from concept data
#'
#' @param tnm_concepts A data frame containing TNM concept information.
#' @param .edition A character string specifying the edition.
#' @param .type A character string specifying the type.
#' 
#' @return An `omopgenerics` codelist object.
#' @keywords internal
createTNMCodelist <- function(
  tnm_concepts,
  .edition,
  .type
) {
  checkmate::assertDataFrame(tnm_concepts)
  tnm_stages_concept <- tnm_concepts |>
    dplyr::filter(
      .data$classification_version == .edition,
      .data$type == .type,
    ) |>
    dplyr::filter(
      !is.na(.data$concept_id)
    )
  tnm_codelist <- tnm_stages_concept |>
    dplyr::pull(
      concept_id
    ) |> lapply(
      FUN = function(x) {
        return(x)
      }
    ) |> setNames(
      tnm_stages_concept$component_tnm
    ) |>
    omopgenerics::newCodelist()
  return(tnm_codelist)
}

#' Add stage rules to a cohort
#'
#' @param cohort A cohort table from a cdm reference object.
#' @param conceptSet An `omopgenerics` codelist object.
#' @param indexDate A character string specifying the index date column.
#' @param censorDate A character string specifying the censor date column.
#' @param window A list of numeric vectors defining the search windows.
#' @param targetDate A character string specifying the target date column.
#' @param order A character string, either `"first"` or `"last"`.
#' @param inObservation A logical value.
#' @param nameStyle A character string for the naming style.
#' @param name A character string for the name.
#' @param ruleset A data frame containing the stage rules.
#' 
#' @return A cohort table with applied stage rules.
#' @keywords internal
.addStageRules <- function(
  cohort,
  conceptSet,
  indexDate = "cohort_start_date",
  censorDate = NULL,
  window = list(c(0,0)),
  targetDate = "event_start_date",
  order = "last",
  inObservation = TRUE,
  nameStyle = "{concept_name}",
  name = NULL,
  ruleset
) {
  omopgenerics::validateCohortArgument(cohort)
  cohort |>
    PatientProfiles::addConceptIntersectDate(
      conceptSet,
      indexDate = "cohort_start_date",
      censorDate = NULL,
      window = window,
      targetDate = "event_start_date",
      order = "last",
      inObservation = TRUE,
      nameStyle = "{concept_name}",
      name = NULL
    ) |>
    .mapStageRules(ruleset)
}

#' Map stage rules to a cohort
#'
#' @param cohort A cohort table.
#' @param ruleset A data frame containing the stage rules.
#' 
#' @return A cohort table with a new `cancer_stage` column.
#' @keywords internal
.mapStageRules <- function(
  cohort,
  ruleset
) {
  omopgenerics::validateCohortArgument(cohort)
  checkmate::assertDataFrame(ruleset)
  cohort |>
    dplyr::collect() |>
    dplyr::rowwise() |>
    dplyr::select_if(~ !all(is.na(.))) |>
    dplyr::mutate(
      cancer_stage = {
        rowStages <- dplyr::pick(tidyselect::any_of(tolower(unique(c(ruleset$T, ruleset$N, ruleset$M))))) |>
          dplyr::select_if(~ !any(is.na(.)))
        stageCombination <- names(rowStages)
        rowStageT <- stageCombination[names(rowStages) |> stringr::str_detect("t")]
        rowStageN <- stageCombination[names(rowStages) |> stringr::str_detect("n")]
        rowStageM <- stageCombination[names(rowStages) |> stringr::str_detect("m")]
        stage <- ruleset |>
          dplyr::select(
            T, N, M, uicc_stage
          ) |>
          dplyr::filter(
            tolower(T) == rowStageT,
            tolower(N) == rowStageN,
            tolower(M) == rowStageM,
          ) |>
          dplyr::pull(uicc_stage)
      }
    )
}

#' Filter stage concepts from a codelist
#'
#' @param codelist An `omopgenerics` codelist object.
#' 
#' @return A filtered codelist containing only parent stage concepts.
#' @keywords internal
filterStageConcepts <- function(
  codelist
) {
  codelist |>
    omopgenerics::assertList()
  filterParents <- names(codelist) |>
    stringr::str_detect(
      pattern = "\\b[TMN]\\d\\b"
    )
  codelist[filterParents]
}

#' Get supported cancer sites
#'
#' @return A character vector of supported cancer sites.
#' @keywords internal
supportedCancerSites <- function() {
  readStagesRDS("mapping") |> 
    dplyr::pull(site) |> 
    unique()
}

#' Get supported editions
#'
#' @return A character vector of supported editions.
#' @keywords internal
supportedEdition <- function() {
  readStagesRDS("mapping") |> 
    dplyr::pull(edition) |> 
    unique()
}

#' Get supported type groupings
#'
#' @return A character vector of supported type groupings.
#' @keywords internal
supportedType <- function() {
  readStagesRDS("mapping") |> 
    dplyr::pull(stage_grouping_scope) |> 
    unique()
}
