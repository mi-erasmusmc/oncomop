#' `addStages()` to a cohort
#'
#' It uses a codelist to date intersect with a cancer cohort.
#' Imposes a predefined or custom set of rules to identify
#' summary stages.
#'
#' @param cohort A cohort table with cancer patients from a
#' cdm reference object.
#' @param cdm A cdm reference object.
#' @param cancer In character, the affected site, a choice of:
#' "bladder", "breast", "colorectal", "lung", "melanoma", "oesophagus"
#' and "prostate".
#' @param window to look up stages codes.
#' @param edition A choice of "unspecified", "7th" and "8th".
#' @param type A choice from "base", "clinical" or "pathological" stage rule.
#' @param order A choice from "first" or "last". If more than one code
#' intersected, the order defines which code to intersect in the window.
#' @param showTnm If TRUE, the cohort will show the date intersects
#' for each matching code. Default FALSE.
#' @importFrom omopgenerics validateCohortArgument validateCdmArgument assertList newCodelist
#' @importFrom checkmate assertChoice assertFileExists assertTRUE assertDataFrame
#' @importFrom dplyr filter pull rowwise select_if mutate pick select
#' @importFrom PatientProfiles addConceptIntersectDate
#' @importFrom tidyselect any_of
#' @importFrom stringr str_detect
#' @returns A cohort table containing the identified cancer stages.
#' @export
addStages <- function(
  cohort,
  cdm,
  cancer,
  window = list(c(0,0)),
  edition = "8th",
  type = "base",
  order = "last",
  showTnm = FALSE
) {

  # Assert parameters ---------------------------------
  cohort |>
    omopgenerics::validateCohortArgument()
  cdm |>
    omopgenerics::validateCdmArgument()
  window |>
    omopgenerics::assertList()
  edition |>
    checkmate::assertChoice(
      c("unspecified", "7th", "8th")
    )
  type |>
    checkmate::assertChoice(
      c("base", "clinical", "pathological")
    )

  # Extract codelist for intersection -----------------
  # At this point, we can put any codelist for stages, subtypes, progression
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

  # .addColumnRules() ---------------------------------
  # General function to analyse if it can be reused for
  # subtypes and progression
  cancer_stage_cohort <- cohort |>
    .addColumnRules(
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

  if (isFALSE(showTnm)) {
    cancer_stage_cohort |>
      dplyr::select(
        cohort_definition_id,
        subject_id,
        cohort_start_date,
        cohort_end_date,
        cancer_stage
      )
  } else {
    return(cancer_stage_cohort)
  }
}

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

.addColumnRules <- function(
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
    .mapRules(ruleset)
}

.mapRules <- function(
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
