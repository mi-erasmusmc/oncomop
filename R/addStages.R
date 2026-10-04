#' Add TNM-derived cancer stages to a cohort
#'
#' Intersects a cancer cohort with TNM concepts and applies the packaged staging
#' rules to derive a summary cancer stage.
#'
#' @param cohort A cohort table in `cdm` containing cancer patients.
#' @param cdm A CDM reference containing `cohort`.
#' @param cancer A single supported cancer-site name, such as `"breast"`.
#' @param window A list of two-element numeric vectors giving the inclusive
#'   start and end offsets, in days, for TNM-code lookup relative to cohort start.
#'   Defaults to `list(c(0, 0))`.
#' @param edition A single staging-system edition. Defaults to `"8th"`.
#' @param type A single stage-rule grouping. Defaults to `"base"`.
#' @param order Whether the `"first"` or `"last"` matching code in each lookup
#'   window is used. Defaults to `"last"`.
#' @param showIntersect Whether to retain columns created during concept
#'   intersection. Defaults to `FALSE`.
#'
#' @returns A cohort table with a `cancer_stage` column. When `showIntersect` is
#'   `FALSE`, it contains only
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
#' @param tnm_concepts A data frame of packaged TNM concept definitions.
#' @param .edition A single staging-system edition used to filter concepts.
#' @param .type A single stage-rule grouping used to filter concepts.
#'
#' @returns An `omopgenerics` codelist of TNM concepts.
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
#' @param cohort A cohort table.
#' @param conceptSet An `omopgenerics` codelist object.
#' @param indexDate A column in `cohort` used as the lookup-date origin.
#' @param censorDate An optional column in `cohort` that censors lookup events.
#' @param window A list of two-element numeric lookup windows in days.
#' @param targetDate The event-date column used for the intersection.
#' @param order Whether the first or last matching event is selected.
#' @param inObservation Whether to limit lookup to the observation period.
#' @param nameStyle A glue specification used for generated column names.
#' @param name An optional name for the generated table.
#' @param ruleset A data frame that maps T, N, and M combinations to stages.
#'
#' @returns A cohort table with applied stage rules.
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
#' @param cohort A cohort table containing intersected TNM values.
#' @param ruleset A data frame containing the stage rules.
#' 
#' @returns A cohort table with a new `cancer_stage` column.
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
#' @param codelist An `omopgenerics` codelist containing stage concepts.
#'
#' @returns A codelist containing only parent TNM stage concepts.
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
#' @returns A character vector of supported cancer-site names.
#' @keywords internal
supportedCancerSites <- function() {
  readStagesRDS("mapping") |> 
    dplyr::pull(site) |> 
    unique()
}

#' Get supported editions
#'
#' @returns A character vector of supported staging-system editions.
#' @keywords internal
supportedEdition <- function() {
  readStagesRDS("mapping") |> 
    dplyr::pull(edition) |> 
    unique()
}

#' Get supported type groupings
#'
#' @returns A character vector of supported stage-rule groupings.
#' @keywords internal
supportedType <- function() {
  readStagesRDS("mapping") |> 
    dplyr::pull(stage_grouping_scope) |> 
    unique()
}
