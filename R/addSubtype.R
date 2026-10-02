#' Add cancer subtypes to a cohort
#' 
#' @description
#' This function filters and appends specific cancer subtype information to the 
#' cohort data based on the provided cancer site, timeframe window, and intersection logic.
#'
#' @param cohort A cohort table with cancer patients from a cdm reference object.
#' @param cdm A cdm reference object.
#' @param cancer In character, the affected site, a choice of: 
#' "bladder", "breast", "colorectal", "lung", "melanoma", "oesophagus" and "prostate".
#' @param window to look up stages codes.
#' @param showIntersect If TRUE, the cohort will show the date intersects for each matching code. Default FALSE.
#' 
#' @importFrom omopgenerics validateCohortArgument
#' @importFrom omopgenerics validateCdmArgument
#' @importFrom omopgenerics assertList
#' @importFrom checkmate assertChoice
#' @importFrom checkmate assertLogical
#' @export
addSubtype <- function(
  cohort,
  cdm,
  cancer,
  window = list(c(-90, 90)),
  showIntersect = FALSE
) {
  # Assert parameters -------------------------
  checkmate::assertCharacter(cohort)
  omopgenerics::validateCohortArgument(cdm[[cohort]])
  omopgenerics::validateCdmArgument(cdm) 
  assertCancerCohortName(cdm[[cohort]], cancer)
  omopgenerics::assertList(window)
  checkmate::assertLogical(showIntersect)

  # Map rules ---------------------------------
  codelist <- subtypeCodelist(
    c(35957667L, 35948983L, 35955862L),
    cdm
  )
  ruleset <- readSubtypeRDS("mapping")

  # Intersection and mapping ------------------
  



  
  
}

subtypeCodelist <- function(
  concepts,
  cdm
) {
  checkmate::assertInteger(concepts)
  omopgenerics::validateCdmArgument(cdm)
  codelist <- list()
  for (i in seq_along(concepts)) {
    codelist[[extractConceptName(concepts[i], cdm)]] <- CodelistGenerator::getDescendants(cdm, concepts[i]) |> 
      dplyr::pull(concept_id)
  }
  codelist |> 
    omopgenerics::newCodelist(cdm)
  }

extractConceptName <- function(
  concept,
  cdm
  ) {
  CodelistGenerator::getDescendants(
    cdm,
    concept
  ) |> 
    dplyr::filter(
      concept_id == concept 
    ) |> 
    dplyr::pull(.data$concept_name) |> 
    tolower() |> 
    stringr::str_remove_all(
      "[()]"
    ) |> 
    stringr::str_replace_all(
      " ",
      "_"
    )
}

.mapSubtypeRules <- function(
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
        rowStages <- dplyr::pick(
          tidyselect::any_of(tolower(unique(c(ruleset$T, ruleset$N, ruleset$M))))) |>
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