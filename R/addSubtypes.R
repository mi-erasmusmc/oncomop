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
addSubtypes <- function(
  cohort,
  cdm,
  cancer,
  window = list(c(0,0)),
  showIntersect = FALSE
) {
  # Assert parameters ---------------------------------
  omopgenerics::validateCohortArgument(cohort)
  omopgenerics::validateCdmArgument(cdm) 
  checkmate::assertChoice(cancer, supportedCancerSites())
  omopgenerics::assertList(window)
  checkmate::assertLogical(showIntersect)

  
  
}