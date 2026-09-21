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