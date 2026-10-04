#' Assert that cohort names match selected cancer site(s)
#'
#' Validates that all names in a cohort table correspond to the selected cancer
#' site(s). Cohort names must follow the `"<cancer>_cancer"` format.
#'
#' @param cohort A cohort table from a cdm reference object.
#' @param cancer A character vector of supported cancer sites (e.g., `"breast"`).
#'
#' @returns `NULL`, invisibly, if validation succeeds.
#'
#' @details
#' Throws an error of class `"Invalid cohort names"` if any cohort name does not
#' correspond to the selected cancer site(s).
#'
#' @importFrom checkmate assertSubset checkSubset
#' @importFrom cli cli_abort
#' @importFrom dplyr pull
#' @importFrom glue glue_collapse
#' @importFrom omopgenerics validateCohortArgument
#' @importFrom PatientProfiles addCohortName
#' @export
assertCancerCohortName <- function(
  cohort,
  cancer = "breast"
) {
  omopgenerics::validateCohortArgument(cohort)
  checkmate::assertSubset(
    cancer,
    supportedCancerSites()
    )
  cohort_names <- cohort |>
    PatientProfiles::addCohortName() |>
    dplyr::pull(cohort_name) |>
    unique()
  expected_cancer <- paste(
    cancer,
    "cancer",
    sep = "_"
  )
  cancer_in_cohort <- checkmate::checkSubset(
    cohort_names,
    expected_cancer
  )
  if (!isTRUE(cancer_in_cohort)) {
    cli::cli_abort(
      c(
        "!" = "Invalid cohort names: {glue::glue_collapse(cohort_names, sep = ', ', last = 'and')}",
        "x" = "They should be equal or a subset of: {glue::glue_collapse(expected_cancer, sep = ', ', last = 'and')}"
      ),
      class = "Invalid cohort names"
    )
  } else {
    return(invisible())
  }
  
}

#' Assert that a character vector contains only allowed characteristics
#'
#' Validates that all elements in the input vector are present in the 
#' provided list of allowed characteristics.
#'
#' @param x A character vector to validate.
#' @param characteristics A character vector of allowed characteristic names.
#'
#' @returns `NULL`, invisibly, if validation succeeds.
#'
#' @details
#' Throws an error of class `"Invalid characteristics"` if any element in `x` 
#' is not present in `characteristics`.
#'
#' @importFrom checkmate assertCharacter
#' @importFrom cli cli_abort
#' @keywords internal
assertCharacteristic <- function(
  x,
  characteristics
) {
  checkmate::assertCharacter(x)
  checkmate::assertCharacter(characteristics)
  invalidCharacteristics <- setdiff(
    x,
    characteristics
  )
  if (length(invalidCharacteristics) > 0) {
    cli::cli_abort(c(
      "!" = "Invalid characteristics: {paste(invalidCharacteristics, collapse = ', ')}",
      "i" = "Provide any of these specific characteristics: {paste0('- ', characteristics, collapse = '\n')}",
      "x" = "Your selected characteristic(s) is/are either misspelled or unavailable."
    ),
    class = "Invalid characteristics")
  } else {
    return(invisible())
  }
}
