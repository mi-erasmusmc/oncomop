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
    pull(cohort_name) |> 
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
      "!" = "Invalid characteristics: {invalidCharacteristics}",
      "i" = "Provide any of this specific characteristics: are:\n{paste0('- ', characteristics, collapse = '\n')}",
      "x" = "Your selected characteristic(s) is/are either misspelled or unavailable."
    ),
    class = "Invalid characteristics")
  } else {
    return(invisible())
  }
}