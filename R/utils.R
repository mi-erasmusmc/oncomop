assertCharacteristic <- function(x, characteristics) {
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

filterCodelist <- function(
  codelist,
  pattern
) {
  codelist |>
    omopgenerics::assertList(
      named = TRUE
    )
  pattern |>
    checkmate::assertCharacter()
  index <- stringr::str_detect(
    names(codelist),
    pattern = pattern
  )
  codelist[index]
}

conceptSetsPath <- function() {
    system.file(
      "concept_sets",
    package = "oncomop"
  )
}
