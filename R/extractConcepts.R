#' Extract concept descendants from packaged concept sets
#'
#' Imports concept set JSON files from `inst/concept_sets/<path>`
#' and returns a codelist where each entry contains all descendants of each
#' concept in the source concept sets.
#'
#' @param cdm A CDM reference created with `CDMConnector`.
#' @param path A character string with the folder name under
#' `inst/concept_sets/` containing the concept set JSON files.
#'
#' @returns An `omopgenerics` codelist object where names are concept names and
#' values are vectors of descendant concept IDs.
#'
#' @importFrom checkmate assertDirectoryExists
#' @importFrom ParallelLogger logInfo
#' @importFrom glue glue
#' @importFrom omopgenerics importConceptSetExpression newCodelist
#' @importFrom dplyr filter select collect pull
#' @keywords internal
extractInnerConcepts <- function(
  cdm,
  path,
  cancerName
) {
  assertCharacteristic(
    path,
    characteristics = c(
      "biomarkers",
      "performance_status",
      "treatments_procedures",
      "stages"
    )
  )
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
  conceptSetExpression <- omopgenerics::importConceptSetExpression(
    path = pathToCohortJsonFiles,
    type = "json"
  )
  if (path == "treatments_procedures") {
    conceptSetExpression <- filterCodelist(
      codelist = conceptSetExpression,
      pattern = cancerName
    )
  }
  namesConceptSet <- conceptSetExpression |>
    names()
  descendants <- list()
  for (i in seq_along(conceptSetExpression)) {
    cli::cli_alert(
      glue::glue("Extracting codelist from '{namesConceptSet[i]}'")
    )
    conceptIds <- dplyr::pull(
      conceptSetExpression[[i]],
      concept_id
    )
    conceptSetExpressionNames <- dplyr::left_join(
      conceptSetExpression[[i]],
      dplyr::collect(
        dplyr::select(
          dplyr::filter(
            cdm$concept,
            concept_id %in% conceptIds
          ),
          concept_id,
          concept_name
          )
        ),
        by = "concept_id"
      )
    codelist <- purrr::pmap(
      conceptSetExpressionNames,
      extractDescendants,
      cdm
    ) |>
      purrr::flatten()
    if (length(names(descendants))) {
      if (any({names(codelist) %in% names(descendants)})) {
        codelist <- codelist[!names(codelist) %in% names(descendants)]
      }
    }
    descendants <- c(descendants, codelist)
  }
  descendantsCodelist <- omopgenerics::newCodelist(descendants)
  return(descendantsCodelist)
}

extractDescendants <- function(
  concept_id,
  excluded,
  descendants,
  mapped,
  concept_name,
  cdm
) {
  if (isTRUE(descendants)) {
    descendants_codes <- CodelistGenerator::getDescendants(
      cdm = cdm,
      conceptId = concept_id
    ) |>
      dplyr::pull(concept_id)
  } else {
    descendants_codes <- concept_id
  }
  if (isTRUE(excluded)) {
    descendants_codes <- descendants_codes
    setdiff(concept_id)
  }
  result <- setNames(
    list(descendants_codes),
    concept_name
  )
  return(result)
}

extractConceptIds <- function(
  codelist
) {
   result <- data.frame(
    "concept_set_name" = character(0),
    "concept_id" = numeric(0)
  )
  for (i in seq_along(codelist)) {
    result <- dplyr::bind_rows(data.frame(
      "concept_set_name" = names(codelist[i]),
      "concept_id" = codelist[[i]][1]
    ),
    result
  )
  }
  result <- result |>
    dplyr::arrange(concept_set_name)
  checkmate::assertDataFrame(result)
  return(result)
}
