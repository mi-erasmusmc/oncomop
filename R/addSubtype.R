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
  name = "breast_cancer_subtypes",
  showIntersect = FALSE
) {

  # Assert parameters -------------------------
  omopgenerics::validateCohortArgument(cohort)
  omopgenerics::validateCdmArgument(cdm) 
  assertCancerCohortName(cohort, cancer)
  omopgenerics::assertList(window)
  checkmate::assertLogical(showIntersect)

  # Map rules ---------------------------------
  codelist <- subtypeCodelist(
    c(35957667L, 35948983L, 35955862L),
    cdm
  )

  # Intersection and mapping ------------------
  cohort |>
    addSubtypeIntersect(
      conceptSet = codelist,
      indexDate = "cohort_start_date",
      censorDate = NULL,
      window = window,
      targetDate = "event_start_date",
      order = "first",
      inObservation = TRUE,
      nameStyle = "{concept_name}",
      name = NULL
    ) |> 
    .mapSubtypeRules(
      cdm,
      name = name
    ) |> 
    dplyr::select(
      -pgr, -erbb2, -esr1
    )
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
      dplyr::pull(.data$concept_id)
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
      .data$concept_id == concept
    ) |> 
    dplyr::pull(.data$concept_name) |> 
    stringr::str_remove_all(
      "[()]"
    ) |> 
    stringr::str_replace_all(
      " ",
      "_"
    ) |> 
    stringr::str_extract(
      "^[^_]+"
    ) |> 
    tolower()
}

.mapSubtypeRules <- function(
  cohort,
  cdm,
  name
) {
  omopgenerics::validateCohortArgument(cohort)
  omopgenerics::validateCdmArgument(cdm)
  checkmate::assertCharacter(name)
  cancer_subtype <- cohort |>
    dplyr::collect() |>
    dplyr::rowwise() |>
    # dplyr::select_if(~ !all(is.na(.))) |>
    dplyr::mutate(
      esr1_pgr_positive = dplyr::case_when(
          isTRUE(.data$esr1 == 9191) | isTRUE(.data$pgr == 9191) ~ 1,
          .default = 0
      ),
      esr1_pgr_negative = dplyr::case_when(
          isTRUE(.data$pgr == 9189) & isTRUE(.data$esr1 == 9189) ~ 1,
          .default = 0
      ),
      her2_positive = dplyr::case_when(
          isTRUE(.data$erbb2 == 9191) ~ 1,
          .default = 0
      ),
      her2_positive = dplyr::case_when(
          isTRUE(.data$erbb2 == 9189) ~ 1,
          .default = 0
      ),
      triple_negative = dplyr::case_when(
          isTRUE(.data$pgr == 9189) & isTRUE(.data$esr1 == 9189) & isTRUE(.data$erbb2 == 9189) ~ 1,
          .default = 0
      )
    ) |> 
    tibble::as_tibble() 
  cancerCohortTableName <- omopgenerics::uniqueTableName()
  cdm <- omopgenerics::insertTable(
    cdm = cdm,
    name = cancerCohortTableName,
    table = cancer_subtype
  ) 
  cdm[[name]] <- omopgenerics::newCohortTable(
    cdm[[cancerCohortTableName]]
  ) |>
    dplyr::compute(
      name = name
    )
  return(cdm[[name]])
}
