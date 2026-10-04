#' Add cancer subtypes to a cohort
#' 
#' This function filters and appends specific cancer subtype information to the 
#' cohort data based on the provided cancer site, timeframe window, and intersection logic.
#'
#' @param cohort A character scalar naming a cohort table in `cdm`.
#' @param cdm A CDM reference containing `cohort`.
#' @param cancer A single supported cancer-site name.
#' @param window A list of two-element numeric vectors giving subtype-code lookup
#'   windows in days relative to cohort start. Defaults to `list(c(-90, 90))`.
#' @param showIntersect Whether to retain columns created during concept
#'   intersection. Defaults to `FALSE`.
#'
#' @returns A cohort table with identified cancer subtypes.
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
  name = "cancer_cohorts"
) {
  omopgenerics::validateCohortArgument(cohort)
  omopgenerics::validateCdmArgument(cdm)
  checkmate::assertCharacter(name)
  rules <- readSubtypeRDS("mapping") 
  rule_cols <- setdiff(names(rules), "subtype")
  cancer_cohorts_subtype <- cohort |>
    dplyr::collect() |>
    dplyr::rowwise() |>
    dplyr::mutate(
      subtype = {
        current_vals <- c(pgr, esr1, erbb2) 
        match_idx <- which(apply(rules[, rule_cols, drop = FALSE], 1, function(rule_row) {
          all(ifelse(is.na(rule_row), 
                     is.na(current_vals), 
                     current_vals == rule_row))
        }))
        if (length(match_idx) > 0) {
          rules$subtype[match_idx[1]]
        } else {
          "No subtype found"
        }
      }
    ) |> 
    dplyr::ungroup() |>
    tibble::as_tibble()
  cancerCohortTableName <- omopgenerics::uniqueTableName()
  cdm <- omopgenerics::insertTable(
    cdm = cdm,
    name = cancerCohortTableName,
    table = cancer_cohorts_subtype
  ) 
  cdm[[name]] <- omopgenerics::newCohortTable(
    cdm[[cancerCohortTableName]]
  ) |>
    dplyr::compute(
      name = name
    )
  return(cdm[[name]])
}
