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

  # 1. Load the mapping rules
  rules <- readSubtypeRDS("mapping") 
  rule_cols <- setdiff(names(rules), "subtype")

  # 2. Process cohort
  cancer_cohorts_subtype <- cohort |>
    dplyr::collect() |>
    dplyr::rowwise() |>
    dplyr::mutate(
      subtype = {
        # Get current row values for the rule columns (pgr, esr1, erbb2)
        # We use current_vals to match against the rule columns
        current_vals <- c(pgr, esr1, erbb2) 
        
        # Find the rule where:
        # For every column in rule, (rule_val is NA and cohort_val is NA) OR (rule_val == cohort_val)
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

  # 3. Update CDM
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