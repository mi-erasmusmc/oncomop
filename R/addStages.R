#' addCancerStages() information to a cohort
#'
#' It uses a codelist to date intersect with a cancer cohort. 
#' Imposes a predefined or custom set of rules to identify
#' summary stages.
#'
#' @param cohort A cohort table from a cdm reference object.
#' @param cdm A cdm reference.
#' @param stageConcepts A concept-set list containing cancer
#' stage concepts.
#' @param ruleSet A set of rules in list format that
#' corresponds to each element of the sstageConcepts codelist.
#'
#' @importFrom omopgenerics assertList assertTable
#' @returns A cohort table containing the identified cancer stages.
#'
#' @export
addStages <- function(
  cohort,
  cdm,
  cancer,
  window = list(c(0,0)),
  edition = "eight",
  type = "base",
  order = "last"
) {
  
  # assert parameters --------------------------------------------
  cohort |>
    omopgenerics::assertTable()
  cdm |> 
    omopgenerics::validateCdmArgument()
  window |> 
    omopgenerics::assertList()
  edition |> 
    checkmate::assertChoice(
      c("unspecified", "seventh", "eight")
    )
  type |> 
    checkmate::assertChoice(
      c("base", "clinical", "pathological")
    )
  
  # read stages rules data ---------------------------------------
  tnm_files_data <- system.file(
    "tnm_files",
    package = "oncomop"
  ) |> 
    list.files(
      full.names = TRUE
    ) |>
    readStagesRDS()

  # Extract codelist for intersection ----------------------------
  tnm_codelist <- tnm_files_data$tnm_concepts |>
    createTNMCodelist(
      .edition = "7th",
      .type = "clinical"
    ) 
  
  # outcome_table <- CohortConstructor::copyCohorts(
  #   cohort,
  #   name = "outcome_table"
  # )
  outcome_table <- cohort |>
    PatientProfiles::addConceptIntersectDate(
      conceptSet = stageCodelist,
      indexDate = "cohort_start_date",
      censorDate = NULL,
      window = list(c(-90, 90)),
      targetDate = "event_start_date",
      order = "last",
      inObservation = TRUE,
      nameStyle = "{concept_name}_{window_name}",
      name = NULL
    )
  prefix <- "ajcc_uicc"
  strataColumnTable <- outcome_table |>
    dplyr::collect() |>
    dplyr::rowwise() |>
  dplyr::mutate(
    strata_column = {
      row <- dplyr::pick(dplyr::starts_with(prefix))
      vals <- as.Date(unlist(row, use.names = FALSE))
      if (all(is.na(vals))) {
        NA_character_
      } else {
        names(row)[which.max(replace(vals, is.na(vals), as.Date("1900-01-01")))]
      }
    }
  ) %>%
  dplyr::ungroup() |>
    dplyr::select(
      cohort_definition_id,
      subject_id,
      cohort_start_date,
      cohort_end_date,
      strata_column
    )
  strataColumnTable[["strata_column"]] <- as.character(
    strataColumnTable[["strata_column"]]
  )
  ParallelLogger::logInfo(
    glue::glue(
      "Labeling values with no cancer stage found"
    )
  )
  strataColumnTable <- strataColumnTable %>%
    dplyr::mutate(
      strata_column = dplyr::case_when(
        is.na(strata_column) ~ glue::glue("no_cancer_stage_found"),
        .default = strata_column
      )
    )
  # Rename column stata_column in outcome_table using the "type" parameter
  strataColumnTable <- strataColumnTable %>%
    dplyr::rename(
      stages = strata_column
    )

  cdm <- omopgenerics::insertTable(
    cdm,
    name = name,
    table = strataColumnTable,
    overwrite = TRUE,
    temporary = FALSE
  )

  ParallelLogger::logInfo("Converting cancer_strata table into a cohort table")
  cdm[[name]] <- omopgenerics::newCohortTable(
    table = cdm[[name]]
  )

  return(cdm[[name]])
}

filterStageConcepts <- function(
  codelist
) {
  codelist |>
    omopgenerics::assertList()
  filterParents <- names(codelist) |>
    stringr::str_detect(
      pattern = "\\b[TMN]\\d\\b"
    )
  codelist[filterParents]
}
