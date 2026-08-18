addStages <- function(
  cohortTable,
  cdm,
  stageConcepts,
  name
) {
  cohortTable |>
    omopgenerics::assertTable()
  stageConcepts |>
    omopgenerics::assertList()
  outcome_table <- CohortConstructor::copyCohorts(
    cohortTable,
    name = "outcome_table"
  )
  outcome_table <- cohortTable |>
    PatientProfiles::addConceptIntersectDate(
      conceptSet = stageConcepts,
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
