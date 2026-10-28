#' Apply CAR filtering for DK-DHR
#'
#' Utility function to use in oncology studies involving the Danish Data
#' Partner DK-DHR. The function applies a filter on the `condition_occurrence`
#' table of the CDM to select only records validated by the Danish Cancer
#' Registry (CAR).
#'
#' @param cdm A cdm instance.
#' @param dbname A character string of the database name (must be "DK-DHR").
#' @param objective A character string specifying the study objective, and
#' consequently the type of filtering to apply. Default `"other"`. Use the
#' default if no further specifications were given.
#'
#' @returns The cdm instance, with filtered condition occurrence records.
#'
#' @export
#'
applyCARFiltering <- function(
    cdm,
    dbname,
    objective = "other"
) {

  checkmate::assertClass(cdm, "cdm_reference")
  checkmate::assertChoice(dbname, c("DK-DHR"))
  checkmate::assertCharacter(objective)

  if (objective == "1.1") {

    n_records <- cdm[["condition_occurrence"]] |>
      dplyr::summarise(
        n = dplyr::n_distinct(.data$condition_occurrence_id)
      ) |>
      dplyr::pull(n)
    n_subjects <- cdm[["condition_occurrence"]] |>
      dplyr::summarise(
        n = dplyr::n_distinct(.data$person_id)
      ) |>
      dplyr::pull(n)

    cli::cli_alert(
      "Applying CAR filter for vocabulary_id = 'ICDO3' to:
       - 'condition_occurrence' table with '{n_records}' unique records and '{n_subjects}' unique subjects"
    )

    cdm[["condition_occurrence"]] <- cdm[["condition_occurrence"]] |>
      dplyr::semi_join(
        cdm[["concept"]] |>
          filter(vocabulary_id == "ICDO3") |>
          select(concept_id),
        by = c("condition_source_concept_id" = "concept_id")
      ) |>
      compute()

    n_records_after <- cdm[["condition_occurrence"]] |>
      dplyr::summarise(
        n = dplyr::n_distinct(.data$condition_occurrence_id)
      ) |>
      dplyr::pull(n)
    n_subjects_after <- cdm[["condition_occurrence"]] |>
      dplyr::summarise(
        n = dplyr::n_distinct(.data$person_id)
      ) |>
      dplyr::pull(n)

    cli::cli_alert(
      "After CAR filter:
       - 'condition_occurrence' table now shows '{n_records_after}' unique records and '{n_subjects_after}' unique subjects"
    )

  } else {

    n_records <- cdm[["condition_occurrence"]] |>
      dplyr::summarise(
        n = dplyr::n_distinct(.data$condition_occurrence_id)
      ) |>
      dplyr::pull(n)
    n_subjects <- cdm[["condition_occurrence"]] |>
      dplyr::summarise(
        n = dplyr::n_distinct(.data$person_id)
      ) |>
      dplyr::pull(n)

    cli::cli_alert(
      "Applying CAR filter for condition_status_concept_id to:
       - 'condition_occurrence' table with '{n_records}' unique records and '{n_subjects}' unique subjects"
    )

    cdm[["condition_occurrence"]] <- cdm[["condition_occurrence"]] |>
      dplyr::filter(
        .data$condition_status_concept_id == 32893
      ) |>
      dplyr::compute()

    n_records_after <- cdm[["condition_occurrence"]] |>
      dplyr::summarise(
        n = dplyr::n_distinct(.data$condition_occurrence_id)
      ) |>
      dplyr::pull(n)
    n_subjects_after <- cdm[["condition_occurrence"]] |>
      dplyr::summarise(
        n = dplyr::n_distinct(.data$person_id)
      ) |>
      dplyr::pull(n)

    cli::cli_alert(
      "After CAR filter:
       - 'condition_occurrence' table now shows '{n_records_after}' unique records and '{n_subjects_after}' unique subjects"
    )
  }

  if (n_records_after == 0) {
    cli::cli_abort(
      "No records after CAR filter. Please report issue.",
      class = "Empty condition_occurrence"
    )
  }

  return(cdm)

}
