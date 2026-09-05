#' Save TNM staging rules to RDS file
#'
#' @description
#' The function reads the `.csv` files containing the TNM rules to derive the summary stage.
#'
#' @details
#' The files that we refer to are:
#' * `tnm_concepts`: contains the concept ids of each TNM component differentiated by type
#'  and edition of the UICC classification system;
#' * `tnm_mapping`: contains the complete rules to map all combinations of TNM components to a
#'  summary stage I-IV, differentiated by edition of staging system and by cancer type.
#'
#' @param path Character directory where the original .csv files are stored.
#' @param results_path Character directory where the RDS files are to be saved.
#' 
#' @importFrom here here
#'
#' @returns `NULL`, called for its side effects.
saveTNMRules <- function(
    path =  here::here("extras"),
    results_path =  system.file(
      "tnm_files",
      package = "oncomop"
    )
    ) {

  tnm_files <- c(
    "tnm_concepts",
    "tnm_mapping"
  )

  if (!dir.exists(results_path)) {
    dir.create(results_path)
  }

  for (i in seq_along(tnm_files)) {
    data <- read.csv(
      file.path(path, paste0(tnm_files[i], ".csv"))
    )
    if (tnm_files[i] == "tnm_mapping") {
      data <- data |> 
        dplyr::mutate(
          site = tolower(.data$site)
        ) |> 
        dplyr::mutate(
          site = dplyr::case_when(
            .data$site == "urinary bladder" ~ "bladder",
            .data$site == "skin melanoma" ~ "skin",
            .default = .data$site
          ),
          stage_grouping_scope = dplyr::case_when(
            .data$stage_grouping_scope == "both" ~ "base",
            .default = .data$stage_grouping_scope
          ),
          edition = as.character(.data$edition)
        ) |> 
        dplyr::mutate(
          edition = dplyr::case_when(
            as.character(.data$edition) == "7" ~ as.character("7th"),
            as.character(.data$edition) == "8" ~ as.character("8th"),
            as.character(.data$edition) == "9" ~ as.character("9th"),
            .default = .data$edition
          )
        )
    }

    saveRDS(
      data,
      file.path(results_path, paste0(tnm_files[i], ".rds"))
    )
  }
}
