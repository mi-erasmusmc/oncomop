#' Save TNM staging rules to RDS file
#'
#' @description
#' The function reads the `.csv` files containing the TNM rules to derive the summary stage.
#'
#' @details
#' The files that we refer to are:
#' * `tnm_concepts`: contains the concept ids of each TNM component differentiated by type
#'  and edition of the UICC classification system;
#' * `tnm_stage_mapping`: contains the complete rules to map all combinations of TNM components to a
#'  summary stage I-IV, differentiated by edition of staging system and by cancer type;
#' * `tnm_stage_shortcut_mapping`: contains more general rules that can be applied to derive the
#' summary stage in a faster way based on a subset of TNM components.
#'
#' @param path Character directory where the original .csv files are stored.
#' @param results_path Character directory where the RDS files are to be saved.
#'
#' @returns `NULL`, called for its side effects.
saveTNMRules <- function(
    path =  here::here("extras"),
    results_path =  here::here("inst")
    ) {

  tnm_files <- c(
    "tnm_concepts",
    "tnm_stage_mapping",
    "tnm_stage_shortcut_mapping"
  )

  for (f in tnm_files) {

    data <- read.csv(
      file.path(path, paste0(f, ".csv"))
    )

    saveRDS(
      data,
      file.path(results_path, paste0(f, ".rds"))
    )
  }
}
