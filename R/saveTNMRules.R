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
    "tnm_stage_mapping",
    "tnm_stage_shortcut_mapping"
  )

  if (!dir.exists(results_path)) {
    dir.create(results_path)
  }

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

readStagesRDS <- function(tnm_files) {
  tnm_files |> 
    checkmate::assertFileExists() |> 
    basename() |> 
    identical(
      c( "tnm_concepts.rds",
         "tnm_stage_mapping.rds",
         "tnm_stage_shortcut_mapping.rds")) |> 
    checkmate::assertTRUE()

  setNames(
    lapply(tnm_files, readRDS),
    basename(tnm_files)
  )
}
