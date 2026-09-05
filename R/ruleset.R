ruleset <- function(x) {
    
}

readStagesRDS <- function() {

  tnm_files <- system.file(
        "tnm_files",
        package = "oncomop"
    ) |>
    list.files(
      full.names = TRUE
    ) 

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
    tools::file_path_sans_ext(
        basename(
            tnm_files
        )
    )
  )
}

cextractStageRuleset <- function(
  tnm_stage_mapping,
  .edition,
  .cancer,
  .type
) {
  checkmate::assertDataFrame(tnm_stage_mapping)
  tnm_stage_mapping |>
    dplyr::filter(
      edition == .edition
    ) |>
    dplyr::filter(
      site == .cancer
    ) |>
    dplyr::filter(
      stage_grouping_scope == .type
    ) |>
    dplyr::select(
      rule_id, T, N, M, uicc_stage
    )
}