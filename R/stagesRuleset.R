stagesRuleset <- function(
  edition,
  cancer,
  type
) {
 readStagesRDS("mapping") |>
    extractStageRuleset(
      .cancer = cancer,
      .edition = edition,
      .type = "base"
    )   
}

readStagesRDS <- function(
    type = "mapping"
) {
  checkmate::assertChoice(
    type,
    c("concepts", "mapping")
  )
  system.file(
    "tnm_files",
    package = "oncomop"
  ) |>
    list.files(
      full.names = TRUE,
      pattern = type
    ) |> 
    readRDS()
}

extractStageRuleset <- function(
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