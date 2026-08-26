test_that("saveTNMRules saves expected RDS files", {

  tnm_files <- c(
    "tnm_concepts",
    "tnm_stage_mapping",
    "tnm_stage_shortcut_mapping"
  )

  # Save rules to RDS files
  saveTNMRules()

  # Check files exist
  for (f in tnm_files) {
    expect_true(
      file.exists(
        system.file(
          "tnm_files",
          paste0(f, ".rds"),
          package = "oncomop"
        )
      )
    )
  }

  tnm_files <- system.file(
    "tnm_files",
    package = "oncomop"
  ) |>
    list.files(
      full.names = TRUE
    )

  tnm_files_data <- readStagesRDS(tnm_files)
  tnm_files_data$tnm_stage_mapping |> 
    pull(site) |>
    unique() |>
    sort() |> 
    expect_equal(
      c("bladder", "breast", "colorectal",
      "lung", "skin", "oesophagus", 
      "prostate")
    )
  
  tnm_files_data$tnm_stage_mapping |> 
    pull(stage_grouping_scope) |>
    unique() |>
    sort() |> 
    expect_equal(
      c("base", "clinical", "pathological")
    )
  
  tnm_files_data$tnm_stage_mapping |> 
    pull(edition) |>
    unique() |>
    sort() |> 
    expect_equal(
      c("7th", "8th", "9th")
    )
  
})
