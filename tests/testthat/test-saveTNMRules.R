test_that("saveTNMRules saves expected RDS files", {

  tnm_files <- c(
    "tnm_concepts",
    "tnm_mapping"
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

  readStagesRDS(
    "mapping"
  ) |> 
    pull(site) |>
    unique() |>
    sort() |> 
    expect_equal(
      c("bladder", "breast", "colorectal",
      "lung", "oesophagus", 
      "prostate", "skin")
    )
  
  readStagesRDS(
    "mapping"
  ) |> 
    pull(stage_grouping_scope) |>
    unique() |>
    sort() |> 
    expect_equal(
      c("base", "clinical", "pathological")
    )
  
  readStagesRDS(
    "mapping"
  ) |> 
    pull(edition) |>
    unique() |>
    sort() |> 
    expect_equal(
      c("7th", "8th", "9th")
    )
  
})
