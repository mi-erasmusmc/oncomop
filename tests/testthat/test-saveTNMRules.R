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
})

test_that("read stages rds", {

  tnm_files <- c(
    "tnm_concepts",
    "tnm_stage_mapping",
    "tnm_stage_shortcut_mapping"
  )

  tnm_files <- system.file(
    "tnm_files",
    package = "oncomop"
  ) |> 
    list.files(
      full.names = TRUE
    ) |>
    readStagesRDS() |> 
    names() |> 
    expect_equal(
      c("tnm_concepts.rds", 
        "tnm_stage_mapping.rds",
        "tnm_stage_shortcut_mapping.rds"
      )
    )
})