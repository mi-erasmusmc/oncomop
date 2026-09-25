test_that("saveStagesRules saves expected RDS files", {
  tnm_files <- c(
    "tnm_concepts",
    "tnm_mapping"
  )
  # Save rules to RDS files
  saveStagesRules()
  # Check files exist
  for (f in tnm_files) {
    expect_true(
      file.exists(
        system.file(
          "data",
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

test_that("saveSubtypeRules saves expected RDS files", {
  subtype_files <- c(
    "subtype_mapping"
  )
  # Save rules to RDS files
  saveSubtypeRules()
  # Check files exist
  for (f in subtype_files) {
    expect_true(
      file.exists(
        system.file(
          "data",
          paste0(f, ".rds"),
          package = "oncomop"
        )
      )
    )
  }
  readSubtypeRDS(
    "mapping"
  ) |> 
    pull(subtype) |>
    expect_identical(
      c("Estrogen/Progesteron Positive", "Estrogen/Progesteron Positive", 
        "Estrogen/Progesteron Negative", "HER2 positive", "HER2 negative", 
        "Triple negative")
      )
})
