test_that("saveStagesRules saves expected RDS files", {
  tnm_files <- c(
    "tnm_concepts",
    "tnm_mapping"
  )
  results_path <- tempfile("oncomop-stage-rules-")
  dir.create(results_path)
  # Save rules to RDS files
  saveStagesRules(results_path = results_path)
  # Check files exist
  for (f in tnm_files) {
    expect_true(
      file.exists(
        file.path(results_path, paste0(f, ".rds"))
      )
    )
  }
  readRDS(file.path(results_path, "tnm_mapping.rds")) |>
    pull(site) |>
    unique() |>
    sort() |> 
    expect_equal(
      c("bladder", "breast", "colorectal",
      "lung", "melanoma", "oesophageal", 
      "prostate")
    )
  readRDS(file.path(results_path, "tnm_mapping.rds")) |>
    pull(stage_grouping_scope) |>
    unique() |>
    sort() |> 
    expect_equal(
      c("base", "clinical", "pathological")
    )
  readRDS(file.path(results_path, "tnm_mapping.rds")) |>
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
  results_path <- tempfile("oncomop-subtype-rules-")
  dir.create(results_path)
  # Save rules to RDS files
  saveSubtypeRules(results_path = results_path)
  # Check files exist
  for (f in subtype_files) {
    expect_true(
      file.exists(
        file.path(results_path, paste0(f, ".rds"))
      )
    )
  }
  readRDS(file.path(results_path, "subtype_mapping.rds")) |>
    pull(subtype) |>
    expect_identical(
      c("ESR1/PGR positive", "ESR1/PGR positive", 
        "ESR1/PGR negative", "HER2 positive", "HER2 negative", 
        "Triple negative")
      )
})
