test_that("Extract stagesRules", {
  expect_no_error({
    ruleset <- stagesRules(
      edition = "7th",
      cancer = "lung",
      type = "base"
    )
  })
})

test_that("Extract stagesRules ovarian", {
  expect_no_error({
    ruleset <- stagesRules(
      edition = "8th",
      cancer = "ovarian",
      type = "base"
    )
  })
})

