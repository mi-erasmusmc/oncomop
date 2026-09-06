test_that("Extract stagesRules", {
  expect_no_error({
    ruleset <- stagesRules(
      edition = "7th",
      cancer = "lung",
      type = "base"
    )
  })
})