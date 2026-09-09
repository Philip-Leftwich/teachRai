test_that("mode matcher accepts plan", {
  expect_identical(teachRai:::teachr_match_mode("plan"), "plan")
  expect_identical(teachRai:::teachr_match_mode("PLAN"), "plan")
})

test_that("plan prompt requires non-empty goal_text", {
  expect_error(
    teachr_build_prompt(mode = "plan"),
    "`goal_text` must be provided and non-empty for `plan` mode\\."
  )

  expect_error(
    teachr_build_prompt(mode = "plan", goal_text = "   "),
    "`goal_text` must be provided and non-empty for `plan` mode\\."
  )
})

test_that("plan prompt includes goal and optional context fields", {
  out <- teachr_build_prompt(
    mode = "plan",
    goal_text = "I want to compare average scores by group.",
    data_columns = c("group", "score"),
    object_names = c("results_tbl", "scores_raw"),
    packages_loaded = c("dplyr", "ggplot2")
  )

  expect_match(out, "Student goal:")
  expect_match(out, "I want to compare average scores by group\\.")
  expect_match(out, "Known data columns:")
  expect_match(out, "group, score")
  expect_match(out, "Known object names:")
  expect_match(out, "results_tbl, scores_raw")
  expect_match(out, "Loaded packages:")
  expect_match(out, "dplyr, ggplot2")
})

test_that("plan system prompt includes intent-first constraints", {
  out <- teachr_system_prompt("plan")

  expect_match(out, "plain-English goal")
  expect_match(out, "British English")
  expect_match(out, "tidyverse-first approaches")
  expect_match(out, "1-2 strategy hints")
  expect_match(out, "exactly one concrete missing detail")
})
