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
  expect_match(out, "Response structure is mandatory")
  expect_match(out, "Do not provide complete solutions")
  expect_match(out, "Never provide an end-to-end pipeline")
  expect_match(out, "exactly one concrete missing detail")
})

test_that("plan guardrail replaces over-complete responses", {
  over_complete <- paste(
    "Here is the complete solution:",
    "df |> filter(a > 1) |> mutate(x = x + 1) |> group_by(g) |> summarise(n = n())",
    "df |> left_join(other, by = 'id') |> arrange(desc(n))",
    sep = "\n"
  )

  out <- teachRai:::teachr_enforce_plan_response(over_complete, mode = "plan")

  expect_false(out$ok)
  expect_match(out$text, "keep this as a plan")
  expect_match(out$reason, "too complete")
})

test_that("plan guardrail keeps concise planning responses", {
  concise <- paste(
    "You are trying to summarise score by treatment.",
    "The main gap is deciding whether to remove missing score values.",
    "Next step: start with score_tbl |> group_by(treatment) |> summarise(avg_score = mean(score, na.rm = TRUE)).",
    sep = "\n"
  )

  out <- teachRai:::teachr_enforce_plan_response(concise, mode = "plan")

  expect_true(out$ok)
  expect_identical(out$text, concise)
})

test_that("style enforcement in plan mode blocks base-R-heavy response", {
  base_r <- "Use aggregate(score ~ treatment, data = df, FUN = mean)"

  out <- teachRai:::teachr_enforce_style(base_r, mode = "plan")

  expect_false(out$ok)
  expect_match(out$text, "tidyverse-first approach")
})
