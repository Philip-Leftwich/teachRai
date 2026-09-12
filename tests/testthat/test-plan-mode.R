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

test_that("plan prompt can include matching teaching exemplars", {
  exemplars <- teachr_find_exemplars(
    mode = "plan",
    goal_text = "I want to compare average body mass by species and sex.",
    packages = "dplyr"
  )

  out <- teachr_build_prompt(
    mode = "plan",
    goal_text = "I want to compare average body mass by species and sex.",
    packages_loaded = "dplyr",
    exemplars = exemplars
  )

  expect_match(out, "Teaching exemplars:")
  expect_match(out, "Instructor explanation:")
  expect_match(out, "plan-grouped-comparison")
})

test_that("plan run mode auto-derives goal_text from a highlighted comment", {
  local_mocked_bindings(
    teachr_current_selection = function() "# compare average body mass by species and sex",
    teachr_chat = function(prompt, system_prompt, provider, model, api_key) {
      list(prompt = prompt, system_prompt = system_prompt)
    }
  )

  out <- teachr_run_mode(mode = "plan", api_key = "test-key", quiet = TRUE)

  expect_identical(out$context$goal_text, "compare average body mass by species and sex")
  expect_match(out$prompt, "Student goal:")
  expect_match(out$prompt, "compare average body mass by species and sex")
})

test_that("plan run mode does not treat a highlighted code selection as a goal", {
  local_mocked_bindings(
    teachr_current_selection = function() "penguins |> group_by(species)"
  )

  expect_error(
    teachr_run_mode(mode = "plan", api_key = "test-key", quiet = TRUE),
    "`goal_text` must be provided and non-empty for `plan` mode\\."
  )
})

test_that("plan system prompt includes intent-first constraints", {
  out <- teachr_system_prompt("plan")

  expect_match(out, "plain-English goal")
  expect_match(out, "British English")
  expect_match(out, "tidyverse-first approaches")
  expect_match(out, "1-2 strategy hints")
  expect_match(out, "exactly one concrete missing detail")
})
