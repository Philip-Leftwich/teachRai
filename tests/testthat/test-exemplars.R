test_that("exemplar library has the expected structure", {
  required_columns <- c(
    "id",
    "mode",
    "topic",
    "student_question",
    "likely_misconception",
    "student_code",
    "instructor_hint",
    "instructor_explanation",
    "tags",
    "source_path"
  )

  expect_true(all(required_columns %in% names(teachRai:::teachr_exemplars)))
  expect_true(nrow(teachRai:::teachr_exemplars) >= 4)
  expect_true(all(nzchar(teachRai:::teachr_exemplars$id)))
  expect_true(all(teachRai:::teachr_exemplars$mode %in% c("explain", "hint", "debug", "plan")))
})

test_that("retrieval prefers topical matches for hint mode", {
  out <- teachr_find_exemplars(
    mode = "hint",
    selection = "penguins_clean_names |> group_by(species) |> summarise(mean_body_mass = mean(body_mass_g))",
    recent_error = "",
    packages = "dplyr"
  )

  expect_true(nrow(out) >= 1)
  expect_identical(out$id[[1]], "hint-filter-missing-values")
  expect_gt(out$score[[1]], 0)
})

test_that("retrieval uses error patterns for debug mode", {
  out <- teachr_find_exemplars(
    mode = "debug",
    selection = "penguins_clean |> filter(Species == \"Adelie\")",
    recent_error = "Error in filter(): object 'Species' not found",
    packages = c("dplyr", "janitor")
  )

  expect_true(nrow(out) >= 1)
  expect_identical(out$id[[1]], "debug-column-not-found")
  expect_identical(out$error_matches[[1]], 1L)
})

test_that("retrieval can fall back to exact package matches", {
  out <- teachr_find_exemplars(
    mode = "explain",
    selection = "help please",
    recent_error = "",
    packages = "  GGPLOT2 "
  )

  expect_true(nrow(out) >= 1)
  expect_identical(out$id[[1]], "explain-ggplot-aesthetics")
  expect_identical(out$package_matches[[1]], 1L)
})

test_that("retrieval stays empty when only packages are supplied", {
  out <- teachr_find_exemplars(
    mode = "explain",
    selection = "",
    recent_error = "",
    packages = "dplyr"
  )

  expect_identical(nrow(out), 0L)
})

test_that("run mode includes retrieved exemplars in the built prompt", {
  local_mocked_bindings(
    teachr_chat = function(prompt, system_prompt, model, api_key) {
      list(prompt = prompt, system_prompt = system_prompt)
    }
  )

  context <- list(
    selection = "penguins_clean_names |> group_by(species) |> summarise(mean_body_mass = mean(body_mass_g))",
    recent_error = "",
    loaded_packages = "dplyr"
  )

  out <- teachr_run_mode(
    mode = "hint",
    context = context,
    api_key = "test-key",
    quiet = TRUE
  )

  expect_true(nrow(out$exemplars) >= 1)
  expect_match(out$prompt, "Teaching exemplars:")
  expect_match(out$prompt, "hint-filter-missing-values")
})

test_that("plan run mode retrieves exemplars from the goal text", {
  local_mocked_bindings(
    teachr_chat = function(prompt, system_prompt, model, api_key) {
      list(prompt = prompt, system_prompt = system_prompt)
    }
  )

  out <- teachr_run_mode(
    mode = "plan",
    goal_text = "I want to compare average body mass by species and sex.",
    packages_loaded = "dplyr",
    api_key = "test-key",
    quiet = TRUE
  )

  expect_true(nrow(out$exemplars) >= 1)
  expect_identical(out$exemplars$id[[1]], "plan-grouped-comparison")
  expect_identical(out$context$goal_text, "I want to compare average body mass by species and sex.")
  expect_identical(out$context$loaded_packages, "dplyr")
  expect_match(out$prompt, "Teaching exemplars:")
})
