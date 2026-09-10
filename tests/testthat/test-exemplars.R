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
