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

test_that("teachr_load_exemplars errors clearly on a bad path", {
  expect_error(teachr_load_exemplars(path = "does/not/exist.yaml"), "Could not locate")
})

test_that("teachr_load_exemplars matches the package-loaded exemplar table", {
  loaded <- teachr_load_exemplars()
  expect_identical(loaded, teachRai:::teachr_exemplars)
  expect_true(nrow(loaded) >= 8)
  expect_identical(
    loaded$id[loaded$mode == "hint"][[1]],
    "hint-filter-missing-values"
  )
})

test_that("retrieval finds the new stringr exemplar for explain mode", {
  out <- teachr_find_exemplars(
    mode = "explain",
    selection = "penguins_raw |> filter(str_detect(species, \"Adelie\"))",
    packages = "stringr"
  )

  expect_true(nrow(out) >= 1)
  expect_identical(out$id[[1]], "explain-stringr-detect-clean")
})

test_that("retrieval finds the new dates exemplar for debug mode", {
  out <- teachr_find_exemplars(
    mode = "debug",
    selection = "field_visits |> arrange(visit_date)",
    packages = c("lubridate", "dplyr")
  )

  expect_true(nrow(out) >= 1)
  expect_identical(out$id[[1]], "debug-dates-string-sort")
})

test_that("retrieval uses the error pattern for the new lm formula-order exemplar", {
  out <- teachr_find_exemplars(
    mode = "debug",
    selection = "model <- lm(penguins_clean, body_mass_g ~ flipper_length_mm)",
    recent_error = "Error in as.data.frame.default(data) : cannot coerce class 'formula' to a data.frame",
    packages = character()
  )

  expect_true(nrow(out) >= 1)
  expect_identical(out$id[[1]], "debug-lm-formula-order")
  expect_identical(out$error_matches[[1]], 1L)
})

test_that("retrieval finds the new duplicates exemplar for debug mode", {
  out <- teachr_find_exemplars(
    mode = "debug",
    selection = "penguins_raw |> left_join(site_lookup, by = \"island\")",
    packages = c("dplyr", "janitor")
  )

  expect_true(nrow(out) >= 1)
  expect_identical(out$id[[1]], "debug-duplicates-join-multiplication")
})

test_that("retrieval prefers topical matches for hint mode", {
  out <- teachr_find_exemplars(
    mode = "hint",
    selection = paste(
      "penguins_clean_names |> group_by(species) |> summarise(mean_body_mass = mean(body_mass_g))",
      "# missing values make the mean come back as NA"
    ),
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

test_that("exact package matching helps rank topical plan matches", {
  out <- teachr_find_exemplars(
    mode = "plan",
    goal_text = "I want to join a lookup table and pivot the result longer.",
    recent_error = "",
    packages = c(" dplyr ", "TIDYR")
  )

  expect_true(nrow(out) >= 1)
  expect_identical(out$id[[1]], "plan-join-and-reshape")
  expect_identical(out$package_matches[[1]], 2L)
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

test_that("debug retrieval can use an observed error without code context", {
  out <- teachr_find_exemplars(
    mode = "debug",
    selection = "",
    recent_error = "Error in filter(): object 'Species' not found",
    packages = c("dplyr", "janitor")
  )

  expect_true(nrow(out) >= 1)
  expect_identical(out$id[[1]], "debug-column-not-found")
})

test_that("short terms do not match inside unrelated words", {
  out <- teachr_find_exemplars(
    mode = "hint",
    selection = "names(penguins_clean)",
    recent_error = "",
    packages = "dplyr"
  )

  expect_identical(nrow(out), 0L)
})

test_that("run mode includes retrieved exemplars in the built prompt", {
  local_mocked_bindings(
    teachr_chat = function(prompt, system_prompt, provider, model, api_key) {
      list(prompt = prompt, system_prompt = system_prompt)
    }
  )

  context <- list(
    selection = paste(
      "penguins_clean_names |> group_by(species) |> summarise(mean_body_mass = mean(body_mass_g))",
      "# missing values make the mean come back as NA"
    ),
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
    teachr_chat = function(prompt, system_prompt, provider, model, api_key) {
      list(prompt = prompt, system_prompt = system_prompt)
    }
  )

  out <- teachr_run_mode(
    mode = "plan",
    context = list(
      goal_text = "I want to compare average body mass by species and sex.",
      packages_loaded = "dplyr"
    ),
    api_key = "test-key",
    quiet = TRUE
  )

  expect_true(nrow(out$exemplars) >= 1)
  expect_identical(out$exemplars$id[[1]], "plan-grouped-comparison")
  expect_identical(out$context$goal_text, "I want to compare average body mass by species and sex.")
  expect_identical(out$context$packages_loaded, "dplyr")
  expect_match(out$prompt, "Teaching exemplars:")
})

test_that("plan run mode also accepts loaded_packages in context", {
  local_mocked_bindings(
    teachr_chat = function(prompt, system_prompt, provider, model, api_key) {
      list(prompt = prompt, system_prompt = system_prompt)
    }
  )

  out <- teachr_run_mode(
    mode = "plan",
    context = list(
      goal_text = "I want to join a lookup table and pivot the result longer.",
      loaded_packages = c("dplyr", "tidyr")
    ),
    api_key = "test-key",
    quiet = TRUE
  )

  expect_true(nrow(out$exemplars) >= 1)
  expect_identical(out$context$packages_loaded, c("dplyr", "tidyr"))
  expect_identical(out$exemplars$id[[1]], "plan-join-and-reshape")
})
