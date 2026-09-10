test_that("non-plan prompts include context states and response rules", {
  context <- list(
    selection = "penguins |> summarise(mean_mass = mean(body_mass_g))",
    recent_error = "",
    loaded_packages = c("dplyr", "ggplot2")
  )

  out <- teachr_build_prompt(mode = "explain", context = context)

  expect_match(out, "Mode: Explain")
  expect_match(out, "Code selection state: PRESENT")
  expect_match(out, "Observed error state: NONE")
  expect_match(out, "Loaded packages:")
  expect_match(out, "dplyr, ggplot2")
  expect_match(out, "Response rules:")
  expect_match(out, "Prefer tidyverse over base R")
})

test_that("prompt leaves exemplar section out when nothing matches", {
  context <- list(
    selection = "x <- 1 + 1",
    recent_error = "",
    loaded_packages = character()
  )

  out <- teachr_build_prompt(
    mode = "explain",
    context = context,
    exemplars = teachr_find_exemplars(
      mode = "explain",
      selection = context$selection,
      recent_error = context$recent_error,
      packages = context$loaded_packages
    )
  )

  expect_no_match(out, "Teaching exemplars:")
})

test_that("hint exemplar formatting stays conservative", {
  exemplars <- teachr_find_exemplars(
    mode = "hint",
    selection = "penguins_clean_names |> summarise(mean_body_mass = mean(body_mass_g))",
    recent_error = "",
    packages = "dplyr"
  )

  out <- teachr_build_prompt(
    mode = "hint",
    context = list(
      selection = "penguins_clean_names |> summarise(mean_body_mass = mean(body_mass_g))",
      recent_error = "",
      loaded_packages = "dplyr"
    ),
    exemplars = exemplars
  )

  expect_match(out, "Teaching exemplars:")
  expect_match(out, "Use these exemplars only to align terminology and approach\\.")
  expect_match(out, "Instructor hint:")
  expect_no_match(out, "Instructor explanation:")
})

test_that("debug exemplar formatting includes explanation text", {
  exemplars <- teachr_find_exemplars(
    mode = "debug",
    selection = "penguins_clean |> filter(Species == \"Adelie\")",
    recent_error = "Error in filter(): object 'Species' not found",
    packages = c("dplyr", "janitor")
  )

  out <- teachr_build_prompt(
    mode = "debug",
    context = list(
      selection = "penguins_clean |> filter(Species == \"Adelie\")",
      recent_error = "Error in filter(): object 'Species' not found",
      loaded_packages = c("dplyr", "janitor")
    ),
    exemplars = exemplars
  )

  expect_match(out, "Teaching exemplars:")
  expect_match(out, "Instructor hint:")
  expect_match(out, "Instructor explanation:")
})

test_that("system prompts keep anti-hallucination rules", {
  explain_sys <- teachr_system_prompt("explain")
  hint_sys <- teachr_system_prompt("hint")
  debug_sys <- teachr_system_prompt("debug")

  expect_match(explain_sys, "Hard rule: if code selection is EMPTY")
  expect_match(explain_sys, "Hard rule: if observed error state is NONE")

  expect_match(hint_sys, "without solving everything")
  expect_match(hint_sys, "Never speculate beyond it")

  expect_match(debug_sys, "Use only the observed error text when supplied")
  expect_match(debug_sys, "Do not invent error messages, warnings, or causes")
})
