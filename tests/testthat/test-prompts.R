test_that("non-plan prompt includes structured context fields", {
  context <- list(
    selection = "x <- 1 + 1",
    recent_error = "Error: object 'y' not found",
    loaded_packages = c("dplyr", "ggplot2")
  )

  out <- teachr_build_prompt(mode = "explain", context = context)

  expect_match(out, "Mode: Explain")
  expect_match(out, "Code selection state: PRESENT")
  expect_match(out, "Current code selection:")
  expect_match(out, "x <- 1 \\+ 1")
  expect_match(out, "Observed error state: PRESENT")
  expect_match(out, "Observed error text:")
  expect_match(out, "object 'y' not found")
  expect_match(out, "Loaded packages:")
  expect_match(out, "dplyr, ggplot2")
})

test_that("non-plan prompt handles empty selection and missing error", {
  context <- list(
    selection = "",
    recent_error = NULL,
    loaded_packages = character()
  )

  out <- teachr_build_prompt(mode = "hint", context = context)

  expect_match(out, "Mode: Hint")
  expect_match(out, "Code selection state: EMPTY")
  expect_match(out, "Observed error state: NONE")
  expect_match(out, "Observed error text:")
  expect_match(out, "NONE")
  expect_match(out, "Loaded packages:")
})

test_that("system prompts retain tidyverse and anti-hallucination constraints", {
  explain_sys <- teachr_system_prompt("explain")
  hint_sys <- teachr_system_prompt("hint")
  debug_sys <- teachr_system_prompt("debug")

  expect_match(explain_sys, "British English")
  expect_match(explain_sys, "tidyverse-first")
  expect_match(explain_sys, "if code selection is EMPTY")

  expect_match(hint_sys, "without solving everything")
  expect_match(hint_sys, "one concrete next step")

  expect_match(debug_sys, "Use only the observed error text")
  expect_match(debug_sys, "Do not invent error messages")
})
