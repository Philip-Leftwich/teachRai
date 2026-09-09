test_that("explain prompt excludes error section and includes code/packages", {
  context <- list(
    selection = "x <- 1 + 1",
    recent_error = "Error: object 'y' not found",
    loaded_packages = c("dplyr", "ggplot2")
  )

  out <- teachr_build_prompt(mode = "explain", context = context)

  expect_match(out, "Mode: Explain")
  expect_match(out, "Current code selection:")
  expect_match(out, "x <- 1 \\+ 1")
  expect_match(out, "Loaded packages:")
  expect_match(out, "dplyr, ggplot2")

  # Critical: explain should not include error framing
  expect_no_match(out, "Recent console error:")
  expect_no_match(out, "Observed error")
  expect_no_match(out, "object 'y' not found")
})

test_that("hint prompt includes optional observed error section", {
  context <- list(
    selection = "mean(x)",
    recent_error = "Error in mean(x): object 'x' not found",
    loaded_packages = character()
  )

  out <- teachr_build_prompt(mode = "hint", context = context)

  expect_match(out, "Mode: Hint")
  expect_match(out, "Observed error state: present")
  expect_match(out, "Observed error \\(if any\\):")
  expect_match(out, "Error in mean\\(x\\): object 'x' not found")
  expect_match(out, "No packages are currently attached\\.")
})

test_that("hint prompt handles missing error gracefully", {
  context <- list(
    selection = "mean(c(1, 2, 3))",
    recent_error = NULL,
    loaded_packages = "stats"
  )

  out <- teachr_build_prompt(mode = "hint", context = context)

  expect_match(out, "Mode: Hint")
  expect_match(out, "Observed error state: absent")
  expect_match(out, "Observed error \\(if any\\):")
  expect_match(out, "No recent console error\\.")
})

test_that("debug prompt includes observed error guidance field", {
  context <- list(
    selection = "log('a')",
    recent_error = "Error in log(\"a\"): non-numeric argument",
    loaded_packages = "base"
  )

  out <- teachr_build_prompt(mode = "debug", context = context)

  expect_match(out, "Mode: Debug")
  expect_match(out, "Observed error state: present")
  expect_match(
    out,
    "Observed error \\(required for concrete diagnosis, if available\\):"
  )
  expect_match(out, "non-numeric argument")
})

test_that("system prompts encode mode-specific anti-hallucination behaviour", {
  explain_sys <- teachr_system_prompt("explain")
  hint_sys <- teachr_system_prompt("hint")
  debug_sys <- teachr_system_prompt("debug")

  expect_match(explain_sys, "Do not infer or invent runtime errors\\.")
  expect_match(explain_sys, "If no error is supplied, do not mention errors\\.")

  expect_match(
    hint_sys,
    "If an observed error is supplied, hint towards diagnosing/fixing it\\."
  )
  expect_match(
    hint_sys,
    "If no error is supplied, hint towards understanding or improving the code\\."
  )

  expect_match(debug_sys, "Use only the observed error text when one is supplied\\.")
  expect_match(debug_sys, "Do not invent error messages\\.")
})
