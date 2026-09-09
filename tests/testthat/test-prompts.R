test_that("system prompts are available for each mode", {
  expect_match(teachRai:::teachr_system_prompt("explain"), "British English")
  expect_match(teachRai:::teachr_system_prompt("hint"), "hint", ignore.case = TRUE)
  expect_match(teachRai:::teachr_system_prompt("debug"), "debug", ignore.case = TRUE)
})

test_that("build prompt includes context sections", {
  prompt <- teachRai:::teachr_build_prompt(
    mode = "debug",
    context = list(
      selection = "x <- mean(y)",
      recent_error = "Error: object 'y' not found",
      loaded_packages = c("stats", "dplyr")
    )
  )

  expect_match(prompt, "Mode: Debug")
  expect_match(prompt, "Current code selection:")
  expect_match(prompt, "x <- mean\\(y\\)")
  expect_match(prompt, "Error: object 'y' not found")
  expect_match(prompt, "stats, dplyr")
})

test_that("unsupported modes fail clearly", {
  expect_error(
    teachRai:::teachr_build_prompt("solve", list()),
    "must be one of"
  )
})
