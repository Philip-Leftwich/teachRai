test_that("numeric addin choices resolve to modes", {
  expect_identical(teachRai:::teachr_resolve_mode(1), "explain")
  expect_identical(teachRai:::teachr_resolve_mode(2), "hint")
  expect_identical(teachRai:::teachr_resolve_mode(3), "debug")
})

test_that("renviron lines are updated in place", {
  lines <- c("OTHER_KEY=value", "GEMINI_API_KEY=old-key")

  updated <- teachRai:::teachr_update_renviron_lines(
    lines = lines,
    name = "GEMINI_API_KEY",
    value = "new-key"
  )

  expect_identical(
    updated,
    c("OTHER_KEY=value", "GEMINI_API_KEY=new-key")
  )
})

test_that("renviron lines append new keys", {
  updated <- teachRai:::teachr_update_renviron_lines(
    lines = "OTHER_KEY=value",
    name = "GEMINI_API_KEY",
    value = "new-key"
  )

  expect_identical(
    updated,
    c("OTHER_KEY=value", "GEMINI_API_KEY=new-key")
  )
})
