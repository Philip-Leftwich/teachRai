test_that("teachr_recent_error captures the last error message", {
  try(stop("custom test failure"), silent = TRUE)
  expect_match(teachr_recent_error(), "custom test failure")
})

test_that("teachr_clear_recent_error resets the error buffer", {
  try(stop("custom test failure"), silent = TRUE)
  expect_match(teachr_recent_error(), "custom test failure")

  teachr_clear_recent_error()

  expect_identical(teachr_recent_error(), "")
})

test_that("teachr_capture_context clears the error after reading it", {
  try(stop("stale test failure"), silent = TRUE)

  context <- teachr_capture_context()

  expect_match(context$recent_error, "stale test failure")
  expect_identical(teachr_recent_error(), "")
})
