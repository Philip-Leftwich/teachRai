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

test_that("teachr_capture_context never reads or clears the error for explain mode", {
  try(stop("explain should not see this"), silent = TRUE)

  context <- teachr_capture_context("explain")

  expect_identical(context$recent_error, "")
  expect_match(teachr_recent_error(), "explain should not see this")

  teachr_clear_recent_error()
})

test_that("teachr_extract_goal_from_comment extracts a goal from comment-only selections", {
  expect_identical(
    teachr_extract_goal_from_comment("# compare average mass by species and sex"),
    "compare average mass by species and sex"
  )

  expect_identical(
    teachr_extract_goal_from_comment("## Step 1: join tables\n# then pivot longer"),
    "Step 1: join tables then pivot longer"
  )
})

test_that("teachr_extract_goal_from_comment rejects selections that aren't comment-only", {
  expect_identical(teachr_extract_goal_from_comment(""), "")
  expect_identical(teachr_extract_goal_from_comment(NULL), "")
  expect_identical(
    teachr_extract_goal_from_comment("penguins |> group_by(species)"),
    ""
  )
  expect_identical(
    teachr_extract_goal_from_comment("x <- 1 # do the thing"),
    ""
  )
})
