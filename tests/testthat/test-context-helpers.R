test_that("teachr_error_state returns absent for missing or blank error", {
  expect_identical(teachr_error_state(list(recent_error = NULL)), "absent")
  expect_identical(teachr_error_state(list(recent_error = "")), "absent")
  expect_identical(teachr_error_state(list(recent_error = "   ")), "absent")
})

test_that("teachr_error_state returns present for non-empty error text", {
  expect_identical(
    teachr_error_state(list(recent_error = "Error: object 'x' not found")),
    "present"
  )
})
