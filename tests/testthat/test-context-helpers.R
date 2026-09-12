test_that("teachr_error_state returns absent for missing or blank error", {
  expect_identical(teachr_error_state(list(recent_error = NULL)), "absent")
  expect_identical(teachr_error_state(list(recent_error = "")), "absent")
  expect_identical(teachr_error_state(list(recent_error = "   ")), "absent")
})

test_that("teachr_error_state returns uncertain when there is no selection to check against", {
  expect_identical(
    teachr_error_state(list(recent_error = "Error: object 'x' not found")),
    "uncertain"
  )
})

test_that("teachr_error_state returns present when the error shares a term with the selection", {
  expect_identical(
    teachr_error_state(list(
      recent_error = "Error in filter(): object 'Species' not found",
      selection = "penguins_clean |> filter(Species == \"Adelie\")"
    )),
    "present"
  )
})

test_that("teachr_error_state returns uncertain when the error looks unrelated to the selection", {
  expect_identical(
    teachr_error_state(list(
      recent_error = "Error in foo(): object 'bar' not found",
      selection = "mtcars |> summarise(mean(mpg))"
    )),
    "uncertain"
  )
})
