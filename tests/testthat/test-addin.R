test_that("teachr_help routes plan mode through teachr_run_mode", {
  captured <- NULL

  testthat::local_mocked_bindings(
    teachr_run_mode = function(mode,
                               context = NULL,
                               goal_text = NULL,
                               data_columns = NULL,
                               object_names = NULL,
                               packages_loaded = NULL,
                               model = "gemini-3.7-flash",
                               api_key = Sys.getenv("GEMINI_API_KEY"),
                               quiet = FALSE) {
      captured <<- list(
        mode = mode,
        context = context,
        goal_text = goal_text,
        data_columns = data_columns,
        object_names = object_names,
        packages_loaded = packages_loaded,
        model = model,
        api_key = api_key,
        quiet = quiet
      )
      invisible(captured)
    },
    .package = "teachRai"
  )

  teachRai::teachr_help(
    choice = "plan",
    context = list(selection = "", recent_error = "", loaded_packages = "dplyr"),
    goal_text = "Compare mean score by treatment group.",
    data_columns = c("treatment", "score"),
    object_names = "scores_tbl",
    packages_loaded = c("dplyr", "ggplot2"),
    model = "gemini-3.7-flash",
    api_key = "test-key",
    quiet = TRUE
  )

  expect_identical(captured$mode, "plan")
  expect_identical(captured$goal_text, "Compare mean score by treatment group.")
  expect_identical(captured$data_columns, c("treatment", "score"))
  expect_identical(captured$object_names, "scores_tbl")
  expect_identical(captured$packages_loaded, c("dplyr", "ggplot2"))
})
