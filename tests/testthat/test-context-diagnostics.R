test_that("diagnostics context is empty when disabled", {
  withr::local_options(list(
    teachr.diagnostics.enabled = FALSE
  ))

  env <- new.env(parent = emptyenv())
  env$fit <- stats::lm(mpg ~ wt, data = mtcars)

  expect_identical(teachr_diagnostics_context(env = env), list())
})

test_that("diagnostics context is empty when no models are present", {
  withr::local_options(list(
    teachr.diagnostics.enabled = TRUE,
    teachr.diagnostics.max_models = 1
  ))

  env <- new.env(parent = emptyenv())
  env$x <- 1:5

  expect_identical(teachr_diagnostics_context(env = env), list())
})

test_that("diagnostics context falls back cleanly when performance support is off", {
  withr::local_options(list(
    teachr.diagnostics.enabled = TRUE,
    teachr.diagnostics.use_performance = FALSE,
    teachr.diagnostics.max_models = 1,
    teachr.diagnostics.max_plots = 1
  ))

  env <- new.env(parent = emptyenv())
  env$fit <- stats::lm(mpg ~ wt, data = mtcars)

  out <- teachr_diagnostics_context(env = env)

  expect_length(out, 1)
  expect_identical(out[[1]]$source, "base")
  expect_identical(out[[1]]$model_name, "fit")
  expect_identical(out[[1]]$summary$formula, "mpg ~ wt")
  expect_equal(length(out[[1]]$checks), 0)
  expect_equal(length(out[[1]]$artifacts), 0)
})

test_that("diagnostics context falls back cleanly without installed performance", {
  if (requireNamespace("performance", quietly = TRUE)) {
    skip("performance is installed in this test environment")
  }

  withr::local_options(list(
    teachr.diagnostics.enabled = TRUE,
    teachr.diagnostics.use_performance = TRUE,
    teachr.diagnostics.max_models = 1,
    teachr.diagnostics.max_plots = 1
  ))

  env <- new.env(parent = emptyenv())
  env$fit <- stats::lm(mpg ~ wt, data = mtcars)

  out <- teachr_diagnostics_context(env = env)

  expect_length(out, 1)
  expect_identical(out[[1]]$source, "base")
  expect_equal(length(out[[1]]$checks), 0)
  expect_equal(length(out[[1]]$artifacts), 0)
})

test_that("top-level capture and explanation payload include diagnostics context", {
  withr::local_options(list(
    teachr.diagnostics.enabled = FALSE
  ))

  captured <- teachr_capture_context()
  diagnostics_context <- list(list(
    model_name = "fit",
    model_class = "lm",
    source = "base",
    summary = list(
      formula = "mpg ~ wt",
      n_obs = 32,
      r_squared = 0.753,
      adj_r_squared = 0.745,
      sigma = 3.046
    ),
    checks = list(),
    artifacts = list()
  ))

  prompt <- teachr_build_prompt(
    mode = "explain",
    context = list(
      selection = "fit <- lm(mpg ~ wt, data = mtcars)",
      recent_error = "",
      loaded_packages = "stats",
      diagnostics_context = diagnostics_context
    )
  )

  expect_true("diagnostics_context" %in% names(captured))
  expect_identical(captured$diagnostics_context, list())
  expect_match(prompt, "Diagnostics context:")
  expect_match(prompt, "Formula: mpg ~ wt")
})

test_that("diagnostics context can use performance checks when available", {
  skip_if_not_installed("performance")

  withr::local_options(list(
    teachr.diagnostics.enabled = TRUE,
    teachr.diagnostics.use_performance = TRUE,
    teachr.diagnostics.max_models = 1,
    teachr.diagnostics.max_plots = 1
  ))

  env <- new.env(parent = emptyenv())
  env$fit <- stats::lm(mpg ~ wt, data = mtcars)

  out <- teachr_diagnostics_context(env = env)

  expect_length(out, 1)
  expect_identical(out[[1]]$source, "performance")
  expect_true(length(out[[1]]$checks) >= 1)
  expect_lte(length(out[[1]]$artifacts), 1)

  if (length(out[[1]]$artifacts) == 1) {
    expect_true(file.exists(out[[1]]$artifacts[[1]]$path))
  }
})
