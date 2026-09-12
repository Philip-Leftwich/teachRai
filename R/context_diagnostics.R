teachr_diagnostics_context <- function(env = .GlobalEnv) {
  # Diagnostics capture is opt-in and should never interrupt explanation flows.
  if (!isTRUE(getOption("teachr.diagnostics.enabled", FALSE))) {
    return(list())
  }

  max_models <- teachr_option_int("teachr.diagnostics.max_models", default = 1L)

  if (max_models < 1L) {
    return(list())
  }

  model_names <- teachr_supported_model_names(env = env)

  if (!length(model_names)) {
    return(list())
  }

  lapply(utils::head(model_names, max_models), function(model_name) {
    model <- get(model_name, envir = env, inherits = FALSE)
    teachr_collect_model_diagnostics(model = model, model_name = model_name)
  })
}

teachr_supported_model_names <- function(env = .GlobalEnv) {
  object_names <- ls(envir = env, all.names = FALSE)

  Filter(
    function(object_name) {
      object <- tryCatch(
        get(object_name, envir = env, inherits = FALSE),
        error = function(...) NULL
      )

      inherits(object, "lm")
    },
    object_names
  )
}

teachr_collect_model_diagnostics <- function(model, model_name) {
  diagnostics <- list(
    model_name = model_name,
    model_class = class(model),
    source = "base",
    summary = teachr_model_summary_metadata(model),
    checks = list(),
    artifacts = list()
  )

  if (!teachr_use_performance_diagnostics()) {
    return(diagnostics)
  }

  performance_context <- teachr_collect_performance_diagnostics(
    model = model,
    model_name = model_name
  )

  diagnostics$checks <- performance_context$checks
  diagnostics$artifacts <- performance_context$artifacts

  has_performance_signal <- any(vapply(
    diagnostics$checks,
    function(check) isTRUE(check$ok),
    logical(1)
  ))

  if (has_performance_signal || length(diagnostics$artifacts) > 0) {
    diagnostics$source <- "performance"
  }

  diagnostics
}

teachr_use_performance_diagnostics <- function() {
  isTRUE(getOption("teachr.diagnostics.use_performance", TRUE)) &&
    requireNamespace("performance", quietly = TRUE)
}

teachr_collect_performance_diagnostics <- function(model, model_name) {
  list(
    checks = teachr_collect_performance_checks(model),
    artifacts = teachr_collect_performance_artifacts(
      model = model,
      model_name = model_name
    )
  )
}

teachr_collect_performance_checks <- function(model) {
  check_fns <- list(
    normality = performance::check_normality,
    heteroscedasticity = performance::check_heteroscedasticity,
    collinearity = performance::check_collinearity
  )

  lapply(names(check_fns), function(check_name) {
    check_result <- tryCatch(
      suppressWarnings(
        suppressMessages(check_fns[[check_name]](model))
      ),
      error = function(error) error
    )

    if (inherits(check_result, "error")) {
      return(list(
        name = check_name,
        ok = FALSE,
        text = conditionMessage(check_result)
      ))
    }

    list(
      name = check_name,
      ok = TRUE,
      text = teachr_collapse_diagnostic_text(utils::capture.output(print(check_result)))
    )
  })
}

teachr_collect_performance_artifacts <- function(model, model_name) {
  max_plots <- teachr_option_int("teachr.diagnostics.max_plots", default = 1L)

  if (max_plots < 1L) {
    return(list())
  }

  if (!requireNamespace("see", quietly = TRUE)) {
    return(list())
  }

  check_model_result <- tryCatch(
    suppressWarnings(
      suppressMessages(performance::check_model(model))
    ),
    error = function(error) error
  )

  if (inherits(check_model_result, "error")) {
    return(list())
  }

  plot_path <- teachr_save_diagnostic_plot(
    plot_object = check_model_result,
    model_name = model_name
  )

  if (!length(plot_path)) {
    return(list())
  }

  c(list(list(
    name = "check_model",
    type = "diagnostic_plot"
  )), list(plot_path))[[1]]
}

teachr_save_diagnostic_plot <- function(plot_object, model_name) {
  plot_path <- tempfile(
    pattern = paste0("teachr-", model_name, "-diagnostics-"),
    fileext = ".png"
  )

  png_device <- NA_integer_
  plot_ok <- tryCatch(
    {
      grDevices::png(filename = plot_path, width = 800, height = 800, res = 96)
      png_device <- grDevices::dev.cur()
      suppressWarnings(
        suppressMessages(plot(plot_object))
      )
      TRUE
    },
    error = function(...) FALSE,
    finally = {
      open_devices <- grDevices::dev.list()

      if (!is.na(png_device) && !is.null(open_devices) && png_device %in% open_devices) {
        grDevices::dev.off(which = png_device)
      }
    }
  )

  if (!isTRUE(plot_ok) || !file.exists(plot_path)) {
    unlink(plot_path)
    return(list())
  }

  plot_info <- file.info(plot_path)

  if (is.na(plot_info$size) || plot_info$size < 1) {
    unlink(plot_path)
    return(list())
  }

  size_bytes <- unname(plot_info$size[[1]])
  unlink(plot_path)

  list(
    format = "png",
    captured = TRUE,
    size_bytes = size_bytes
  )
}

teachr_model_summary_metadata <- function(model) {
  model_summary <- tryCatch(summary(model), error = function(...) NULL)

  list(
    formula = teachr_safe_formula_text(model),
    n_obs = tryCatch(stats::nobs(model), error = function(...) NA_integer_),
    r_squared = teachr_safe_summary_value(model_summary, "r.squared"),
    adj_r_squared = teachr_safe_summary_value(model_summary, "adj.r.squared"),
    sigma = teachr_safe_summary_value(model_summary, "sigma")
  )
}

teachr_safe_formula_text <- function(model) {
  formula_text <- tryCatch(
    paste(deparse(stats::formula(model)), collapse = " "),
    error = function(...) ""
  )

  trimws(formula_text)
}

teachr_safe_summary_value <- function(model_summary, name) {
  if (is.null(model_summary) || is.null(model_summary[[name]])) {
    return(NA_real_)
  }

  unname(model_summary[[name]])
}

teachr_collapse_diagnostic_text <- function(lines, max_lines = 8L) {
  if (!length(lines)) {
    return("")
  }

  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]

  if (!length(lines)) {
    return("")
  }

  paste(utils::head(lines, max_lines), collapse = " ")
}

teachr_option_int <- function(name, default = 0L) {
  value <- getOption(name, default)

  if (!length(value)) {
    return(default)
  }

  value <- suppressWarnings(as.integer(value[[1]]))

  if (is.na(value)) {
    return(default)
  }

  value
}
