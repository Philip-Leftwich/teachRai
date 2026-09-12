teachr_explain <- function(context = NULL,
                           provider = NULL,
                           model = NULL,
                           api_key = NULL,
                           quiet = FALSE) {
  teachr_run_mode(
    mode = "explain",
    context = context,
    provider = provider,
    model = model,
    api_key = api_key,
    quiet = quiet
  )
}

teachr_hint <- function(context = NULL,
                        provider = NULL,
                        model = NULL,
                        api_key = NULL,
                        quiet = FALSE) {
  teachr_run_mode(
    mode = "hint",
    context = context,
    provider = provider,
    model = model,
    api_key = api_key,
    quiet = quiet
  )
}

teachr_debug <- function(context = NULL,
                         provider = NULL,
                         model = NULL,
                         api_key = NULL,
                         quiet = FALSE) {
  teachr_run_mode(
    mode = "debug",
    context = context,
    provider = provider,
    model = model,
    api_key = api_key,
    quiet = quiet
  )
}

teachr_run_mode <- function(mode,
                            context = NULL,
                            goal_text = NULL,
                            data_columns = NULL,
                            object_names = NULL,
                            packages_loaded = NULL,
                            provider = NULL,
                            model = NULL,
                            api_key = NULL,
                            quiet = FALSE) {
  mode <- teachr_match_mode(mode)

  if (identical(mode, "plan")) {
    context <- context %||% list()
    context$packages_loaded <- context$packages_loaded %||% context$loaded_packages
    context$goal_text <- goal_text %||% context$goal_text
    context$data_columns <- data_columns %||% context$data_columns
    context$object_names <- object_names %||% context$object_names
    context$packages_loaded <- packages_loaded %||% context$packages_loaded %||% character()

    exemplars <- teachr_find_exemplars(
      mode = mode,
      goal_text = context$goal_text %||% "",
      packages = context$packages_loaded
    )
    prompt <- teachr_build_prompt(
      mode = mode,
      goal_text = context$goal_text,
      data_columns = context$data_columns,
      object_names = context$object_names,
      packages_loaded = context$packages_loaded,
      exemplars = exemplars
    )
    captured_context <- context
  } else {
    context <- context %||% teachr_capture_context()
    exemplars <- teachr_find_exemplars(
      mode = mode,
      selection = context$selection %||% "",
      recent_error = context$recent_error %||% "",
      packages = context$loaded_packages %||% character()
    )
    prompt <- teachr_build_prompt(
      mode = mode,
      context = context,
      exemplars = exemplars
    )
    captured_context <- context
  }

  response <- teachr_chat(
    prompt = prompt,
    system_prompt = teachr_system_prompt(mode),
    provider = provider,
    model = model,
    api_key = api_key
  )

  if (!isTRUE(quiet)) {
    teachr_print_response(response, mode = mode)
  }

  invisible(list(
    mode = mode,
    context = captured_context,
    exemplars = exemplars,
    prompt = prompt,
    response = response
  ))
}
