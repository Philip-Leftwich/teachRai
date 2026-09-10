teachr_explain <- function(context = NULL,
                           model = "gemini-3.7-flash",
                           api_key = Sys.getenv("GEMINI_API_KEY"),
                           quiet = FALSE) {
  teachr_run_mode(
    mode = "explain",
    context = context,
    model = model,
    api_key = api_key,
    quiet = quiet
  )
}

teachr_hint <- function(context = NULL,
                        model = "gemini-3.7-flash",
                        api_key = Sys.getenv("GEMINI_API_KEY"),
                        quiet = FALSE) {
  teachr_run_mode(
    mode = "hint",
    context = context,
    model = model,
    api_key = api_key,
    quiet = quiet
  )
}

teachr_debug <- function(context = NULL,
                         model = "gemini-3.7-flash",
                         api_key = Sys.getenv("GEMINI_API_KEY"),
                         quiet = FALSE) {
  teachr_run_mode(
    mode = "debug",
    context = context,
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
                            model = "gemini-3.7-flash",
                            api_key = Sys.getenv("GEMINI_API_KEY"),
                            quiet = FALSE) {
  mode <- teachr_match_mode(mode)

  if (identical(mode, "plan")) {
    plan_context <- context %||% list()
    plan_context$packages_loaded <- plan_context$packages_loaded %||% plan_context$loaded_packages
    goal_text <- goal_text %||% plan_context$goal_text
    data_columns <- data_columns %||% plan_context$data_columns
    object_names <- object_names %||% plan_context$object_names
    packages_loaded <- packages_loaded %||% plan_context$packages_loaded %||% character()

    exemplars <- teachr_find_exemplars(
      mode = mode,
      goal_text = goal_text %||% "",
      packages = packages_loaded
    )
    prompt <- teachr_build_prompt(
      mode = mode,
      goal_text = goal_text,
      data_columns = data_columns,
      object_names = object_names,
      packages_loaded = packages_loaded,
      exemplars = exemplars
    )
    captured_context <- list(
      goal_text = goal_text,
      data_columns = data_columns,
      object_names = object_names,
      packages_loaded = packages_loaded
    )
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
