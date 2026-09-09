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
  context <- context %||% teachr_capture_context()
  prompt <- teachr_build_prompt(
    mode = mode,
    context = context,
    goal_text = goal_text,
    data_columns = data_columns,
    object_names = object_names,
    packages_loaded = packages_loaded
  )
  response <- teachr_chat(
    prompt = prompt,
    system_prompt = teachr_system_prompt(mode),
    model = model,
    api_key = api_key
  )
  response <- teachr_enforce_style(response, mode = mode)$text
  response <- teachr_enforce_plan_response(response, mode = mode)$text

  if (!isTRUE(quiet)) {
    teachr_print_response(response, mode = mode)
  }

  invisible(list(
    mode = mode,
    context = context,
    prompt = prompt,
    response = response
  ))
}
