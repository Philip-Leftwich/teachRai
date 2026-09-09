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
                            model = "gemini-3.7-flash",
                            api_key = Sys.getenv("GEMINI_API_KEY"),
                            quiet = FALSE) {
  context <- context %||% teachr_capture_context()
  prompt <- teachr_build_prompt(mode = mode, context = context)
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
    context = context,
    prompt = prompt,
    response = response
  ))
}
