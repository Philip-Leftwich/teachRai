teachr_help <- function(choice = NULL,
                        context = NULL,
                        goal_text = NULL,
                        model = "gemini-3.7-flash",
                        api_key = Sys.getenv("GEMINI_API_KEY"),
                        quiet = FALSE) {
  choice <- choice %||% teachr_menu_choice()
  mode <- teachr_resolve_mode(choice)
  teachr_run_mode(
    mode = mode,
    context = context,
    goal_text = goal_text,
    model = model,
    api_key = api_key,
    quiet = quiet
  )
}
