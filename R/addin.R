teachr_help <- function(choice = NULL,
                        context = NULL,
                        provider = NULL,
                        model = NULL,
                        api_key = NULL,
                        quiet = FALSE) {
  choice <- choice %||% teachr_menu_choice()
  mode <- teachr_resolve_mode(choice)
  teachr_run_mode(
    mode = mode,
    context = context,
    provider = provider,
    model = model,
    api_key = api_key,
    quiet = quiet
  )
}
