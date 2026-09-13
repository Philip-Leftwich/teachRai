teachr_setup <- function(provider = NULL,
                         api_key = NULL,
                         open = interactive(),
                         renviron = path.expand("~/.Renviron")) {
  provider <- tolower(trimws(provider %||% teachr_prompt_for_provider()))
  provider_info <- teachr_resolve_provider(provider)

  if (isTRUE(open)) {
    message("Opening the ", provider_info$label, " API key page in your browser.")
    utils::browseURL(provider_info$signup_url)
  }

  api_key <- api_key %||% teachr_prompt_for_key(provider_info$label)

  if (!nzchar(trimws(api_key))) {
    stop("A ", provider_info$label, " API key is required.", call. = FALSE)
  }

  teachr_write_renviron_var(
    name = provider_info$env_var,
    value = trimws(api_key),
    path = renviron
  )
  teachr_write_renviron_var(
    name = "TEACHR_PROVIDER",
    value = provider,
    path = renviron
  )

  message("Saved ", provider_info$env_var, " in ", renviron, ".")
  message("Please restart R before using teachRai.")

  invisible(renviron)
}
