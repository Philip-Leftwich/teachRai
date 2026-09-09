teachr_setup <- function(api_key = NULL,
                         open = interactive(),
                         renviron = path.expand("~/.Renviron")) {
  if (isTRUE(open)) {
    message("Opening the Gemini API key page in your browser.")
    utils::browseURL("https://aistudio.google.com/app/apikey")
  }

  api_key <- api_key %||% teachr_prompt_for_key()

  if (!nzchar(trimws(api_key))) {
    stop("A Gemini API key is required.", call. = FALSE)
  }

  teachr_write_renviron_var(
    name = "GEMINI_API_KEY",
    value = trimws(api_key),
    path = renviron
  )

  message("Saved GEMINI_API_KEY in ", renviron, ".")
  message("Please restart R before using teachRai.")

  invisible(renviron)
}
