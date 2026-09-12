teachr_capture_context <- function() {
  list(
    selection = teachr_current_selection(),
    recent_error = teachr_recent_error(),
    loaded_packages = teachr_loaded_packages(),
    diagnostics_context = teachr_diagnostics_context()
  )
}

teachr_current_selection <- function() {
  if (!requireNamespace("rstudioapi", quietly = TRUE)) {
    return("")
  }

  if (!rstudioapi::isAvailable()) {
    return("")
  }

  context <- rstudioapi::getActiveDocumentContext()
  selections <- vapply(context$selection, `[[`, character(1), "text")
  selections <- selections[nzchar(selections)]

  paste(selections, collapse = "\n")
}

teachr_recent_error <- function() {
  error <- trimws(geterrmessage())

  if (!nzchar(error) || identical(error, "Error: ")) {
    return("")
  }

  error
}

teachr_loaded_packages <- function() {
  packages <- search() |>
    grep(pattern = "^package:", value = TRUE)

  sub("^package:", "", packages)
}
