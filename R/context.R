teachr_capture_context <- function() {
  recent_error <- teachr_recent_error()
  teachr_clear_recent_error()

  list(
    selection = teachr_current_selection(),
    recent_error = recent_error,
    loaded_packages = teachr_loaded_packages()
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

  if (!nzchar(error) || identical(error, "Error:")) {
    return("")
  }

  error
}

teachr_clear_recent_error <- function() {
  # geterrmessage() has no public "clear" API. Throwing a call-less, empty
  # error and swallowing it resets the buffer to the same "no error" shape
  # teachr_recent_error() already treats as absent.
  try(stop("", call. = FALSE), silent = TRUE)
}

teachr_loaded_packages <- function() {
  packages <- search() |>
    grep(pattern = "^package:", value = TRUE)

  sub("^package:", "", packages)
}
