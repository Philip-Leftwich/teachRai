teachr_capture_context <- function(mode = NULL) {
  if (identical(mode, "explain")) {
    # Explain mode never reads or reports errors, so it must not consume the
    # session's error buffer either - a later hint/debug call still needs it.
    recent_error <- ""
  } else {
    recent_error <- teachr_recent_error()
    teachr_clear_recent_error()
  }

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

  if (!nzchar(error) || identical(error, "Error :")) {
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

teachr_extract_goal_from_comment <- function(selection) {
  lines <- strsplit(selection %||% "", "\n", fixed = TRUE)[[1]]
  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]

  if (!length(lines) || !all(grepl("^#", lines))) {
    return("")
  }

  goal_lines <- trimws(sub("^#+\\s*", "", lines))
  goal_lines <- goal_lines[nzchar(goal_lines)]

  paste(goal_lines, collapse = " ")
}
