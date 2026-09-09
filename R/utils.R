`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

teachr_modes <- function() {
  c("explain", "hint", "debug")
}

teachr_match_mode <- function(mode) {
  mode <- tolower(mode)
  valid_modes <- c(teachr_modes(), "plan")

  if (!mode %in% valid_modes) {
    stop("`mode` must be one of: explain, hint, debug, or plan.", call. = FALSE)
  }

  mode
}

teachr_resolve_mode <- function(choice) {
  if (is.numeric(choice)) {
    modes <- teachr_modes()

    if (length(choice) != 1 || is.na(choice) || choice < 1 || choice > length(modes)) {
      stop("Menu choice must be 1, 2, or 3.", call. = FALSE)
    }

    return(modes[[choice]])
  }

  teachr_match_mode(choice)
}

teachr_menu_choice <- function() {
  if (!interactive()) {
    stop(
      "teachr_help() needs an explicit `choice` outside an interactive session.",
      call. = FALSE
    )
  }

  choice <- utils::menu(
    choices = c("Explain selection", "Give a hint", "Help debug"),
    title = "teachRai Assistant"
  )

  if (choice < 1) {
    stop("No menu choice was selected.", call. = FALSE)
  }

  choice
}

teachr_title_case <- function(x) {
  paste0(toupper(substr(x, 1, 1)), substring(x, 2))
}

teachr_value_or_default <- function(x, default) {
  if (is.null(x) || length(x) == 0) {
    return(default)
  }

  if (is.character(x) && !any(nzchar(x))) {
    return(default)
  }

  x
}

teachr_compact_lines <- function(lines) {
  lines <- lines[!is.na(lines)]
  text <- paste(lines, collapse = "\n")
  text <- gsub("\n{3,}", "\n\n", text)
  sub("\\n+$", "", text)
}

teachr_prompt_for_key <- function() {
  if (!interactive()) {
    stop(
      "Supply `api_key` when running teachr_setup() non-interactively.",
      call. = FALSE
    )
  }

  readline("Paste your Gemini API key: ")
}

teachr_write_renviron_var <- function(name, value, path = path.expand("~/.Renviron")) {
  if (file.exists(path)) {
    lines <- readLines(path, warn = FALSE)
  } else {
    lines <- character()
  }

  lines <- teachr_update_renviron_lines(lines, name = name, value = value)
  writeLines(lines, path)

  invisible(path)
}

teachr_update_renviron_lines <- function(lines, name, value) {
  entry <- paste0(name, "=", value)
  pattern <- paste0("^", name, "=")

  if (any(grepl(pattern, lines))) {
    lines[grepl(pattern, lines)] <- entry
    return(lines)
  }

  c(lines, entry)
}
