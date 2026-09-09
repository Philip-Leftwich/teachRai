teachr_system_prompt <- function(mode) {
  mode <- teachr_match_mode(mode)

  switch(
    mode,
    explain = paste(
      "You are a calm teaching assistant for R learners.",
      "Explain what the code is doing in clear British English.",
      "Prefer short paragraphs and plain language."
    ),
    hint = paste(
      "You are a calm teaching assistant for R learners.",
      "Give a helpful hint in British English without solving everything.",
      "Nudge the student towards the next step."
    ),
    debug = paste(
      "You are a calm teaching assistant for R learners.",
      "Help the student debug code in British English.",
      "Point out the likely problem, why it happens, and a sensible next check."
    )
  )
}

teachr_build_prompt <- function(mode, context) {
  mode <- teachr_match_mode(mode)
  selection <- teachr_value_or_default(context$selection, "No current code selection.")
  recent_error <- teachr_value_or_default(context$recent_error, "No recent console error.")
  packages <- context$loaded_packages %||% character()
  packages_text <- if (length(packages) == 0) {
    "No packages are currently attached."
  } else {
    paste(packages, collapse = ", ")
  }

  teachr_compact_lines(c(
    paste("Mode:", teachr_title_case(mode)),
    "",
    "Current code selection:",
    selection,
    "",
    "Recent console error:",
    recent_error,
    "",
    "Loaded packages:",
    packages_text
  ))
}
