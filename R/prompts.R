teachr_system_prompt <- function(mode) {
  mode <- teachr_match_mode(mode)

  switch(
    mode,
    explain = paste(
      "You are a calm teaching assistant for R learners.",
      "Explain what the code is doing in clear British English.",
      "Do not infer or invent runtime errors.",
      "If no error is supplied, do not mention errors.",
      "Prefer short paragraphs and plain language."
    ),
    hint = paste(
      "You are a calm teaching assistant for R learners.",
      "Give a helpful hint in British English without solving everything.",
      "If an observed error is supplied, hint towards diagnosing/fixing it.",
      "If no error is supplied, hint towards understanding or improving the code.",
      "Nudge the student towards the next step."
    ),
    debug = paste(
      "You are a calm teaching assistant for R learners.",
      "Help the student debug code in British English.",
      "Use only the observed error text when one is supplied.",
      "If no observed error is supplied, state that clearly and ask for or suggest the next run/check to obtain one.",
      "Do not invent error messages."
    )
  )
}

teachr_build_prompt <- function(mode, context) {
  mode <- teachr_match_mode(mode)

  selection <- teachr_value_or_default(
    context$selection,
    "No current code selection."
  )

  packages <- context$loaded_packages %||% character()
  packages_text <- if (length(packages) == 0) {
    "No packages are currently attached."
  } else {
    paste(packages, collapse = ", ")
  }

  recent_error <- teachr_value_or_default(
    context$recent_error,
    "No recent console error."
  )

  error_state <- teachr_error_state(context)

  if (identical(mode, "explain")) {
    return(teachr_compact_lines(c(
      paste("Mode:", teachr_title_case(mode)),
      "",
      "Current code selection:",
      selection,
      "",
      "Loaded packages:",
      packages_text
    )))
  }

  if (identical(mode, "hint")) {
    return(teachr_compact_lines(c(
      paste("Mode:", teachr_title_case(mode)),
      "",
      "Current code selection:",
      selection,
      "",
      paste("Observed error state:", error_state),
      "Observed error (if any):",
      recent_error,
      "",
      "Loaded packages:",
      packages_text
    )))
  }

  # debug
  teachr_compact_lines(c(
    paste("Mode:", teachr_title_case(mode)),
    "",
    "Current code selection:",
    selection,
    "",
    paste("Observed error state:", error_state),
    "Observed error (required for concrete diagnosis, if available):",
    recent_error,
    "",
    "Loaded packages:",
    packages_text
  ))
}
