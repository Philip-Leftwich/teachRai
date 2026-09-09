teachr_system_prompt <- function(mode) {
  mode <- teachr_match_mode(mode)

  switch(
    mode,
    explain = paste(
      "You are a calm teaching assistant for R learners.",
      "Explain code in clear British English.",
      "Hard rule: if code selection is EMPTY, do not infer code intent, bugs, or runtime issues.",
      "Hard rule: if observed error state is NONE, do not mention any error, warning, or problem.",
      "When information is missing, say exactly what is missing and ask for the smallest useful next input.",
      "Prefer short paragraphs and plain language."
    ),
    hint = paste(
      "You are a calm teaching assistant for R learners.",
      "Give a helpful hint in British English without solving everything.",
      "Hard rule: if observed error state is NONE, do not infer or invent any error/problem.",
      "Hard rule: if code selection is EMPTY, give a process hint only (what to run/share next), not a code diagnosis.",
      "Use only supplied context. Never speculate beyond it.",
      "Nudge the student towards one concrete next step."
    ),
    debug = paste(
      "You are a calm teaching assistant for R learners.",
      "Help the student debug code in British English.",
      "Use only the observed error text when supplied.",
      "Hard rule: if observed error state is NONE, do not provide a diagnosis.",
      "If no observed error is supplied, state this clearly and request the next run/check to capture one.",
      "Do not invent error messages, warnings, or causes."
    )
  )
}

teachr_build_prompt <- function(mode, context) {
  mode <- teachr_match_mode(mode)

  selection <- context$selection %||% ""
  selection_state <- if (nzchar(selection)) "PRESENT" else "EMPTY"
  selection_text <- if (nzchar(selection)) selection else "EMPTY"

  packages <- context$loaded_packages %||% character()
  packages_text <- if (length(packages) == 0) "NONE" else paste(packages, collapse = ", ")

  recent_error <- context$recent_error %||% ""
  error_state <- if (nzchar(recent_error)) "PRESENT" else "NONE"
  recent_error_text <- if (nzchar(recent_error)) recent_error else "NONE"

  base_lines <- c(
    paste("Mode:", teachr_title_case(mode)),
    "",
    paste("Code selection state:", selection_state),
    "Current code selection:",
    selection_text,
    "",
    paste("Observed error state:", error_state),
    "Observed error text:",
    recent_error_text,
    "",
    "Loaded packages:",
    packages_text,
    "",
    "Response rules:",
    "1) If observed error state is NONE, do not mention any specific error/problem.",
    "2) If code selection state is EMPTY, do not infer what the code does.",
    "3) Ask for exactly one concrete next step."
  )

  teachr_compact_lines(base_lines)
}
