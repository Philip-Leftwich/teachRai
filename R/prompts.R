teachr_system_prompt <- function(mode) {
  mode <- teachr_match_mode(mode)

  switch(
    mode,
    explain = paste(
      "You are a calm teaching assistant for R learners.",
      "Explain code in clear British English.",
      "Use tidyverse-first recommendations for data manipulation, transformation, and visualisation.",
      "Do not suggest base R alternatives for data tasks unless the user explicitly asks for base R.",
      "Prefer short, clean, and well-organised code chunks that are easy to read.",
      "When suggesting code, use the native pipe operator |>, where possible.",
      "Hard rule: if code selection is EMPTY, do not infer code intent, bugs, or runtime issues.",
      "Hard rule: if observed error state is NONE, do not mention any error, warning, or problem.",
      "When information is missing, say exactly what is missing and ask for the smallest useful next input.",
      "Prefer short paragraphs and plain language."
    ),
    hint = paste(
      "You are a calm teaching assistant for R learners.",
      "Give a helpful hint in British English without solving everything.",
      "Use tidyverse-first recommendations for data manipulation, transformation, and visualisation.",
      "Do not suggest base R alternatives for data tasks unless the user explicitly asks for base R.",
      "Prefer short, clean, and well-organised code chunks that are easy to read.",
      "When suggesting code, use the native pipe operator |>, where possible.",
      "Hard rule: if observed error state is NONE, do not infer or invent any error/problem.",
      "Hard rule: if code selection is EMPTY, give a process hint only (what to run/share next), not a code diagnosis.",
      "Use only supplied context. Never speculate beyond it.",
      "Nudge the student towards one concrete next step."
    ),
    debug = paste(
      "You are a calm teaching assistant for R learners.",
      "Help the student debug code in British English.",
      "Use tidyverse-first recommendations for data manipulation, transformation, and visualisation.",
      "Do not suggest base R alternatives for data tasks unless the user explicitly asks for base R.",
      "Prefer short, clean, and well-organised code chunks that are easy to read.",
      "When suggesting code, use the native pipe operator |>, where possible.",
      "Use only the observed error text when supplied.",
      "Hard rule: if observed error state is NONE, do not provide a diagnosis.",
      "If no observed error is supplied, state this clearly and request the next run/check to capture one.",
      "Do not invent error messages, warnings, or causes."
    ),
    plan = paste(
      "You are a calm teaching assistant for R learners.",
      "The student may provide a plain-English goal instead of code.",
      "Use British English.",
      "Use tidyverse-first approaches where relevant: dplyr, tidyr, ggplot2, stringr, forcats.",
      "Do not provide full end-to-end scripts.",
      "Provide 1-2 strategy hints and short scaffold snippets only.",
      "Use the native pipe operator |> in code examples.",
      "If one critical input is missing, ask for exactly one concrete missing detail.",
      "State assumptions explicitly and keep them minimal."
    )
  )
}

teachr_build_prompt <- function(
  mode,
  context = NULL,
  goal_text = NULL,
  data_columns = NULL,
  object_names = NULL,
  packages_loaded = NULL
) {
  mode <- teachr_match_mode(mode)

  if (identical(mode, "plan")) {
    if (is.null(goal_text) || !nzchar(trimws(goal_text))) {
      stop("`goal_text` must be provided and non-empty for `plan` mode.", call. = FALSE)
    }

    lines <- c(
      "Student goal:",
      goal_text
    )

    if (!is.null(data_columns) && length(data_columns) > 0) {
      lines <- c(lines, "", "Known data columns:", paste(data_columns, collapse = ", "))
    }

    if (!is.null(object_names) && length(object_names) > 0) {
      lines <- c(lines, "", "Known object names:", paste(object_names, collapse = ", "))
    }

    if (!is.null(packages_loaded) && length(packages_loaded) > 0) {
      lines <- c(lines, "", "Loaded packages:", paste(packages_loaded, collapse = ", "))
    }

    return(teachr_compact_lines(lines))
  }

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
    "3) Ask for exactly one concrete next step.",
    "4) Prefer tidyverse over base R for data tasks unless base R is explicitly requested.",
    "5) Suggest short, readable code chunks and use |> where possible."
  )

  teachr_compact_lines(base_lines)
}

teachr_check_style <- function(text) {
  text <- paste(text %||% "", collapse = "\n")

  # Simple heuristics for base-R-style data manipulation suggestions.
  # Keep this lightweight: flag patterns, then let caller decide enforcement.
  base_r_patterns <- c(
    "\\bapply\\s*\\(",
    "\\blapply\\s*\\(",
    "\\bsapply\\s*\\(",
    "\\btapply\\s*\\(",
    "\\baggregate\\s*\\(",
    "\\btransform\\s*\\(",
    "\\bwithin\\s*\\(",
    "\\bby\\s*\\(",
    "\\bmerge\\s*\\(",
    "\\bsubset\\s*\\(",
    "\\border\\s*\\(",
    "\\bwith\\s*\\(",
    "\\b\\w+\\s*\\[\\s*\\w+\\s*[!<>=]"
  )

  has_base_r <- any(vapply(
    base_r_patterns,
    function(p) grepl(p, text, perl = TRUE, ignore.case = TRUE),
    logical(1)
  ))

  has_pipe <- grepl("\\|>", text, perl = TRUE)

  list(
    ok = !has_base_r,
    has_base_r_patterns = has_base_r,
    has_native_pipe = has_pipe
  )
}

teachr_enforce_style <- function(text) {
  check <- teachr_check_style(text)

  if (isTRUE(check$ok)) {
    return(list(
      ok = TRUE,
      text = text,
      reason = "Style checks passed."
    ))
  }

  replacement <- paste(
    "I can refine that into a tidyverse-first approach.",
    "Please share the smallest reproducible code chunk, and I will return a short solution using dplyr/tidyr with the |> pipe."
  )

  list(
    ok = FALSE,
    text = replacement,
    reason = "Response contained base-R-style data manipulation patterns."
  )
}
