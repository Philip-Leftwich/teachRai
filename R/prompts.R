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
      "Hard rule: if observed error state is UNCERTAIN, say the error may not match the current selection and offer general debugging steps rather than a definitive diagnosis.",
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
  packages_loaded = NULL,
  exemplars = NULL
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

    exemplar_lines <- teachr_format_exemplars(exemplars, mode = mode)

    if (length(exemplar_lines) > 0) {
      lines <- c(lines, "", exemplar_lines)
    }

    return(teachr_compact_lines(lines))
  }

  selection <- context$selection %||% ""
  selection_state <- if (nzchar(selection)) "PRESENT" else "EMPTY"
  selection_text <- if (nzchar(selection)) selection else "EMPTY"

  packages <- context$loaded_packages %||% character()
  packages_text <- if (length(packages) == 0) "NONE" else paste(packages, collapse = ", ")

  recent_error <- context$recent_error %||% ""
  error_state_raw <- teachr_error_state(context)
  error_state <- switch(
    error_state_raw,
    absent = "NONE",
    present = "PRESENT",
    uncertain = "UNCERTAIN"
  )
  recent_error_text <- if (identical(error_state_raw, "absent")) "NONE" else recent_error

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
    "1a) If observed error state is UNCERTAIN, mention explicitly that the error text may be unrelated to the current selection before using it.",
    "2) If code selection state is EMPTY, do not infer what the code does.",
    "3) Ask for exactly one concrete next step.",
    "4) Prefer tidyverse over base R for data tasks unless base R is explicitly requested.",
    "5) Suggest short, readable code chunks and use |> where possible."
  )

  exemplar_lines <- teachr_format_exemplars(exemplars, mode = mode)

  if (length(exemplar_lines) > 0) {
    base_lines <- c(base_lines, "", exemplar_lines)
  }

  teachr_compact_lines(base_lines)
}

teachr_format_exemplars <- function(exemplars, mode) {
  mode <- teachr_match_mode(mode)

  if (!is.data.frame(exemplars) || !nrow(exemplars)) {
    return(character())
  }

  entries <- vapply(
    seq_len(nrow(exemplars)),
    function(i) {
      teachr_format_exemplar_entry(exemplars[i, , drop = FALSE], mode = mode)
    },
    character(1)
  )

  c(
    "Teaching exemplars:",
    "Use these exemplars only to align terminology and approach.",
    "Do not claim that the exemplar code or data belongs to the student.",
    unlist(strsplit(entries, "\n", fixed = TRUE), use.names = FALSE)
  )
}

teachr_format_exemplar_entry <- function(exemplar, mode) {
  lines <- c(
    paste0("Exemplar ID: ", exemplar$id[[1]]),
    paste0("Mode: ", teachr_title_case(exemplar$mode[[1]])),
    paste0("Topic: ", exemplar$topic[[1]]),
    paste0("Student question: ", exemplar$student_question[[1]]),
    paste0("Likely misconception: ", exemplar$likely_misconception[[1]]),
    paste0("Student code pattern: ", teachr_inline_text(exemplar$student_code[[1]])),
    paste0("Instructor hint: ", exemplar$instructor_hint[[1]])
  )

  if (mode != "hint") {
    lines <- c(
      lines,
      paste0("Instructor explanation: ", exemplar$instructor_explanation[[1]])
    )
  }

  lines <- c(
    lines,
    paste0("Tags: ", exemplar$tags[[1]]),
    paste0("Provenance: ", teachr_provenance_label(exemplar$source_path[[1]]))
  )

  teachr_compact_lines(lines)
}

teachr_inline_text <- function(x) {
  x <- gsub("\\s+", " ", x %||% "")
  trimws(x)
}

teachr_provenance_label <- function(source_path) {
  if (!nzchar(source_path %||% "")) {
    return("Teaching exemplar")
  }

  paste(trimws(strsplit(source_path, "/", fixed = TRUE)[[1]][1]), "teaching materials")
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
