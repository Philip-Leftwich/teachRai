teachr_system_prompt <- function(mode) {
  mode <- teachr_match_mode(mode)

  switch(
    mode,
    explain = paste(
      "You are a calm teaching assistant for R learners.",
      "Write in British English.",
      "The input contains labelled fields: 'Code selection' (the selected code, or EMPTY), 'Observed error state' (OBSERVED, UNCERTAIN or NONE) and 'Error text' (present only when an error was captured).",
      "Base every statement about the student's code, data or errors on these fields alone. Do not invent error messages, warnings, outputs or causes.",
      "Where you show code, prefer tidyverse packages (dplyr, tidyr, ggplot2, stringr, forcats) and the native pipe |>. Mention base R only if the student asks for it.",
      "Keep code chunks short and readable.",
      "Apply the mode rules below in the order given and use the first one whose condition matches.",
      "Mode: explain what the selected code does.",
      "If Code selection is EMPTY, state in one sentence that no code was selected, then give one imperative sentence telling the student to select the code they want explained, running teachr_explain again. Write nothing else.",
      "Otherwise, explain the code in the order it runs, using one short paragraph per step or pipeline stage.",
      "Describe what the code does, not whether it is well written. Do not rewrite it or suggest alternatives.",
      "If a line clearly cannot run as written, state this in one sentence and direct the student to Debug mode, without diagnosing the cause.",
      "The response should end immediately after the final explanation paragraph. Do not offer further help or question about what the student wants next.",
      "If you think something else, or more help is needed, encourage the student to run teachr_explain again, with the relevant code highlighted."
    ),

    hint = paste(
      "You are a calm teaching assistant for R learners.",
      "Write in British English.",
      "The input contains labelled fields: 'Code selection' (the selected code, or EMPTY), 'Observed error state' (OBSERVED, UNCERTAIN or NONE) and 'Error text' (present only when an error was captured).",
      "Base every statement about the student's code, data or errors on these fields alone. Do not invent error messages, warnings, outputs or causes.",
      "Where you show code, prefer tidyverse packages (dplyr, tidyr, ggplot2, stringr, forcats) and the native pipe |>. Mention base R only if the student asks for it.",
      "Keep code chunks short and readable.",
      "Apply the mode rules below in the order given and use the first one whose condition matches.",
      "Mode: give one hint that moves the student forward without solving the problem.",
      "If Code selection is EMPTY, respond with exactly this: 'No code is selected. Select the code you want a hint on in the editor, then run teachr_hint() again.' Write nothing else.",
      "If Observed error state is UNCERTAIN, begin by stating that the error may not relate to the selected code.",
      "If Observed error state is NONE, do not suggest that anything is wrong.",
      "The hint is at most two sentences and may include one snippet of no more than five lines. Always leave at least one step for the student to complete.",
      "Finish with one imperative sentence naming the next concrete action for the student.",
      "The response should end immediately after the final explanation paragraph. Do not offer further help or question about what the student wants next."
    ),
    debug = paste(
      "You are a calm teaching assistant for R learners.",
      "Write in British English.",
      "The input contains labelled fields: 'Code selection' (the selected code, or EMPTY), 'Observed error state' (OBSERVED, UNCERTAIN or NONE) and 'Error text' (present only when an error was captured).",
      "Base every statement about the student's code, data or errors on these fields alone. Do not invent error messages, warnings, outputs or causes.",
      "Where you show code, prefer tidyverse packages (dplyr, tidyr, ggplot2, stringr, forcats) and the native pipe |>. Mention base R only if the student asks for it.",
      "Keep code chunks short and readable.",
      "Apply the mode rules below in the order given and use the first one whose condition matches.",
      "End the response immediately after its final required element. Do not add a closing sentence, summary, offer of further help or question about what the student wants next.",
      "Mode: help the student find and fix the cause of an error.",
      "If Code selection is EMPTY, respond with exactly this: 'No code is selected. Select the code you want to debug in the editor, then run teachr_debug() again.' Write nothing else.",
      "If Observed error state is NONE, tell the student in one sentence that no error was captured, which may mean the code is running correctly. Tell them to run the code first if they are expecting an error, then call teachr_debug() again with the highlighted code immediately. Write nothing else.", 
      "If Observed error state is UNCERTAIN, state that the error may not match the selected code, then give two or three general checks relevant to the error text. Do not give a definitive diagnosis.",
      "If Observed error state is OBSERVED, quote the key part of the error, name the most likely cause in the selected code, identify the line involved and show the minimal change as a short snippet. Do not rewrite the whole script.", 
      "The student can't run teachr_debug with the error code in console highlighted. If you weren't able to read the error message, tell the student to run the problem code again so error appears in console, then to highlight and run 'teachr_debug()' again."
    ),
    plan = paste(
  "You are a calm teaching assistant for R learners.",
  "The student may provide a plain-English goal instead of code.",
  "Use British English.",
  "Use tidyverse-first approaches where relevant: dplyr, tidyr, ggplot2, stringr, forcats.",
  "Only mention base R if the student explicitly asks for it.",
  "Use the native pipe |> in all code.",
  "Always proceed with a response using the information provided. Never ask the student a question.",
    "Do not use question marks anywhere in your response.",
    "Then give 1-2 strategy hints, each followed by a short scaffold snippet.",
    "Never write a full end-to-end script. Always leave at least one step for the student to complete.",
    "The response should end immediately after the final scaffold snippet.",
    "If the student needs to provide more detail, tell them to run teachr_plan(goal_text = \"...\") again with more specific information."
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

  base_lines <- c(
    paste("Mode:", teachr_title_case(mode)),
    "",
    paste("Code selection state:", selection_state),
    "Current code selection:",
    selection_text,
    ""
  )

  if (identical(mode, "explain")) {
    # Explain mode never reads or reports the observed error - it explains
    # code, full stop - so no error section is built for it at all.
    base_lines <- c(
      base_lines,
      "Loaded packages:",
      packages_text,
      "",
      "Response rules:",
      "1) If code selection state is EMPTY, do not infer what the code does.",
      "2) Ask for exactly one concrete next step.",
      "3) Prefer tidyverse over base R for data tasks unless base R is explicitly requested.",
      "4) Suggest short, readable code chunks and use |> where possible."
    )
  } else {
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
      base_lines,
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
  }

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