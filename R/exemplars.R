teachr_exemplars <- data.frame(
  id = c(
    "explain-grouped-summary",
    "explain-ggplot-aesthetics",
    "hint-filter-missing-values",
    "hint-mutate-case-when",
    "debug-missing-na-rm",
    "debug-column-not-found",
    "plan-grouped-comparison",
    "plan-join-and-reshape"
  ),
  mode = c(
    "explain", "explain",
    "hint", "hint",
    "debug", "debug",
    "plan", "plan"
  ),
  topic = c(
    "grouped summaries with dplyr",
    "building a layered ggplot",
    "handling missing values before a summary",
    "creating a new grouped label with mutate",
    "summaries that return missing values",
    "column names after cleaning data",
    "comparing averages across groups",
    "joining tables before reshaping"
  ),
  student_question = c(
    "Why does group_by() change what summarise() returns?",
    "What does aes() do and why are my points not coloured by species?",
    "Why does my grouped mean come back as NA?",
    "How do I turn a long species label into a shorter category?",
    "Why does mean(body_mass_g) give me NA inside summarise()?",
    "Why does dplyr say the column is not found after I cleaned the names?",
    "How should I compare average body mass by species and sex?",
    "How do I join the lookup table and then pivot the result for plotting?"
  ),
  likely_misconception = c(
    "Thinking group_by() changes the data values rather than the level at which summaries are calculated.",
    "Thinking aesthetics set outside aes() will map values from the data automatically.",
    "Assuming summary functions ignore missing values by default.",
    "Thinking mutate() edits labels in place without creating or reassigning a column.",
    "Assuming the error is in group_by() rather than missing values inside mean().",
    "Assuming old column names still exist after janitor::clean_names() or rename().",
    "Jumping straight to a full script instead of planning groups, summaries, and any missing-value step.",
    "Treating joins and pivoting as one step instead of checking keys first and reshaping afterwards."
  ),
  student_code = c(
    "penguins_raw |>\n  group_by(species) |>\n  summarise(mean_flipper = mean(flipper_length_mm, na.rm = TRUE))",
    "penguins_raw |>\n  ggplot(aes(x = species, y = body_mass_g, colour = sex)) +\n  geom_point()",
    "penguins_clean_names |>\n  group_by(species) |>\n  summarise(mean_body_mass = mean(body_mass_g))",
    "penguins_clean |>\n  mutate(species = case_when(\n    species == \"Adelie Penguin (Pygoscelis adeliae)\" ~ \"Adelie\"\n  ))",
    "penguins_clean_names |>\n  group_by(species) |>\n  summarise(mean_body_mass = mean(body_mass_g))",
    "penguins_clean |>\n  filter(Species == \"Adelie\")",
    "penguins_raw |>\n  group_by(species, sex) |>\n  summarise(mean_body_mass = mean(body_mass_g, na.rm = TRUE), .groups = \"drop\")",
    "left_join(scores_tbl, lookup_tbl, by = \"student_id\") |>\n  pivot_longer(starts_with(\"week_\"), names_to = \"week\", values_to = \"score\")"
  ),
  instructor_hint = c(
    "Track what each pipe step returns: after group_by() the data is grouped, so summarise() now calculates one result per group.",
    "Check which parts belong inside aes() for data-driven mappings, then add the geom as a separate layer.",
    "Look at whether body_mass_g contains missing values and decide whether this summary needs na.rm = TRUE.",
    "Start by deciding whether you want a new column or to overwrite the old one, then write one case_when() condition at a time.",
    "Test mean(body_mass_g) on its own with and without na.rm = TRUE before changing the whole pipeline.",
    "Print names(penguins_clean) and compare them with the column name you typed inside filter().",
    "Plan this as three small steps: choose the grouping columns, decide the summary, then check how to handle missing values.",
    "Plan the key columns first, confirm the join result, and only then pivot the repeated measurement columns longer."
  ),
  instructor_explanation = c(
    "group_by() does not alter the values in the tibble. It stores grouping metadata so that summarise() works within each group and can return one row per group instead of one row for the whole dataset.",
    "ggplot() builds plots layer by layer. Variables that should vary with the data belong inside aes(), while fixed styling choices sit outside aes() as plain arguments to a geom.",
    "Most summary functions in R do not remove missing values unless you ask them to. When a group contains NA values, mean() returns NA unless you set na.rm = TRUE or remove those rows first.",
    "mutate() creates or replaces columns and returns a new tibble. case_when() is helpful when you want readable conditional recoding with one rule per line.",
    "This pattern usually comes from missing values rather than a broken grouped summary. The summary is working, but mean() propagates NA unless you remove missing values for that calculation.",
    "After clean_names(), columns usually become lower snake_case names. A later filter() call must use the current column names, not the earlier printed labels from the raw file.",
    "A good plan is to identify the comparison groups, choose the summary statistic, and make the missing-value decision explicit. That keeps the final code short and makes each step easier to explain.",
    "A tidy workflow usually joins related tables before reshaping repeated columns for plotting or summary work. Checking the join keys first helps you avoid accidental duplicated rows or unexpected missing values."
  ),
  tags = c(
    "dplyr, group_by, summarise, penguins, tidyverse",
    "ggplot2, aes, geom_point, visualisation, tidyverse",
    "dplyr, summarise, mean, NA, missing-values",
    "dplyr, mutate, case_when, recode, strings",
    "dplyr, summarise, mean, na.rm, debugging",
    "dplyr, filter, clean_names, rename, debugging",
    "dplyr, group_by, summarise, plan, missing-values",
    "dplyr, left_join, pivot_longer, tidyr, plan"
  ),
  source_path = c(
    "Oct-Intro-Analytics/05-dplyr.qmd",
    "Oct-Intro-Analytics/ggplot.qmd",
    "Oct-Intro-Analytics/08-missing-values.qmd",
    "Oct-Intro-Analytics/R/clean_penguins.R",
    "Oct-Intro-Analytics/08-missing-values.qmd",
    "Oct-Intro-Analytics/R/clean_penguins.R",
    "Oct-Intro-Analytics/summarise.qmd",
    "Oct-Intro-Analytics/join.qmd"
  ),
  match_terms = c(
    "group_by, summarise, summary, grouped, mean, average, species, flipper",
    "ggplot, aes, colour, color, points, geom_point, plot, species",
    "missing, na, mean, summarise, body_mass_g, species, drop_na, na.rm",
    "mutate, case_when, recode, rename, label, category",
    "mean, summarise, na, missing, na.rm, body_mass_g, average",
    "column not found, object not found, clean_names, rename, filter, species",
    "compare, average, mean, species, sex, grouped, summary, plan",
    "join, left_join, pivot_longer, reshape, plotting, lookup, key"
  ),
  packages = c(
    "dplyr",
    "ggplot2",
    "dplyr",
    "dplyr",
    "dplyr",
    "dplyr,janitor",
    "dplyr",
    "dplyr,tidyr"
  ),
  error_pattern = c(
    "", "",
    "", "",
    "missing values|NA",
    "column .* not found|object .* not found",
    "", ""
  ),
  stringsAsFactors = FALSE
)

teachr_find_exemplars <- function(mode,
                                  selection = "",
                                  recent_error = "",
                                  packages = character(),
                                  goal_text = "",
                                  n = 2) {
  mode <- teachr_match_mode(mode)
  pool <- teachr_exemplars[teachr_exemplars$mode == mode, , drop = FALSE]

  if (!nrow(pool)) {
    return(pool)
  }

  packages <- packages %||% character()
  query_text <- teachr_normalise_text(c(selection, recent_error, goal_text, packages))
  package_tokens <- unique(tolower(packages[nzchar(packages)]))
  error_text <- teachr_normalise_text(recent_error)

  if (!nzchar(query_text) && !nzchar(error_text) && !length(package_tokens)) {
    return(pool[0, , drop = FALSE])
  }

  term_matches <- integer(nrow(pool))
  package_matches <- integer(nrow(pool))
  error_matches <- integer(nrow(pool))

  for (i in seq_len(nrow(pool))) {
    terms <- teachr_split_csv(pool$match_terms[[i]])
    exemplar_packages <- teachr_split_csv(pool$packages[[i]])
    pattern <- pool$error_pattern[[i]]

    term_matches[[i]] <- sum(vapply(
      terms,
      function(term) nzchar(term) && grepl(term, query_text, fixed = TRUE),
      logical(1)
    ))

    package_matches[[i]] <- sum(exemplar_packages %in% package_tokens)

    error_matches[[i]] <- if (
      nzchar(pattern) &&
        nzchar(error_text) &&
        grepl(pattern, error_text, ignore.case = TRUE, perl = TRUE)
    ) {
      1L
    } else {
      0L
    }
  }

  pool$term_matches <- term_matches
  pool$package_matches <- package_matches
  pool$error_matches <- error_matches
  pool$score <- term_matches + package_matches + (error_matches * 3L)

  keep <- pool$term_matches > 0 | pool$error_matches > 0

  if (!any(keep)) {
    keep <- pool$package_matches > 0
  }

  if (!any(keep)) {
    return(pool[0, , drop = FALSE])
  }

  pool <- pool[keep, , drop = FALSE]
  ordering <- order(
    -pool$score,
    -pool$error_matches,
    -pool$package_matches,
    pool$id
  )
  pool <- pool[ordering, , drop = FALSE]
  utils::head(pool, n)
}

teachr_format_exemplars <- function(exemplars, mode) {
  mode <- teachr_match_mode(mode)

  if (is.null(exemplars) || !nrow(exemplars)) {
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
    entries
  )
}

teachr_format_exemplar_entry <- function(exemplar, mode) {
  lines <- c(
    paste0("Exemplar ID: ", exemplar$id[[1]]),
    paste0("Mode: ", teachr_title_case(exemplar$mode[[1]])),
    paste0("Topic: ", exemplar$topic[[1]]),
    paste0("Student question: ", exemplar$student_question[[1]]),
    paste0("Likely misconception: ", exemplar$likely_misconception[[1]]),
    "Student code:",
    exemplar$student_code[[1]],
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
    paste0("Provenance: ", exemplar$source_path[[1]])
  )

  teachr_compact_lines(lines)
}

teachr_split_csv <- function(x) {
  x <- trimws(x %||% "")

  if (!nzchar(x)) {
    return(character())
  }

  trimws(strsplit(x, ",", fixed = TRUE)[[1]])
}

teachr_normalise_text <- function(x) {
  x <- x %||% character()
  text <- tolower(paste(x, collapse = " "))
  text <- gsub("[^a-z0-9_ ]+", " ", text)
  text <- gsub("\\s+", " ", text)
  trimws(text)
}
