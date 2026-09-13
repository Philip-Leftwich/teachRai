teachr_exemplar_columns <- function() {
  c(
    "id", "mode", "topic", "student_question", "likely_misconception",
    "student_code", "instructor_hint", "instructor_explanation", "tags",
    "source_path", "match_terms", "packages", "error_pattern"
  )
}

teachr_load_exemplars <- function(path = system.file("extdata", "exemplars.yaml", package = "teachRai")) {
  if (!nzchar(path) || !file.exists(path)) {
    stop("Could not locate exemplars.yaml.", call. = FALSE)
  }

  records <- yaml::read_yaml(path)
  rows <- lapply(records, function(r) as.data.frame(r, stringsAsFactors = FALSE))
  df <- do.call(rbind, rows)

  df[teachr_exemplar_columns()]
}

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
  packages <- trimws(tolower(packages[nzchar(packages)]))
  selection_text <- teachr_normalise_text(selection)
  goal_text_text <- teachr_normalise_text(goal_text)
  context_text <- teachr_normalise_text(c(selection, goal_text))
  query_text <- context_text
  package_tokens <- unique(packages)
  error_text <- teachr_normalise_text(recent_error)

  if (
    !nzchar(selection_text) &&
      !nzchar(goal_text_text) &&
      !(identical(mode, "debug") && nzchar(error_text))
  ) {
    return(pool[0, , drop = FALSE])
  }

  term_matches <- integer(nrow(pool))
  package_matches <- integer(nrow(pool))
  error_matches <- integer(nrow(pool))

  for (i in seq_len(nrow(pool))) {
    terms <- teachr_split_csv(pool$match_terms[[i]])
    exemplar_packages <- tolower(teachr_split_csv(pool$packages[[i]]))
    pattern <- pool$error_pattern[[i]]

    term_matches[[i]] <- sum(vapply(
      terms,
      function(term) teachr_term_detect(term, query_text),
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

  keep <- pool$term_matches > 0

  if (identical(mode, "debug")) {
    keep <- keep | pool$error_matches > 0
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

teachr_split_csv <- function(x) {
  x <- x %||% character()
  x <- trimws(x)
  x <- x[nzchar(x)]

  if (!length(x)) {
    return(character())
  }

  values <- unlist(strsplit(x, ",", fixed = TRUE), use.names = FALSE)
  values <- trimws(values)
  values[nzchar(values)]
}

teachr_normalise_text <- function(x) {
  x <- x %||% character()
  text <- tolower(paste(x, collapse = " "))
  text <- gsub("[^a-z0-9_. ]+", " ", text)
  text <- gsub("\\s+", " ", text)
  trimws(text)
}

teachr_term_detect <- function(term, text) {
  term <- teachr_split_words(term)
  text <- teachr_split_words(text)

  if (!length(term) || !length(text) || length(term) > length(text)) {
    return(FALSE)
  }

  if (length(term) == 1) {
    return(term %in% text)
  }

  windows <- seq_len(length(text) - length(term) + 1L)

  any(vapply(
    windows,
    function(i) identical(text[i:(i + length(term) - 1L)], term),
    logical(1)
  ))
}

teachr_split_words <- function(x) {
  x <- teachr_normalise_text(x)

  if (!nzchar(x)) {
    return(character())
  }

  strsplit(x, " ", fixed = TRUE)[[1]]
}
