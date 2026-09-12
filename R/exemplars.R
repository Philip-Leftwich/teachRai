teachr_required_exemplar_columns <- c(
  "id",
  "mode",
  "topic",
  "student_question",
  "likely_misconception",
  "student_code",
  "instructor_hint",
  "instructor_explanation",
  "tags",
  "source_path",
  "match_terms",
  "packages",
  "error_pattern"
)

teachr_allowed_exemplar_modes <- c("explain", "hint", "debug", "plan")

teachr_exemplar_yaml_path <- function() {
  path <- system.file("extdata", "exemplars.yml", package = "teachRai")

  if (nzchar(path) && file.exists(path)) {
    return(path)
  }

  override_path <- Sys.getenv("TEACHRAI_EXEMPLAR_PATH", unset = "")

  if (nzchar(override_path) && file.exists(override_path)) {
    return(override_path)
  }

  namespace_path <- tryCatch(
    getNamespaceInfo(asNamespace("teachRai"), "path"),
    error = function(...) ""
  )

  fallback_path <- ""

  if (is.character(namespace_path) && nzchar(namespace_path)) {
    candidate <- file.path(namespace_path, "inst", "extdata", "exemplars.yml")
    candidate <- normalizePath(candidate, winslash = "/", mustWork = FALSE)

    if (file.exists(candidate)) {
      fallback_path <- candidate
    }
  }

  if (!is.na(fallback_path) && nzchar(fallback_path)) {
    return(fallback_path)
  }

  ""
}

teachr_coerce_exemplar_scalar <- function(x, field, row_index) {
  if (is.null(x) || !length(x)) {
    return("")
  }

  if (length(x) != 1L) {
    stop(
      paste0(
        "Exemplar row ",
        row_index,
        " field `",
        field,
        "` must be a single scalar value."
      ),
      call. = FALSE
    )
  }

  value <- x[[1]]

  if (is.list(value) || length(value) != 1L) {
    stop(
      paste0(
        "Exemplar row ",
        row_index,
        " field `",
        field,
        "` must be a single scalar value."
      ),
      call. = FALSE
    )
  }

  if (is.na(value)) {
    return("")
  }

  as.character(value)
}

teachr_load_exemplars <- function(path = teachr_exemplar_yaml_path()) {
  if (!nzchar(path) || !file.exists(path)) {
    stop("Exemplar YAML file was not found in package extdata.", call. = FALSE)
  }

  payload <- tryCatch(
    yaml::read_yaml(path),
    error = function(err) {
      stop(
        paste("Failed to parse exemplar YAML:", conditionMessage(err)),
        call. = FALSE
      )
    }
  )

  if (!is.list(payload)) {
    stop("Exemplar YAML must contain a top-level mapping.", call. = FALSE)
  }

  exemplars <- payload$exemplars

  if (is.null(exemplars)) {
    stop("Exemplar YAML must include a non-empty `exemplars` key.", call. = FALSE)
  }

  if (!is.list(exemplars)) {
    stop("`exemplars` must be a list of exemplar records.", call. = FALSE)
  }

  if (!length(exemplars)) {
    stop("Exemplar YAML must include a non-empty `exemplars` key.", call. = FALSE)
  }

  rows <- lapply(seq_along(exemplars), function(i) {
    exemplar <- exemplars[[i]]

    exemplar_names <- names(exemplar)

    if (
      !is.list(exemplar) ||
        is.null(exemplar_names) ||
        any(!nzchar(trimws(exemplar_names)))
    ) {
      stop("Each exemplar record must be a named mapping.", call. = FALSE)
    }

    missing_columns <- setdiff(teachr_required_exemplar_columns, names(exemplar))

    if (length(missing_columns)) {
      stop(
        paste0(
          "Exemplar row ",
          i,
          " is missing required fields: ",
          paste(missing_columns, collapse = ", ")
        ),
        call. = FALSE
      )
    }

    exemplar <- exemplar[teachr_required_exemplar_columns]
    exemplar[] <- Map(
      function(value, field) {
        teachr_coerce_exemplar_scalar(value, field = field, row_index = i)
      },
      exemplar,
      names(exemplar)
    )

    as.data.frame(exemplar, stringsAsFactors = FALSE)
  })

  if (!length(rows)) {
    stop("Exemplar YAML must include a non-empty `exemplars` key.", call. = FALSE)
  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL

  missing_columns <- setdiff(teachr_required_exemplar_columns, names(out))

  if (length(missing_columns)) {
    stop(
      paste(
        "Loaded exemplars are missing required columns:",
        paste(missing_columns, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  duplicate_ids <- unique(out$id[duplicated(out$id)])

  if (length(duplicate_ids)) {
    stop(
      paste(
        "Exemplar `id` values must be unique. Duplicates:",
        paste(duplicate_ids, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  invalid_modes <- setdiff(unique(out$mode), teachr_allowed_exemplar_modes)

  if (length(invalid_modes)) {
    stop(
      paste(
        "Exemplar `mode` values must be one of:",
        paste(teachr_allowed_exemplar_modes, collapse = ", "),
        "- found:",
        paste(invalid_modes, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  out[teachr_required_exemplar_columns]
}

teachr_exemplar_cache <- new.env(parent = emptyenv())
teachr_exemplar_cache$data <- NULL

teachr_get_exemplars <- function(force_reload = FALSE) {
  if (isTRUE(force_reload) || is.null(teachr_exemplar_cache$data)) {
    teachr_exemplar_cache$data <- teachr_load_exemplars()
  }

  teachr_exemplar_cache$data
}

makeActiveBinding(
  "teachr_exemplars",
  function() teachr_get_exemplars(),
  env = environment()
)

teachr_find_exemplars <- function(mode,
                                  selection = "",
                                  recent_error = "",
                                  packages = character(),
                                  goal_text = "",
                                  n = 2) {
  mode <- teachr_match_mode(mode)
  exemplar_pool <- teachr_get_exemplars()
  pool <- exemplar_pool[exemplar_pool$mode == mode, , drop = FALSE]

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
