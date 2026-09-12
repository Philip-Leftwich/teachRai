teachr_error_state <- function(context) {
  err <- trimws(context$recent_error %||% "")

  if (!nzchar(err)) {
    return("absent")
  }

  selection <- trimws(context$selection %||% "")

  if (!nzchar(selection)) {
    # An error with no current selection to check it against: keep it, but
    # flag as uncertain rather than asserting relevance we can't verify.
    return("uncertain")
  }

  if (teachr_error_relates_to_selection(err, selection)) {
    "present"
  } else {
    "uncertain"
  }
}

teachr_error_relates_to_selection <- function(error_text, selection_text) {
  error_words <- teachr_split_words(error_text)
  selection_words <- unique(teachr_split_words(selection_text))

  if (!length(error_words) || !length(selection_words)) {
    return(FALSE)
  }

  candidate_terms <- unique(error_words[nchar(error_words) >= 4])
  any(candidate_terms %in% selection_words)
}
