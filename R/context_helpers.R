teachr_error_state <- function(context) {
  err <- context$recent_error %||% ""

  if (!nzchar(trimws(err))) {
    return("absent")
  }

  "present"
}
