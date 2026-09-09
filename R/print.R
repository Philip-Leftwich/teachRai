teachr_print_response <- function(response, mode = NULL) {
  heading <- "teachRai Assistant"

  if (!is.null(mode)) {
    heading <- paste0(heading, " [", teachr_title_case(mode), "]")
  }

  cat("\n", heading, "\n", sep = "")
  cat(strrep("-", nchar(heading)), "\n", sep = "")
  cat(response, "\n", sep = "")

  invisible(response)
}
