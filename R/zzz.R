teachr_exemplars <- NULL

.onLoad <- function(libname, pkgname) {
  teachr_exemplars <<- teachr_load_exemplars()
}
