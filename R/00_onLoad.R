.onLoad <- function(libname, pkgname) {
  op <- options()
  op.profile <- list(
    profile.use_naming_conventions = FALSE,
    profile.useNA = "ifany"
  )
  toset <- !(names(op.profile) %in% names(op))
  if(any(toset)) options(op.profile[toset])

  invisible()
}
