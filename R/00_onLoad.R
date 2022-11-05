.onLoad <- function(libname, pkgname) {
  op <- options()
  op.targeter <- list(
    targeter.use_naming_conventions = FALSE,
    targeter.useNA = "ifany"
  )
  toset <- !(names(op.targeter) %in% names(op))
  if(any(toset)) options(op.targeter[toset])

  invisible()
}
