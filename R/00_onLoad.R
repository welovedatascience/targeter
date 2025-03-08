
# for quarto/rmd
#' @importFrom kableExtra kable

.onLoad <- function(libname, pkgname) {
  op <- options()
  op.targeter <- list(
    targeter.use_naming_conventions = FALSE,
    targeter.useNA = "ifany",
    targeter.author = "wlds targeter package",
    targeter.auto_install_deps = TRUE
  )
  toset <- !(names(op.targeter) %in% names(op))
  if (any(toset)) options(op.targeter[toset])

  invisible()
}
