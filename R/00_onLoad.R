
# for quarto/rmd
#' @importFrom kableExtra kable
#' @importFrom knitr opts_chunk pandoc_to

.onLoad <- function(libname, pkgname) {
  op <- options()
  op.targeter <- list(
    targeter.use_naming_conventions = FALSE,
    targeter.useNA = "ifany",
    targeter.author = "wlds targeter",
    targeter.auto_install_deps = TRUE
  )
  toset <- !(names(op.targeter) %in% names(op))
  if (any(toset)) options(op.targeter[toset])

  invisible()
}
