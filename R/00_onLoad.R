
## Imports required for Rmd/Quarto templates
#' @importFrom kableExtra kable_styling
#' @importFrom knitr kable
#' @importFrom DT datatable


.onLoad <- function(libname, pkgname) {
  op <- options()
  op.targeter <- list(
    targeter.use_naming_conventions = FALSE,
    targeter.useNA = "ifany",
    targeter.author = "welovedatascience targeter package"
  )
  toset <- !(names(op.targeter) %in% names(op))
  if (any(toset)) options(op.targeter[toset])

  invisible()
}
