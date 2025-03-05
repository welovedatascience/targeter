
# for quarto/rmd
#' @importFrom kableExtra kable

.onLoad <- function(libname, pkgname) {
  op <- options()
  op.targeter <- list(
    targeter.use_naming_conventions = FALSE,
    targeter.useNA = "ifany",
    targeter.author = "welovedatascience targeter package",
    targeter.auto_install_deps = TRUE,
    targeter.brandfile =file.path(
      find.package("targeter", lib.loc = .libPaths()),
      "ressources",
      "_brand.yml"
    )
  )
  toset <- !(names(op.targeter) %in% names(op))
  if (any(toset)) options(op.targeter[toset])

  invisible()
}
