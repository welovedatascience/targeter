# to prevent checks of data.table used variables
# see:  ?globalVariables

if (getRversion() >= "3.1.0")
  utils::globalVariables(
    c(
      "rpart.plot",
      "visTree")
  )



# ' @importFrom visNetwork visTree
# ' @importFrom rpart.plot rpart.plot

#' @title Plot a targeter tartree object
#'
#' @description This function creates a plot for a targeter decision tree 
#' model build with `tartree` function If format is static, one will use
#' rpart.plot package. If interactive, one will use visTree.
#' 
#' @param x - a decision tree as fitted by `tartree` function
#' @param type One of 'static' (use rpart.plot) or interactive (use visTree)
#' Parameter expansion is used so that one can use "i". Default: static. 
#' @param format see `rpart.plot` help
#' @param prefix -see `rpart.plot` help
#' @param yesno -see `rpart.plot` help
#' @param branch -see `rpart.plot` help
#' @param branch.type -see `rpart.plot` help
#' @param box.palette -see `rpart.plot` help
#' @param shadow.col -see `rpart.plot` help
#' @param cex -see `rpart.plot` help
#' @param left -see `rpart.plot` help
#' @param ... -  eventuial additional parameters to be passed to `rpart.plot`
#' or visTree

#' @importFrom pacman p_load
#' @export
plot.tartree <- function(
  x,
  format = "static",
  prefix = "target = ",
  type = 2,
  yesno = 2,
  branch = 0,
  branch.type = 5,
  box.palette = "Blues",
  shadow.col = 0,
  cex = 0.7,
  left = FALSE,
  ...
) {

  deps <- c("visNetwork","visTree","rpart.plot")
  if (getOption("targeter.auto_install_deps", FALSE)){
    pacman::p_load(char=deps, install = FALSE)
  }
  assertthat::assert_that(all(pacman::p_load(deps)), 
  msg=paste('some of targeter following optional packages are not available:',
  paste(deps, collapse=",")))
  


  assertthat::assert_that(
    inherits(x, 'rpart'),
    msg = "there is a problem with the fitted decision tree"
  )

  
  format <- match.arg(format, c("static", "interactive"), several.ok = FALSE)
  if (format == "static") {
    # rpart.plot::rpart.plot(
    rpart.plot(
    
      x,
      prefix = prefix,
      type = type,
      yesno = yesno,
      branch = branch,
      branch.type = branch.type,
      box.palette = box.palette,
      shadow.col = shadow.col,
      cex = cex,
      left = left,
      ...
    )
  } else {
    # visNetwork::visTree(dt, ...)
    visTree(x, ...)
  }
}
