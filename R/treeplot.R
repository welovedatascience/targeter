
# ' @importFrom visNetwork visTree
# ' @importFrom rpart.plot rpart.plot

#' @importFrom pacman p_load
#' @export treeplot


treeplot <- function(
  decision_tree,
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
    inherits(decision_tree, 'rpart'),
    msg = "there is a problem with the fitted decision tree"
  )

  
  format <- match.arg(format, c("static", "interactive"), several.ok = FALSE)
  if (format == "static") {
    # rpart.plot::rpart.plot(
    rpart.plot(
    
      decision_tree,
      prefix = prefix,
      type = type,
      yesno = yesno,
      branch = branch,
      branch.type = branch.type,
      box.palette = "Blues",
      shadow.col = 0,
      cex = 0.7,
      left = FALSE,
      ...
    )
  } else {
    # visNetwork::visTree(dt, ...)
    visTree(decision_tree, ...)
  }
}
