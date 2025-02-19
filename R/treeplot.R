
# @importFrom visNetwork visTree
# @importFrom rpart.plot rpart.plot

treeplot <- function(
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
  assertthat::assert_that(
    inherits(x, 'targeter'),
    msg = 'x is not a targeter object.'
  )
  assertthat::assert_that(
    x$decision_tree,
    msg = "targeter has not been fitted with a decision tree"
  )
  assertthat::assert_that(
    inherits(x$decision_tree_model, 'rpart'),
    msg = "there is a problem with the fitted decision tree"
  )

  dt <- x$decision_tree_model
  format <- match.arg(format, c("static", "interactive"), several.ok = FALSE)
  if (format == "static") {
    rpart.plot::rpart.plot(
      dt,
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
    visNetwork::visTree(dt, ...)
  }
}
