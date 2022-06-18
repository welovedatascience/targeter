#' @title Cross a single variable with a target
#' @description this is a convenience function that behind the scene calls complete \code{\link[targeter]{targeter}}
#' function on data. This allows to quickly assess relation for one variable and have access to plots. It is highly
#' recommended to have a look at \code{\link[targeter]{targeter}} parameters as they could be used here also.
#' @param data data.frame or data.table
#' @param target character - name of the target variable of interest
#' @param var character - name of the candidate variable to be crossed with target
#' @param ... Additional parameter to be passeed to \code{\link[targeter]{targeter}}
#' @return returns an object of class 'crossvar'
#' @details content of returned object depends on the type of the target (binary/categorical/continuous.)
#' For more information, look at the documentation of the crosssvar class [todo].
#' @examples
#' \dontrun{
#' if(interactive()){
#'  crossvar(iris, target='Sepal.Length', var='Species')
#'  crossvar(iris, target='Species', var='Sepal.Length')
#'  }
#' }
#' @seealso
#'  \code{\link[targeter]{targeter}}
#'  \code{\link[plot.crossvar]{plot.crossvar}}
#' @rdname crossvar
#' @export
#' @importFrom assertthat assert_that
crossvar <- function(data, target, var, ...){
  assertthat::assert_that(inherits(data,"data.frame") | inherits(data, "data.table"), msg = "Data must to be a data.frame or a data.table")
  assertthat::assert_that(inherits(target,"character"), msg = "Target must  be a character")
  assertthat::assert_that(inherits(var,"character"), msg = "var must be a character")
  assertthat::assert_that(var %in% names(data), msg = "var is not present in data")
  assertthat::assert_that(target %in% names(data), msg = "target is not present in data")
  assertthat::assert_that(requireNamespace('data.table', quietly = TRUE), msg='data.table package required')
  data_sel <- as.data.table(data)[,c(target, var), with=FALSE]
  targeter(data_sel, target=target, select_vars=var,...)$profiles[[var]]
}


# crossvar(iris, target='Sepal.Length', var='Species')
# crossvar(iris, target='Species', var='Sepal.Length')

