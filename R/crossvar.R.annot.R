## crossvar.R - compiled by RoxygenReady, a package by @vertesy


#' crossvar 
#' 
#' crossvar <- function(data, target, var, ...){
#' @param data 
#' @param target 
#' @param var 
#' @param ... 
#' @examples crossvar(data =  , target =  , var =  , ... =  )
#' @export 

crossvar <-function (data, target, var, ...) {
	assertthat::assert_that(inherits(data, "data.frame") | inherits(data, "data.table"), msg = "Data must to be a data.frame or a data.table")
	assertthat::assert_that(inherits(target, "character"), msg = "Target must  be a character")
	assertthat::assert_that(inherits(var, "character"), msg = "var must be a character")
	assertthat::assert_that(var %in% names(data), msg = "var is not present in data")
	assertthat::assert_that(target %in% names(data), msg = "target is not present in data")
	assertthat::assert_that(requireNamespace("data.table", quietly = TRUE), msg = "data.table package required")
	data_sel <- as.data.table(data)[, c(target, var), with = FALSE]
	targeter(data_sel, target = target, select_vars = var, ...)$profiles[[var]]
}


