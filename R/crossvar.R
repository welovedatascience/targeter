#' @title Cross a single variable with a target

#' @description this is a convenience function that behind the scene calls complete \code{\link[targeter]{targeter}}
#'  function on data. This allows to quickly assess relation for one variable and have access to plots. It is highly
#'  recommended to have a look at \code{\link[targeter]{targeter}} parameters as they could be used here also.

#' @param data data.frame or data.table
#' @param target character - name of the target variable of interest
#' @param var character - name of the candidate variable to be crossed with target
#' @param ... Additional parameter to be passed to \code{\link[targeter]{targeter}}

#' @return returns an object of class crossvar
#' @details content of returned object depends on the type of the target (binary/categorical/continuous.)
#'  For more information, look at the documentation of the crossvar class.

#' @examples
#' \dontrun{
#' if(interactive()){
#'  data(adult)
#'  crossvar(adult, target='ABOVE50K', var='AGE')
#'  t <- crossvar(adult, target='HOURSPERWEEK', var='FNLWGT')
#'  plot(t)
#'  }
#' }

#' @seealso
#'  \code{\link[targeter]{targeter}}
#'  \code{\link[targeter]{plot.crossvar}}

#' @rdname crossvar
#' @export crossvar
#' @importFrom assertthat assert_that
#' @importFrom data.table as.data.table

crossvar <- function(data, target, var, ...) {
  assertthat::assert_that(
    inherits(data, "data.frame") | inherits(data, "data.table"),
    msg = "Data must to be a data.frame or a data.table"
  )
  assertthat::assert_that(
    inherits(target, "character"),
    msg = "Target must  be a character"
  )
  assertthat::assert_that(
    inherits(var, "character"),
    msg = "var must be a character"
  )
  assertthat::assert_that(
    var %in% names(data),
    msg = "var is not present in data"
  )
  assertthat::assert_that(
    target %in% names(data),
    msg = "target is not present in data"
  )
  assertthat::assert_that(
    requireNamespace('data.table', quietly = TRUE),
    msg = 'data.table package required'
  )
  data_sel <- data.table::as.data.table(data)[, c(target, var), with = FALSE]
  targeter(data_sel, target = target, select_vars = var, ...)$profiles[[var]]
}

#' @method print crossvar
#' @export
print.crossvar <- function(x, ...) {
  cat("\nTargeter crossvar profiling object with following properties:")
  cat(paste0("\n\tTarget:"), x$targetname, " of type:", x$target_type)
  if (x$target_type == 'binary')
    cat(paste0("  (target level:", x$target_reference_level), ")")
  cat(
    paste0(
      "\n\tCrossed with variable: ",
      x$varname,
      " of type: ",
      x$variable_type,
      "\n\n"
    )
  )
  if (x$target_type %in% c("binary", "categorical")) {
    print(cbind(as.data.frame.matrix(x$counts), as.data.frame.matrix(x$props)))
  } else if (x$target_type %in% c("numeric")) {
    if (!is.null(x$woe)) {
      x$stats <- cbind(x$stats, x$woe)
    }
    print(x$stats)
  }
  if (!is.null(x$IV)) cat(paste0("\nInfomation value: ", round(x$IV, 6), "\n"))
  cat(
    "\nThere is a `summary` method available as well as various `plot` function."
  )
}
