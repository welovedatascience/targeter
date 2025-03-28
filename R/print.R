
#' @method print targeter
#' @export
#' @title Print method for targeter objects
#' @description Print method for targeter objects
#' @param x A targeter object
#' @param ... Additional arguments (not used)
#' @return NULL
#' @details This method prints a summary of the targeter object, 
#' including the target variable, the data used, and the available profiles.
#' @importFrom utils head
print.targeter <- function(x, ...) {
  cat("\nTarget profiling object with following properties:")
  cat(paste0("\n\tTarget:"), x$target, " of type:", x$target_type)
  if (x$target_type == 'binary')
    cat(paste0("  (target level:", x$target_reference_level), ")")
  cat(paste0("\n\tRun on data:"), x$dataname, " the:", format(x$date))
  nprof <- length(x$profiles)
  vars_profile <- utils::head(names(x$profiles), 5)
  vars_profile <- paste(vars_profile, collapse = ", ")
  if (nprof > 5) vars_profile <- paste0(vars_profile, "...")
  cat(
    paste0("\n", length(x$profiles), " profiles available (", vars_profile, ")")
  )
  cat(
    paste0(
      "\nYou can access each crossing using slot $profiles[[__variable__]]. Then on it use `plot` or `summary`"
    )
  )
  cat(
    paste0(
      "\nYou can also directly invoke a global `summary` function on this object.\n"
    )
  )
}

