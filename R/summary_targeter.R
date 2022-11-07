#' @title summary.targeter
#' @description summary method for targeter object
#' @param object an object of class targeter
#' @param extra_stats boolean - whether function returns some additional associations statistics (default: FALSE)
#' @param criteria which criteria is used to sort the summary table (default: IV)
#' @param ... additional parameters (currently not used)
#' @return a data.table summary table
#' @details summary method invoked on a targeter object loops over all individual profiles/crossvar/associations and derive statistics (cf \code{\link{summary.crossvar}}).
#' Returned table contains thus one row per explanatory variable analysed in targeter and columns for associations measures / metrics.
#'
#' @examples
#' data(adult)
#' t <- targeter(adult,target ="ABOVE50K",analysis_name="Analyse")
#' summary(t)
#' summary(t, extra_stats = TRUE)
#' @seealso
#'  \code{\link{summary.crossvar}}
#' @method summary targeter
#' @importFrom data.table rbindlist setorderv
#' @export

summary.targeter <- function(object,
                             extra_stats=FALSE,
                             criteria=c( "IV","index.max.index","chisquare", "pvalue", "index.max.count", "index.max.props"),...){


  assertthat::assert_that(inherits(object,"targeter"), msg = "The parameter object must to be an object of class targeter")
  criteria <- match.arg(criteria, c( "IV","index.max.index","chisquare", "pvalue", "index.max.count", "index.max.props"), several.ok = FALSE)


  if (criteria %in% c("chisquare","pvalue") & !extra_stats) stop("Sorting criteria requires extra_stats computations.")
  if (object$target_type =="categorical" & criteria=="IV"){
    criteria <- "pvalue"
    extra_stats <- TRUE
    cat("\ntarget is categorical, sorting criteria changed to p-value (extra stats activated)\n")
  }

  allstats <- lapply(object$profiles, summary.crossvar, extra_stats=extra_stats)
  out <- data.table::rbindlist(allstats)
  data.table::setorderv(out, criteria, order=ifelse(criteria %in% c("pvalue"),1,-1))

  return(out)


}
