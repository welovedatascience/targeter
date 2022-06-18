summary.targeter <- function(x,
                             extra_stats=FALSE,
                             criteria=c( "IV","index.max.index","chisquare", "pvalue", "index.max.count", "index.max.props"),...){


  assertthat::assert_that(inherits(x,"targeter"), msg = "The parameter x must to be an object of class targeter")
  criteria <- match.arg(criteria, c( "IV","index.max.index","chisquare", "pvalue", "index.max.count", "index.max.props"), several.ok = FALSE)


  if (criteria %in% c("chisquare","pvalue") & !extra_stats) stop("Sorting criteria requires extra_stats computations.")
  if (x$target_type =="categorical" & criteria=="IV"){
    criteria <- "pvalue"
    extra_stats <- TRUE
    cat("\ntarget is categorical, sorting criteria changed to p-value (extra stats activated)\n")
  }

  allstats <- lapply(x$profiles, summary.crossvar, extra_stats=extra_stats)
  out <- rbindlist(allstats)
  data.table::setorderv(out, criteria, order=ifelse(criteria %in% c("pvalue"),1,-1))

  return(out)


}
