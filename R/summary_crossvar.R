#' @title summary.crossvar
#' @description summary method for crossvar object
#' @param object an object of class targeter
#' @param extra_stats boolean - whether function returns some additional associations statistics (default: FALSE)
#' @param which_minmax defines which target level is used as reference/target for minmax computations
#' @param ... additional parameters (currently not used)
#' @return a one-row data.table
#' @details
#' Returned table contains one row for the crossvar explanatory variable analyzed in targeter and columns for associations measures / metrics.
#'
#' @examples
#'  data(adult)
#'  x <- crossvar(adult, target='HOURSPERWEEK', var='FNLWGT')
#' summary(x)
#' summary(x, extra_stats = TRUE)
#' @seealso
#'  \code{\link{summary.targeter}}
#'  \code{\link{crossvar}}
#'  \code{\link{targeter}}
#' @method summary crossvar
#' @export
summary.crossvar <- function(
  object,
  extra_stats = FALSE,
  which_minmax = as.character(object$target_reference_level),
  ...
) {
  assertthat::assert_that(
    inherits(object, c("crossvar")),
    msg = "object must be a crosscall object from profile package"
  )
  assertthat::assert_that(
    inherits(extra_stats, c("logical")),
    msg = "extra_stats  must be a logical"
  )

  if (is.null(object)) return(data.frame())

  if (object$target_type %in% c("binary", "categorical")) {
    if (is.na(which_minmax)) which_minmax <- "NA"
    if (is.character(which_minmax)) {
      which_minmax <- which(names(object$counts) == which_minmax)
    }
  }
  out <- data.frame(
    varname = object$varname,
    targetname = object$targetname,
    vartype = object$variable_type,
    stringsAsFactors = FALSE
  )
  if (object$target_type != "categorical") {
    out$IV <- object$IV
    #major impact direction
    direction <- sign(range(object$woe)[which.max(abs(range(object$woe)))])
    out$highest_impact <- ifelse(
      direction == 1,
      '[+] over-target',
      '[-] under-target'
    )
  }

  ## binary/categorical target
  if (object$target_type %in% c("binary", "categorical")) {
    counts <- object$counts
    index <- object$index
    props <- object$props
    totalcount <- sum(counts)
    levels <- rownames(counts)
    index.max.which <- which.max(index[, which_minmax])
    index.max.level <- levels[index.max.which]
    index.max.count <- counts[index.max.level, which_minmax]
    index.max.index <- index[index.max.level, which_minmax]
    index.max.props <- props[index.max.level, which_minmax]

    index.min.which <- which.min(index[, which_minmax])
    index.min.level <- levels[index.min.which]
    index.min.count <- counts[index.min.level, which_minmax]
    index.min.index <- index[index.min.level, which_minmax]
    index.min.props <- props[index.min.level, which_minmax]
    which_minmax.level <- ifelse(
      is.numeric(which_minmax),
      colnames(props)[which_minmax],
      which_minmax
    )

    minmax <- data.frame(
      index.max.level = index.max.level,
      index.max.count = index.max.count,
      index.max.index = index.max.index,
      index.max.props = index.max.props,
      index.min.level = index.min.level,
      index.min.count = index.min.count,
      index.min.index = index.min.index,
      index.min.props = index.min.props,
      which_minmax.level = which_minmax.level
    )

    out <- cbind(out, minmax)

    if (extra_stats) {
      ### extra stats
      ## conversion to a table to have chi square, and prop.table
      tab <- as.table(as.matrix(counts))
      resume <- summary(tab)
      extra <- data.frame(
        chisquare = resume$statistic,
        pvalue = resume$p.value
      )
      out <- cbind(out, extra)
    }
  }

  ## continuous target
  if (object$target_type %in% c("numeric")) {
    ### extra stats
    if (extra_stats) {
      stats <- object$stats
      out <- cbind(
        out,
        aov_aggstats(stats$avg, stats$std, stats$count, full_output = FALSE)
      )
    }
  }

  return(out)
}
