
#' @method summary crossvar
#' @export
summary.crossvar <- function(x, extra_stats=FALSE, which_minmax=as.character(x$target_reference_level),...){
  assertthat::assert_that(inherits(x,c("crossvar")),msg="x must be a crosscall object from profile package")
  assertthat::assert_that(inherits(extra_stats,c("logical")),msg="extra_stats  must be a logical")

  if (is.null(x)) return(data.frame())

  if (x$target_type %in% c("binary","categorical")){
    if (is.na(which_minmax)) which_minmax <- "NA"
    if (is.character(which_minmax)){
      which_minmax <- which(names(x$counts)==which_minmax)
    }
  }
  out <- data.frame(
    varname=x$varname,
    targetname= x$targetname,
    vartype=x$variable_type,
    stringsAsFactors = FALSE
  )
  if (x$target_type != "categorical") out$IV <- x$IV

  ## binary/categorical target
  if (x$target_type %in% c("binary","categorical")){
    counts <- x$counts
    index <- x$index
    props <- x$props
    totalcount <- sum(counts)
    levels  <-rownames(counts)
    index.max.which <- which.max(index[,which_minmax])
    index.max.level <- levels[index.max.which]
    index.max.count <- counts[index.max.level, which_minmax]
    index.max.index <- index[index.max.level, which_minmax]
    index.max.props <- props[index.max.level, which_minmax]

    index.min.which <- which.min(index[,which_minmax])
    index.min.level <- levels[index.min.which]
    index.min.count <- counts[index.min.level, which_minmax]
    index.min.index <- index[index.min.level, which_minmax]
    index.min.props <- props[index.min.level, which_minmax]
    which_minmax.level <- ifelse (
      is.numeric(which_minmax),
      colnames(props)[which_minmax] ,
      which_minmax)



    minmax <- data.frame(
      index.max.level=index.max.level,
      index.max.count=index.max.count,
      index.max.index=index.max.index,
      index.max.props=index.max.props,
      index.min.level=index.min.level,
      index.min.count=index.min.count,
      index.min.index=index.min.index,
      index.min.props=index.min.props,
      which_minmax.level=which_minmax.level
    )

    out <- cbind(out,minmax)

    if (extra_stats){
      ### extra stats
      ## conversion to a table to have chi square, and prop.table
      tab <- as.table(as.matrix(counts))
      resume <- summary(tab)
      extra <- data.frame(
        chisquare = resume$statistic,
        pvalue = resume$p.value)
      out <- cbind(out, extra)

    }
  }

  ## continuous target
  if (x$target_type %in% c("numeric")){
    ### extra stats
    if (extra_stats){
      stats <- x$stats
      out <- cbind(out,aov_aggstats(stats$avg, stats$std, stats$count, full_output=FALSE))
    }
  }


  return(out)
}
