% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summary_crossvar.R
\name{summary.crossvar}
\alias{summary.crossvar}
\title{summary.crossvar}
\usage{
\method{summary}{crossvar}(
  object,
  extra_stats = FALSE,
  which_minmax = as.character(object$target_reference_level),
  ...
)
}
\arguments{
\item{object}{an object of class targeter}

\item{extra_stats}{boolean - whether function returns some additional associations statistics (default: FALSE)}

\item{which_minmax}{defines which target level is used as reference/target for minmax computations}

\item{...}{additional parameters (currently not used)}
}
\value{
a one-row data.table
}
\description{
summary method for crossvar object
}
\details{
Returned table contains one row for the crossvar explanatory variable analyzed in targeter and columns for associations measures / metrics.
}
\examples{
 data(adult)
 x <- crossvar(adult, target='HOURSPERWEEK', var='FNLWGT')
summary(x)
summary(x, extra_stats = TRUE)
}
\seealso{
\code{\link{summary.targeter}}
 \code{\link{crossvar}}
 \code{\link{targeter}}
}
