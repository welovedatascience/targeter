% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/targeter.R
\name{targeter}
\alias{targeter}
\title{targeter}
\usage{
targeter(
  data,
  description_data = NULL,
  target,
  target_type = c("autoguess", "binary", "categorical", "numeric"),
  target_reference_level = NULL,
  description_target = NULL,
  analysis_name = NULL,
  select_vars = NULL,
  exclude_vars = NULL,
  nbins = 12,
  binning_method = c("quantile", "clustering", "smart"),
  naming_conventions = getOption("targeter.use_naming_conventions"),
  useNA = getOption("targeter.useNA"),
  verbose = FALSE,
  dec = 2,
  order_label = c("auto", "alpha", "count", "props", "means"),
  cont_target_trim = 0.01,
  bxp_factor = 1.5,
  num_as_categorical_nval = 5,
  autoguess_nrows = 1000,
  woe_alternate_version = c("if_continuous", "always"),
  woe_shift = 0.01,
  woe_post_cluster = FALSE,
  woe_post_cluster_n = 6,
  smart_quantile_by = 0.01
)
}
\arguments{
\item{data}{data - data.table or data.frame.}

\item{description_data}{text on the description of data.}

\item{target}{character - name of the variable to explain.}

\item{target_type}{character: type of target - one of 'autoguess' (default), 'binary','categorical' (>2 modalities) or 'numeric'.
ExpParameter expansion if applied so that one could also use 'a' or 'b','c' or 'n'}

\item{target_reference_level}{character or numeric. For categorical or (especially) binary targets, level / value of special
TRUE or 1 or 'bad' for binary targets.
that}

\item{description_target}{text on the description of target.}

\item{analysis_name}{name of the analysis.}

\item{select_vars}{a list of explanatory variables. By default, NULL and all columns are considered.}

\item{exclude_vars}{a list of variables to exclude from the analysis.}

\item{nbins}{The nbins is by default 10. It is the number of quantiles to be considered.}

\item{binning_method}{character, one of 'quantile' (default) or 'clustering', 'smart' (parameter expansion usable).
Method used to derive the `nbins` buckets for the continuous explanatory variables.}

\item{naming_conventions}{boolean - by default TRUE. It means that a certain naming convention is respected.}

\item{useNA}{Two values are possible : "ifany" and "no". By default, the package option is "ifany".
\itemize{
\item The value "ifany" takes in consideration the missing values if there are any.
\item The value "no" doesn't take in consideration the missing values in any case.
}}

\item{verbose}{- boolean (default FALSE). If TRUE some more information is displayed in console. Could be useful
when using package on big data.}

\item{dec}{- integer : the number of decimal for numeric variable. By default, the value is 2.}

\item{order_label}{character - this option output an order used for the plot function. The parameter can only take the following values:
\itemize{
\item "auto": for the numeric variable, the order is alphabetic. For the categorical variable, the order is the count
\item "alpha": order alphabetic
\item "count": order decreasing of the number of observations by class
\item "props : order decreasing by the proportion of the second value of the target
\item "means : order decreasing by the target mean (continuous target)
}}

\item{cont_target_trim}{numeric (default 0.01). For continuous targets, it is desirable to trim
extreme values before computing WOE. This is the trimming factor in percentage (between 0: no trim and <1).}

\item{bxp_factor}{(default) 1.5 for continuous target, coefficient to be used to compute boxplot whiskers.}

\item{num_as_categorical_nval}{(default: 5). If a variable has less than num_as_categorical_nval distinct values,
it will be considered and used as categorical and not numeric.}

\item{autoguess_nrows}{- integer (default 1000). Numbers of rows to be used to guess the variables types.
Special value 0 could be provided and then all rows be be used.}

\item{woe_alternate_version}{character. Specify in which context WOE alternate definition will be used
See vignette on  methodology. Possible values are 'if_continuous' (default) or 'always'.}

\item{woe_shift}{numeric (default) 0.01. Shifting value in WOE computation to prevent issues when one bucket
contains 0\% or 100\% of target. For binary target, some people also propose to use 0.5.}

\item{woe_post_cluster}{boolean (default FALSE). Once WOE are computed, on could cluster values to see if this
this could be adequate to group together some modalities/buckets. Clusters would be used in graphics. See vignette on methodology.}

\item{woe_post_cluster_n}{integer (default: 6). If woe_post_cluster is TRUE, number of clusters to be used.}

\item{smart_quantile_by}{numeric, for binning method 'smart', quantile step - default y step of 0.01.}
}
\value{
The function returns a list of class "targeter".
This list is composed of this following elements:
\itemize{
\item dataname - name of the analyzed dataset.
\item description_data text about the description of data
\item target - name of the target.
\item target_type - Target type, one of autoguess (default)/binary/numeric/categorical (text is expended so "b" also works for binary)
\item description_target text about the description of target
\item analysis - name of the analysis.
\item date - date of the analysis.
\item profiles : list of elements containing the result the individual crossing per variable.
See crossvar class documentation.
}
}
\description{
For each variable, the function crosses two variables: a target to be explained and an explanatory variable.
For this purpose, these variables are converted in categorical variables by a binning process and the statistics
are derived.
\itemize{
\item The contingency table gives the counts of each class of the explanatory variable for each modality of the target.
\item A proportion is calculated as the count of profiles per class and target modality is divided by the sum of profiles by class (=count/sum of row counts).
\item An index is calculated as the proportion of profiles per class and modality divided by the proportion of the modality of all profiles (=proportion / (sum of column counts/sum of all counts)).
\itemize{
\item If the value of the index is high (more than one), it implies that
for this subpopulation is over-represented for this variable.
\item If the value of the index is equal to 1, the criterium is not significant
\item If the value of the index is less than 1, it implies that
for this subpopulation is sub-represented for this variable.
}
\item Weight of Evidence and Information Value are derived for binary and continuous targets.
}
}
\examples{
targeter(adult,target ="ABOVE50K")
}
\seealso{
\itemize{
\item \code{\link{summary.targeter}}
\item \code{\link{plot.crossvar}}
\item \code{\link{summary.crossvar}}
\item \code{\link{report}}
}
}
