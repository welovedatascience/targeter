#' Function focus()
#'
#' The function subset the targeter object and retains the
#' n top discriminant variables with regards to a criteria.
#' It also allows filtering on minimum number of values within
#'  modality which is useful when using criteria index.max.index.
#'
#' @param x an object of class "targeter".
#' @param criteria character - sort the output according to
#'  (by default, order by decreasing index.max.index):
#' \itemize{
#' \item "IV" - information value.
#' \item "index.max.index" -value of the maximum index.
#' \item "chisquare" - chi-square value.
#' \item "pvalue" - p-value.
#' \item "index.max.count" - number of observations in the class
#' associated to the maximum index.
#' \item "index.max.props" - proportion of the class
#' associated to the maximum index.
#' }
#' @param nmin integer - minimum number of profiles in the class.
#'  By default, 100.
#' @param n integer - number of top variables to select.
#' By default, n = 10.
#' @param min_criteria - By default NULL. If it's specified,
#' only the observations that have criteria >= min_criteria.
#' @param force_vars - character, list of variables that will be
#' kept and then won't never be filtered.
#' @param summary_object - By default NULL. If provided, must be
#'  pre-computed summary on x callcross var object
#'
#' @return It returns the filtered input object.
#'
#' @seealso
#' \itemize{
#' \item \code{\link{targeter}}
#' \item \code{\link{summary.targeter}}
#' }
#'
#' @export focus
#'
#' @examples
#' data(adult)
#' t <- targeter(adult, target = "ABOVE50K", analysis_name = "Analyse")
#' focus(t)
#' focus(t, n = 3)
#' focus(t, nmin = 500, criteria = "index.max.index")
#' focus(t, criteria = "chisquare")
focus <- function(x,
                  criteria = c(
                    "IV", "index.max.index", "chisquare",
                    "pvalue", "index.max.count", "index.max.props"
                  ),
                  n = 10,
                  nmin = NULL,
                  min_criteria = NULL,
                  force_vars = character(),
                  summary_object = NULL) {

  ## test
  assertthat::assert_that(inherits(x, "targeter"),
    msg = "Error: x must be an object of class 'targeter'."
  )
  assertthat::assert_that(
    is.null(nmin) | inherits(nmin, "numeric") | inherits(nmin, "integer"),
    msg = "The parameter nmin must  be  integer or numeric"
  )
  assertthat::assert_that(
    inherits(n, "numeric") | inherits(n, "integer"),
    msg = "The parameter n must be  integer or numeric"
  )
  assertthat::assert_that(
    inherits(min_criteria, "numeric") |
      inherits(min_criteria, "integer") |
      is.null(min_criteria),
    msg = "The parameter min_criteria must be  integer or numeric"
  )

  ## Verify that criteria has the good values
  criteria <- match.arg(criteria[1],
    c(
      "IV", "chisquare", "pvalue", "index.max.count",
      "index.max.props", "index.max.index"
    ),
    several.ok = FALSE
  )

  ## Apply the summary function
  if (!is.null(summary_object)) {
    ## <TODO> add checks for consistency between summary_object and x
    tmp <- summary_object
    if (criteria %in% c("index.max.index", "chisquare", "pvalue",
      "index.max.count", "index.max.props")) {
      assertthat::assert_that(criteria %in% names(tmp), 
      msg = "Criteria not in summary object. Did you use extra_stats?")
    }
  } else {
    extra_stats <- criteria %in% c("index.max.index", "chisquare",
    "pvalue", "index.max.count", "index.max.props")
    tmp <- summary.targeter(x, extra_stats = extra_stats)
  }

  ## filter on minimum number of records
  if (!is.null(nmin)) tmp <- tmp[tmp$index.max.count >= nmin, ]

  ## minimum value for criteria
  if (!is.null(min_criteria)) tmp <- tmp[tmp[[criteria]] >= min_criteria, ]

  ## reorder per selected criteria
  tmp <- tmp[order(tmp[[criteria]],
    decreasing =
      ifelse(criteria %in% c("pvalue"), FALSE, TRUE)
  ), ]

  ## retrieve top n records corresponding variable names
  varnames <- utils::head(tmp[["varname"]], n)

  # add forced variables
  varnames <- unique(c(force_vars, varnames))

  ## filter the object
  sub <- x$profiles[which(names(x$profiles) %in% varnames)]

  out <- x
  out$profiles <- sub

  return(out)
}
