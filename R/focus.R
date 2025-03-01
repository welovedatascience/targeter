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
#' By default, n = 25.
#' @param min_criteria - By default NULL. If it's specified,
#' only the observations that have criteria >= min_criteria.
#' @param max_criteria - By default NULL. If it's specified,
#' only the observations that have criteria <= max_criteria. usefull to filter
#' out suspicious variables having (too) high WoE due to high correlation with
#' target.
#' @param force_vars - character, list of variables that will be
#' kept and then won't never be filtered.
#' @param force_vars_groups - list - list of variables, for instance coming 
#' from  a call of `split` on variables names (usually based on some metadata).
#'  default: NULL.
#' @param force_vars_groups_n - integer -  number of variables to be 
#' retained/forced per group? Default: 1
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
focus <- function(
  x,
  summary_object = NULL,
  criteria = c(
    "IV",
    "index.max.index",
    "chisquare",
    "pvalue",
    "index.max.count",
    "index.max.props"
  ),
  n = 25,
  nmin = NULL,
  min_criteria = NULL,
  max_criteria = NULL,
  force_vars = character(),
  force_vars_groups = NULL,
  force_vars_groups_n = 1 
) {
  ## test
  assertthat::assert_that(
    inherits(x, "targeter"),
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
  criteria <- match.arg(
    criteria[1],
    c(
      "IV",
      "chisquare",
      "pvalue",
      "index.max.count",
      "index.max.props",
      "index.max.index"
    ),
    several.ok = FALSE
  )

  ## Apply the summary function
  if (!is.null(summary_object)) {
    ## <TODO> add checks for consistency between summary_object and x
    tmp <- summary_object
    if (
      criteria %in%
        c(
          "index.max.index",
          "chisquare",
          "pvalue",
          "index.max.count",
          "index.max.props"
        )
    ) {
      assertthat::assert_that(
        criteria %in% names(tmp),
        msg = "Criteria not in summary object. Did you use extra_stats?"
      )
    }
  } else {
    extra_stats <- criteria %in%
      c(
        "index.max.index",
        "chisquare",
        "pvalue",
        "index.max.count",
        "index.max.props"
      )
    tmp <- summary.targeter(x, extra_stats = extra_stats)
  }


  tmp[, KEEP_CRITERIA := TRUE]

  ## filter on minimum number of records
  if (!is.null(nmin)){
    tmp[which(index.max.count)<nmin, KEEP_CRITERIA:=FALSE]
  } 

  ## minimum value for criteria
  if (!is.null(min_criteria)){
    tmp[get(criteria)<min_criteria, KEEP_CRITERIA:=FALSE]
  } 

  ## maximum value for criteria
  if (!is.null(max_criteria)){
    tmp[get(criteria)>max_criteria, KEEP_CRITERIA:=FALSE]
  } 

  # KEEP according to criteria

  ## reorder per selected criteria
  setorderv(tmp, criteria, order = ifelse(criteria %in% c("pvalue"), 1, -1))


  ## retrieve top n records corresponding variable names
  varnames_criteria <- utils::head(tmp[which(KEEP_CRITERIA)][["varname"]], n)

  # add forced variables
  varnames <- unique(c(force_vars, varnames_criteria))

  ## if there are forced groups passed with force_vars_groups, add variables
  if (!is.null(force_vars_groups)){
    assertthat::assert_that(is.list(force_vars_groups), msg = "force_vars_groups must be a list (with variables names in slots)")
    assertthat::assert_that(length(force_vars_groups)>1, msg = "force_vars_groups list size mut be greater or equal to 2")

    if (is.null(names(force_vars_groups))) names(force_vars_groups) <- paste0("G", 1:length(force_vars_groups))


    df_groups <- data.table(
      group = rep(
        names(force_vars_groups),
        times = sapply(force_vars_groups, length)),
      var = unlist(force_vars_groups)
    )

    assertthat::assert_that(any(df_groups$var %in% tmp$varname), msg = "No variable provided is in targeter object")

    tmp <- merge(tmp, df_groups, by.x = "varname", by.y ="var", all.x = TRUE, all.y = FALSE)

    in_scope <- tmp[!is.na(group)]
    selected <- sapply(
      split(in_scope, by = "group"),
      function(grp) utils::head(grp[["varname"]], force_vars_groups_n))

    # add forced variables introduced per subgroup
    varnames <- unique(c(varnames,selected))


  }

  # print(varnames)

  ## filter the object
  sub <- x$profiles[which(names(x$profiles) %in% varnames)]

  out <- x
  out$profiles <- sub

  return(out)
}
