# to prevent checks of data.table used variables
# see:  ?globalVariables
if ( getRversion() >= "3.1.0") utils::globalVariables(
  c(".N", ":=", "vcount", "vsum", "WOE", "vperc", "cperc")
)

#' @importFrom data.table setnames
dt_WOE_IV <- function(data,
                      var_interest,
                      var_cross,
                      alternate_version = FALSE,
                      woe_shift = 0.01,
                      useNA = c("ifany", "no"),
                      target_reference_level = 1){
  ## parameters checks
  # if (!is.null(binary)){
  #   assertthat::assert_that(is.logical(binary),
  # msg= "binary must be logical")
  # }
  assertthat::assert_that(inherits(data, "data.frame"),
                          msg = "data must be a data frame or data table")
  data.table::setDT(data)
  nm <- colnames(data)
  assertthat::assert_that(var_interest %in% nm,
                          msg = "varinterest is not an existing 
                          variable in data")

  assertthat::assert_that(var_cross %in% nm,
                          msg = "varcount is not an existing variable in data")

  useNA <- match.arg(useNA, c("ifany", "no"), several.ok = FALSE)

  vRANGE <- data[, range(ifelse(get(var_interest)==target_reference_level,1,0), na.rm = TRUE)]
  # print(vRANGE)
  if (useNA == "no") data <- data[!is.na(var_cross),]

  agg <- data[, .(vcount = .N, 
   vsum = sum((ifelse(get(var_interest)==target_reference_level,1,0) - vRANGE[1]) / diff(vRANGE))), 
   by =c(var_cross)]
  data.table::setnames(agg, var_cross, 'variable')
  if (!alternate_version){
    # replace count with "non-events 0" or numeric "opposite
    agg[, vcount:=(vcount - vsum)]
  }

    agg[, `:=`(
      vperc=((vsum+woe_shift)/sum(vsum+woe_shift)),
      cperc=((vcount+woe_shift)/sum(vcount+woe_shift))
    )
    ]
  agg[,WOE:=log(vperc/cperc)]
  IV <- agg[, sum((vperc-cperc)*WOE)]
  return(list(WOE=agg, IV=IV))

}
