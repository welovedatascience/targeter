dt_vartype_autoguess <- function (data, num_as_nominal_nval = 5, ...) 
{
    assertthat::assert_that(inherits(data, "data.table"), msg = "data is not a data table")
    sapply(names(data), function(var) dt_vartype_autoguess_onevar(data, 
        var, num_as_nominal_nval = num_as_nominal_nval, ...))
}
