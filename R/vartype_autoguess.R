dt_vartype_autoguess_onevar <- function(
  data,
  var,
  num_as_nominal_nval = 5,
  ...
) {
  assertthat::assert_that(
    inherits(data, "data.table"),
    msg = "data is not a data table"
  )
  if (inherits(data[, get(var)], c("logical"))) return("binary")
  if (inherits(data[, get(var)], c("ordered"))) return("ordinal")

  ncat <- data[, uniqueN(get(var))]
  if (inherits(data[, get(var)], c("factor", "character"))) {
    if (ncat == 2) return("binary") else if (ncat == 1) return("unimode") else
      return("categorical")
  }
  if (
    inherits(
      data[, get(var)],
      c("numeric", "integer", "Date", "POSIXct", "POSIXlt", "IDate")
    )
  ) {
    if (ncat == 2) return("binary") else if (ncat == 1)
      return("unimode") else if (ncat <= num_as_nominal_nval)
      return("categorical") else return("numeric")
  }
  return("unknown")
}

dt_vartype_autoguess <- function(data, num_as_nominal_nval = 5, ...) {
  # print("ok")
  # print(data)

  assertthat::assert_that(
    inherits(data, "data.table"),
    msg = "data is not a data table"
  )
  sapply(
    names(data),
    function(var)
      dt_vartype_autoguess_onevar(
        data,
        var,
        num_as_nominal_nval = num_as_nominal_nval,
        ...
      )
  )
}
