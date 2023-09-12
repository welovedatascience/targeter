#' Check on the naming convention
#'
#' This function checks that the naming convention used by WeLoveDataScience
#'  are respected for each column.
#'
#' @param data data.table or data.frame
#' @param test_target_type also check if any variable that should be a target
#' through naming conventions (ie Z_) also has a target nature provided in name.
#'
#' @return a table which contains two columns "ErrorType" and "ErrorTarget".
#' Each row corresponds to a variable of the data.
#' The column "ErrorType" == FALSE when the condition on the first letter is
#' respected.
#' The column "ErrorTarget" == FALSE when the condition on BIN/INT/ORD/CAT on
#' the target is respected
#' @export
#'
check_naming_conventions <- function(data, test_target_type = FALSE) {
  ## perform tests on colnames : respect on the naming convention

  ## select colnames
  varnames <- colnames(data)

  ## definition of the result
  out <- as.data.frame(matrix(NA, nrow = length(varnames), ncol = 2))
  colnames(out) <-
    c(
      "ErrorType",
      "ErrorTarget"
    )
  rownames(out) <- varnames

  for (i in seq_along(varnames)) {
    ivar <- varnames[[i]]

    ## we count the number of words separated by "_"
    splitw <- strsplit(ivar, "_")[[1]]
    nb_words <- length(splitw)

    ## First test : we test the first letter !
    type <- toupper(substr(ivar, 1, 2))
    cl <- class(data[[ivar]][1])
    if (
      ((type %in% c("N_", "M_", "F_", "R_", "P_", "Y_", "J_")) & (cl %in% c("numeric", "integer"))) |                   #nolint
        ((type %in% c("D_", "S_")) & (cl %in% c("Date", "POSIXct", "POSIXlt"))) |                                       #nolint
        ((type %in% c("C_", "T_", "L_")) & (cl %in% c("factor", "character", "logical", "integer", "numeric"))) |       #nolint
        ((type %in% c("E_", "I_", "A_", "O_", "X_", "Z_", "K_", "W_", "Q_")))
    ) {
      out[ivar, "ErrorType"] <- FALSE
    } else {
      out[ivar, "ErrorType"] <- TRUE
    }

    # test on the target type
    if (test_target_type & type == "Z") {
      target_type <- splitw[nb_words] # lastelement
      out[ivar, "ErrorTarget"] <- target_type %in% c("BIN", "INT", "ORD", "CAT")
    }
  }

  return(out)
}
