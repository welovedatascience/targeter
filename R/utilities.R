


# to prevent checks of data.table used variables
# see:  ?globalVariables
if (getRversion() >= "3.1.0") {
  utils::globalVariables(c("varsum", "std", "positions",
  "L_TARGET","osVersion","packageVersion"))
}

#' @title label: add label to a var
#'
#' @description This functions allows to add a label to var. It's useful for the
#'  graphics or the reports.
#' Obviously, this function can be applied only if a metadata exists
#'
#' @param var character - the name of the var(s) that we want to replace by the
#' label, could be a vector
#' @param metadata data.frame - if we have a metadata loaded in your R, you can
#' use to take the label of the vars.
#' @param var_field - character: name of the column in metadata that contains
#' variable name (default: 'var')
#' @param label_field - character: name of the column in metadata that contains
#' variable label (default: 'LABEL')
#' @param include_varname - logical. If returned string will also include 
#' variables names within brackets after label for variables that has label.
#' (default: FALSE).
#' 
#' @return a label
#' @export label
#'
#' @examples
#' \dontrun{
#' label(adult$EDUCATIONNUM, metadata=mymetadata) # assumes mymetadata dataframe exists
#' }

label <- function(
  var,
  metadata,
  var_field = "var",
  label_field = "LABEL",
  include_varname = FALSE
) {
  ##test

  if (is.null(metadata)) return(var)
  assertthat::assert_that(
    inherits(metadata, "data.frame"),
    msg = "The parameter metadata must be a data.frame or data.table"
  )

  metadata <- as.data.frame(metadata) # in case of data tabke so gthat we can use rownames
  
  rownames(metadata) <- metadata[[var_field]]
  assertthat::assert_that(
    inherits(var, "character"),
    msg = "var must be character"
  )

  # assertthat::assert_that(length(var)==1,msg="Only one var is admitted")

  assertthat::assert_that(
    var_field %in% names(metadata),
    msg = paste("var_field ", var_field, "not present in metadata")
  )

  #
  assertthat::assert_that(
    label_field %in% names(metadata),
    msg = paste("label_field ", label_field, " not present in metadata")
  )

  
  ##take the label in the metadata; if not label, keep var name
  var_hasLabel <- var %in% metadata[[var_field]]
  #labels <- sapply(var, function(var) metadata[which(metadata[[var_field]]==var),][[label_field]])
  labels <- metadata[var, ][[label_field]]

  labels[!var_hasLabel] <- var[!var_hasLabel]
  if (include_varname){
    labels[var_hasLabel] <- paste0(labels[var_hasLabel], " [",var[var_hasLabel], "]")
  }
  labels[which(is.na(labels))] <- var[which(is.na(labels))]

  labels <- unlist(labels)
  return(labels)
}

#' @title table_crossvar: build a meaningful table from a crossvar object.
#'
#' @description This functions builds a dataframe that gather most relevant
#' information from a crossvar object. Useful in reports, associated with
#' kableExtra or other tables output generating engine.
#'
#' @param x crossvar - crossvar object (stored in targeter$profiles slot).
#' @param round_digits integer - number of digits to rounds some values.
#' @param counts_and - character - output will always contain counts, we can
#' add row percentages and column percentage for target (both always by default)
#' @return a data.frame
#' @export table_crossvar
#'
#' @examples
#' tar <- targeter(adult, target = 'ABOVE50K')
#' table_crossvar(tar$profiles[['EDUCATION']])
#'
#' @importFrom data.table setDT
#' @importFrom utils globalVariables 

# to prevent checks of data.table used variables
# see:  ?globalVariables

table_crossvar <- function(
  x,
  round_digits = 3,
  counts_and = c("props", "target%")
) {
  if (x$target_type %in% c("binary", "categorical")) {
    cnts <- as.data.frame.matrix(x$counts)
    vn <- colnames(cnts)
    colnames(cnts) <- paste0("N", vn)
    cnts[, "Ntot"] <- apply(cnts, 1, sum)

    props <- as.data.frame.matrix(x$props)
    rn <- rownames(props)
    props <- as.data.frame(
      lapply(100 * props, function(col) {
        paste0(round(col, round_digits), "%")
      })
    )
    colnames(props) <- paste0("perc", vn)
    pcol <- as.data.frame.matrix(x$pcol)
    # names(pcol) <- c("colper0","colperc1")
    # restrict to target
    pcol <- pcol[,
      which(names(pcol) %in% as.character(x$target_reference_level)),
      drop = FALSE
    ]
    colnames(pcol) <- "target%"
    pcol[, 1] <- paste0(round(pcol[, 1] * 100, round_digits), "%")
    out <- cnts
    if ("props" %in% counts_and) {
      out <- cbind(out, props)
    }

    if ("target%" %in% counts_and) {
      out <- cbind(out, pcol)
    }
  } else if (x$target_type %in% c("numeric")) {
    if (!is.null(x$woe)) {
      x$stats <- cbind(x$stats, x$woe)
    }
    out <- x$stats
    rn <- rownames(out)
    data.table::setDT(out)
    out[, varsum := NULL]
    out[, avg := round(avg, 4)]
    out[, std := round(std, 4)]
    out[, bxp_min := NULL]
    out[, bxp_max := NULL]
    out <- as.data.frame(out)
    rownames(out) <- rn
  }
  return(out)
}


#' @importFrom utils osVersion packageVersion 
targeter_session_info <- function() {
  list(
    R_version = R.Version()$version.string,
    os = osVersion,
    targeter_package_version = packageVersion("targeter")
  )
}






tar_foldername <- function(object){

  assertthat::assert_that(
    inherits(object, "targeter") ||
    inherits(object, "tartree") ,
    msg = "Function designed for targeter package objects"
    )
  
  current_date <-  format(Sys.Date())
  

  if (inherits(object, "targeter")){
    analysis <- object$analysis
    txt <- "-targeter-report"
  } else  if (inherits(object, "tartree")){

    analysis <- attr(object,"tar_object")$analysis
    txt <- "-targeter-tree"
  }

  analysis <- basename(tempfile(pattern="report-"))

  # analysis <- tolower(make.names(analysis))
  # analysis <- gsub(".", "-", analysis, fixed=TRUE)

  folder <- paste0(current_date, txt,"-", analysis)
  return(folder)
}
