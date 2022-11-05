#' Add label to a var
#'
#' This functions allows to add a label to var. It's useful for the graphics or the reports.
#' Obviously, this function can be applied only if a metadata exists
#'
#' @param var character - the name of the var(s) that we want to remplace by the label, could be a vector
#' @param metadata data.frame - if we have a metadata loaded in your R, you can use to take the label of the vars.
#' @param var_field - character: name of the column in metadata that contains variable name (default: 'var')
#' @param label_field - character: name of the column in metadata that contains variable label (default: 'LABEL')
#' @param lang  character - the code for the language. It's useful if you have a metadata with different labels for differents languages.
#' By default, the value is NULL.
#' @return a label
#' @export label
#'
#' @examples
#' \dontrun{
#' label(adult$EDUCATIONNUM, metadata=mymetadata) # assumes mymetadata file exists
#' }
label <- function(var, metadata, var_field="var", label_field="LABEL",lang=NULL){
  ##test

  if (is.null(metadata)) return(var)
  assertthat::assert_that(inherits(metadata,"data.frame"), msg = "The parameter metadata must be a data.frame or data.table")

  metadata <- as.data.frame(metadata)
  rownames(metadata) <- metadata[[var_field]]
  assertthat::assert_that(inherits(var,"character"), msg = "var must be character")
  assertthat::assert_that(length(var)==1,msg="Only one var is admitted")

  assertthat::assert_that(var_field %in% names(metadata), msg = paste("var_field ",var_field,"not present in metadata"))

  ##add language code
  if (!is.null(lang)) label_field <- paste(label_field,lang, sep='_')
  #
  assertthat::assert_that(label_field %in% names(metadata), msg = paste("label_field ",label_field," not present in metadata"))

  ##take the label in the metadata; if not label, keep var name
  var_hasLabel <- var %in% metadata[[var_field]]
  #labels <- sapply(var, function(var) metadata[which(metadata[[var_field]]==var),][[label_field]])
  labels <- metadata[var,][[label_field]]

  labels[!var_hasLabel] <- var[!var_hasLabel]
  labels <- unlist(labels)
  return(labels)

}
