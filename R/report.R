#' Function report()
#'
#'This function creates an automatic report according to a predefined template in the package or a user-generated template.
#'
#' @param object an object of calss "callCrossvar".
#' @param template template file path. By default, it put to NULL and take the default template.
#' @param browse by default TRUE : print in the browse the output.
#' @param ntop integer - number of variables to show in the report. For more information, see the function top. By default, NULL i.e. all variables are taken.
#' @param nmin integer - minimum number of profiles in the seleected class. For more information, see the function top.
#' @param criteria character - By default, the value is index.max.index. For more information, see the function top.
#' @param min_criteria - By default NULL. If it's speciefied, only the observations which have criteria >= min_criteria.
#' @param metadata data.frame - if metadata is  loaded in R environment, label of the variables can be used. Default value (NULL) corresponds to no metadata available.
#' The label will be used for the title and the x-axis of the graph.
#' @param  output_format - By default it's html. For a pdf, output_format="pdf_document" and for a word, output_format="word_document".
#' @param ... other parameter from the function render.
#' @return A report in pdf, html or Word.
#' @export generateReport
#'
#' @seealso
#' \itemize{
#' \item \code{\link{callCrossvar}}
#' \item \code{\link{summary.callCrossvar}}
#' \item \code{\link{top}}
#' }
#' @examples
#' t <- targeter(adult,target ="ABOVE50K",analysis_name="Analyse",naming_conventions=FALSE)
#' report(t,output_format="pdf_document")
#' report(ntop=5)
report <- function(object,
                           template=NULL,
                           summary_object=NULL,
                           browse=TRUE,
                           ntop=NULL,
                           nmin=20,
                           criteria=c( "index.max.index"),
                           min_criteria = NULL,
                           metadata=NULL,
                           force_vars=character(),
                           output_format="pdf_document",
                           ...){


  knitr::knit_meta(clean=TRUE) ## https://github.com/rstudio/rstudio/issues/2319

  ##test
  assertthat::assert_that(inherits(object, "targeter"), msg ="The class of object must to be 'targeter'.")
  assertthat::assert_that(inherits(nmin,"numeric")|inherits(nmin,"integer"), msg="The parameter nmin must to be an integer or numeric")
  assertthat::assert_that(inherits(ntop,"numeric")|inherits(ntop,"integer")|is.null(ntop), msg="The parameter ntop must to be an integer or numeric")
  assertthat::assert_that(inherits(metadata,"data.frame")| is.null(metadata), msg = "The parameter metadata must be either NULL (no metadata) or a data.frame")
  assertthat::assert_that(inherits(min_criteria,"numeric")|inherits(min_criteria,"integer")|is.null(min_criteria), msg="The parameter min_criteria must to be an integer or numeric")

  ## apply focus/top
  if(!is.null(ntop)){
    object <- focus(object, n = ntop, nmin = nmin, criteria = criteria,
                            min_criteria = min_criteria, force_vars = force_vars,
                            summary_object = summary_object )}
  attr(object, 'metadata') <- metadata

  if (is.null(template)){
    ## default template
    template <- file.path(path.package("targeter"), "ressources",paste0("report_template_",object$target_type,".Rmd"))
  }

  format_tables <- switch(output_format,
                          "html_document"="html",
                          "pdf_document"="latex")

  outfile <- rmarkdown::render(template,output_format=output_format,...)
  if (browse) browseURL(outfile)
  return(outfile)
}
