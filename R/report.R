#' Function report()
#'
#'This function creates an automatic report according to a predefined template in the package or a user-generated template.
#'
#' @param object an object of calss "targeter".
#' @param template template file path. By default, it put to NULL and take the default template.
#' @param summary_object an object of class "summary.targeter": pre-computed summary of object.
#' @param browse by default TRUE : print in the browse the output.
#' @param ntop integer - number of variables to show in the report. For more information, see the function top. By default, NULL i.e. all variables are taken.
#' @param nmin integer - minimum number of profiles in the seleected class. For more information, see the function top.
#' @param criteria character - By default, the value is index.max.index. For more information, see the function top.
#' @param min_criteria - By default NULL. If it's speciefied, only the observations which have criteria >= min_criteria.
#' @param metadata data.frame - if metadata is  loaded in R environment, label of the variables can be used. Default value (NULL) corresponds to no metadata available.
#' The label will be used for the title and the x-axis of the graph.
#' @param force_vars - character, list of variables that will be kept and then won't never be filterered.
#' @param  output_format - By default it's html. For a pdf, output_format="pdf_document" and for a word, output_format="word_document".
#' @param ... other parameter from the function render.
#' @return A report in pdf, html or Word.
#' @export report
#'
#' @seealso
#' \itemize{
#' \item \code{\link{targeter}}
#' \item \code{\link{summary.targeter}}
#' \item \code{\link{top}}
#' }
#' @examples
#' \dontrun{
#' t <- targeter(adult,target ="ABOVE50K",analysis_name="Analyse",naming_conventions=FALSE)
#' report(t,output_format="pdf_document")
#' report(ntop=5)
#' }
report <- function(object,
                   template=NULL,
                   summary_object=NULL,
                   browse=FALSE,
                   ntop=NULL,
                   nmin=20,
                   criteria=c("IV"),
                   min_criteria = NULL,
                   metadata=NULL,
                   force_vars=character(),
                   output_format=c("html","pdf","word"),
                   output_file = NULL,
                   output_dir = tempdir(),
                   ...){


  knitr::knit_meta(clean=TRUE) ## https://github.com/rstudio/rstudio/issues/2319

  ##test
  assertthat::assert_that(inherits(object, "targeter"), msg ="The class of object must to be 'targeter'.")
  assertthat::assert_that(inherits(nmin,"numeric")|inherits(nmin,"integer"), msg="The parameter nmin must to be an integer or numeric")
  assertthat::assert_that(inherits(ntop,"numeric")|inherits(ntop,"integer")|is.null(ntop), msg="The parameter ntop must to be an integer or numeric")
  assertthat::assert_that(inherits(metadata,"data.frame")| is.null(metadata), msg = "The parameter metadata must be either NULL (no metadata) or a data.frame")
  assertthat::assert_that(inherits(min_criteria,"numeric")|inherits(min_criteria,"integer")|is.null(min_criteria), msg="The parameter min_criteria must to be an integer or numeric")


  output_format <- match.arg(output_format, c("html","pdf","word"), several.ok = FALSE)
  ## apply focus/top
  if (is.null(summary_object)){
    # compute summary object
    cat("?")
    summary_object <- summary(object, nmin = nmin, criteria = criteria,
                              min_criteria = min_criteria)
  }
  if(!is.null(ntop)){
    object <- focus(object, n = ntop, force_vars = force_vars,
                    summary_object = summary_object )}
  attr(object, 'metadata') <- metadata

  if (is.null(template)){
    ## default template
    template <- file.path(path.package("targeter"), "ressources",paste0("report_template_",object$target_type,".Rmd"))
  }

  format_tables <- switch(output_format,
                          "html"="html",
                          "word"="word",
                          "pdf"="latex")

  outfile <- rmarkdown::render(template,
                               output_format=paste(output_format,'document',sep="_"),
                               output_file=output_file,output_dir=output_dir,...)
  if (browse) utils::browseURL(outfile)
  return(outfile)
}


# A <- targeter(adult, target="ABOVE50K",binning_method='clustering',woe_post_cluster=TRUE)
# A <- targeter(adult, target="ABOVE50K",binning_method='smart',woe_post_cluster=TRUE)

# N <- targeter(adult, target="AGE",binning_method='smart',woe_post_cluster=TRUE)

## html
# report(A,  browse=TRUE, template=file.path("./inst/ressources/report_template_target_binary.Rmd"), output_format='html')

# report(A,  browse=TRUE, template="./inst/ressources/report_template_target_binary.Rmd", output_format='html')
# report(N,  browse=TRUE, template="./inst/ressources/report_template_target_numeric.Rmd", output_format='html')

## pdf
# report(A,  browse=TRUE, template="./inst/ressources/report_template_target_binary.Rmd", output_format='pdf')

## word
# report(A,  browse=TRUE, template=file.path("./inst/ressources/report_template_target_binary.Rmd"), output_format='word')
