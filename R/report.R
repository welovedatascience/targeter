# to prevent checks of data.table used variables
# see:  ?globalVariables


if(getRversion() >= "3.1.0") utils::globalVariables(
  c(".", ".N", ":=", "vcount", "vsum", "WOE", "vperc",
"cperc", "uniqueN", "..cn", "cn", "level", "value", "target", "bxp_min",
"q25", "q75", "bxp_max", "avg", "N", "cluster", "Y", "color", "varname",
"..select_vars", "percNA", "nNA", "count", "perc", "qrange", "<<-", "X")
)

#' @title report
#' @description This function creates an automatic report according to a predefined template in the package or a user-generated template.
#'
#' @param object an object of calss "targeter".
#' @param template template file path. By default, it put to NULL and take the default template.
#' @param summary_object an object of class "summary.targeter": pre-computed summary of object.
#' @param browse by default TRUE : print in the browse the output.
#' @param ntop integer - number of variables to show in the report. For more information, see the function focus By default, NULL i.e. all variables are taken.
#' @param nmin integer - minimum number of profiles in the selected class. For more information, see the function focus.
#' @param criteria character - By default, the value is index.max.index. For more information, see the function focus.
#' @param min_criteria - By default NULL. If it's specified, only the observations which have criteria >= min_criteria.
#' @param metadata data.frame - if metadata is  loaded in R environment, label of the variables can be used. Default value (NULL) corresponds to no metadata available.
#' The label will be used for the title and the x-axis of the graph.
#' @param force_vars - character, list of variables that will be kept and then won't never be filtered.
#' @param  output_format - By default it's html. For a pdf, output_format="pdf_document" and for a word, output_format="word_document".
#' @param output_file - name of the output file to be generated. If NULL (default) an, automated name will be generated.
#' @param output_dir - output directory, default: tempdir().
#' @param ... other parameter from the function render.
#' @return path to the generated report. Side effect would be to open a document, if \code{browse} is TRUE.
#' @export report
#'
#' @seealso
#' \itemize{
#' \item \code{\link{targeter}}
#' \item \code{\link{summary.targeter}}
#' \item \code{\link{focus}}
#' }
#' @examples
#' \dontrun{
#' t <- targeter(adult,target ="ABOVE50K",analysis_name="Analyse",naming_conventions=FALSE)
#' report(t,output_format="pdf_document")
#' report(ntop=5)
#' }
#' @importFrom utils head
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

    summary_object <- summary(object, nmin = nmin, criteria = criteria,
                              min_criteria = min_criteria)
  }
  if(!is.null(ntop)){
    object <- focus(object, n = ntop, force_vars = force_vars,
                    summary_object = summary_object )

  }

  summary_object<- summary_object[varname %in% names(object$profiles)]
  # attr(object, 'metadata') <- metadata

  if (is.null(template)){
    ## default template
    template <- file.path(find.package("targeter", lib.loc=.libPaths()), "ressources",paste0("report_template_target_",object$target_type,".Rmd"))
  }

  format_tables <- switch(output_format,
                          "html"="html",
                          "word"="word",
                          "pdf"="latex")

  print(utils::head(metadata,2))
  print(utils::head(summary_object,2))


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
