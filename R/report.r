#' @title report
#' @description This function creates an automatic report according to a
#'  predefined template in the package or a user-generated template.
#' Create a report from a targeter report object
#' 
#' @param object - an object of class targeter
#' @param summary_object an object of class "summary.targeter": pre-computed
#' summary of object.
#' @param metadata - data.frame - if metadata is  loaded in R environment,
#' label of the variables can be used. Default value (NULL) corresponds to
#' no metadata available.
#' The label will be used for the title and the x-axis of the graph.
#' @param format - Target output format (defaults to "html").
#'  The option "all" will render all formats.
#' @param nmax - integer: maximum number of variables to be used. Default 100. 
#' use NULL to force all variables to be present in report.See notes.
#' @param template - character. path to a Quarto qmd document that will be used
#' to generate presentation. If NULL (default), we will use targeter package
#' default template.
#' @param output_dir - character. Output directory - default to create a 
#' new dedicated directory in working folder. 
#' @param output_file - character, name of output file (without file extension).
#' default value: index 
#' @param title - character, title for the presentation. Overide default title
#' that consists in taking title for targeter object 'analysis' slot.
#' @param author - character: presentatiopn author.
#' @param fullplot_which_plot - character, selection of plots (cf `fullplot`
#' documentation) 
#' @param tables - logical whetheer to print tables in seprate slides in 
#' addition of graphics. Might ot produce nice outputs as tables are usually
#' large.
#' @param logo - character: path to a logo file. If NULL (default), we will take
#' package welovedatascience logo. If empty, no logo will be used. Only used
#' by revealjs format.
#' @param pptx_reference_doc - character. Path to a default PPTX template 
#' that will be used as Quarto reference-doc for pppx formzt
#' If NULL (default), we will use targeter
#' package default templates for PPTX. 
#' If empty string "", we won't use any template.
#' @param render - logical. If TRUE (default), the report will be rendered
#'  and saved in the output_dir. If FALSE, the report will be generated but not
#'  rendered. Useful for debugging.
#' @param debug - logical. If TRUE, the function will print debug information (passed to Quarto call).
#' @param ...  additional parameters to be passed to quarto, for instance
#' allowing to pass/overwite any YAML set-up
#' @return invisibly returns path to the generated specific file (unique format)
#' or folder (several formats)
#' @export report
#' 
#' @seealso
#' \itemize{
#' \item \code{\link{targeter}}
#' \item \code{\link{summary.targeter}}
#' \item \code{\link{focus}}
#' }
#' @importFrom assertthat assert_that
## ' @importFrom quarto quarto_render
## ' @importFrom qs qsave
#' @importFrom pacman p_load
#' @examples
#' \dontrun{
#' tar <- targeter(adult, target ="ABOVE50K", analysis_name="Analyse",
#' naming_conventions=FALSE)
#' slidify(tar, format = "pptx", author = "this is me")
#' }

report <- function(
  object,
  summary_object = NULL,
  metadata = NULL,
  format = "html",
  nmax = 100, #todo implement 
  template = NULL, # default QMD template for slides
  pptx_reference_doc = NULL, # for pptx format, default template,
  # revealjs_template = "", # todo prepare a revealjs template
  output_dir = paste0("targeter-report", format(Sys.time(), format = "%Y-%m-%d_%H%M%S")),
  output_file = "index",
  title = object$analysis,
  author = getOption("targeter.author", "welovedatascience targeter"),
  fullplot_which_plot = "1:2",
  tables = FALSE,
  brandfile = getOption("targeter.brandfile", file.path(
      find.package("targeter", lib.loc = .libPaths()),
      "ressources",
      "_brand.yml"
    )),
  logo = NULL,
  debug = FALSE,
  render = TRUE,
  ... # additional parameters to be passed to quarto
) {
  
  assertthat::assert_that(pacman::p_load("quarto"), msg="quarto package and runtime are required.")

  # todo cover all parameters  assert tests 
  assertthat::assert_that(is.character(format), msg = "format must be a character")
  # format <- match.arg(format, c(""pptx","revealjs","beamer"), several.ok = FALSE)
  

  assertthat::assert_that(
    inherits(object, "targeter"),
    msg = "The class of object must to be 'targeter'."
  )
  assertthat::assert_that(
    inherits(metadata, "data.frame") | is.null(metadata),
    msg = "The parameter metadata must be either NULL (no metadata) or a data.frame"
  )
  if (format == "pptx" & !is.null(pptx_reference_doc)) {
    assertthat::assert_that(
      is.character(pptx_reference_doc),
      msg = "pptx template must be a character string giving the path of a powerpoint template"
    )
    assertthat::assert_that(
      file.exists(pptx_reference_doc),
      msg = "powerpoint template provided file does not exist"
    )
  }
  

  assertthat::assert_that(is.character(brandfile) & file.exists(brandfile), msg = "brandfile must be a character string giving the path of a brandfile")
  assertthat::assert_that(is.character(output_dir), msg = "output_dir must be a character string giving the path of the output directory")
  

  ## handled default values 
  default_pptx_template <- is.null(pptx_reference_doc)
  if (is.null(pptx_reference_doc)) {
    pptx_reference_doc <- file.path(
      find.package("targeter", lib.loc = .libPaths()),
      "ressources",
      "slidify-pptx-targeter-template.pptx"
    )
  }
  # cat(pptx_reference_doc)
  if (is.null(logo)){
    logo <- file.path(
      find.package("targeter", lib.loc = .libPaths()),
      "ressources",
      "wlds-logo.png"
    )
  }

  default_template <- is.null(template)
  if (is.null(template)) {
    template <- file.path(
      find.package("targeter", lib.loc = .libPaths()),
      "ressources",
      "report-template.qmd"
    )
  }

  assertthat::assert_that(
    file.exists(template),
    msg = "template file does not exist"
  )



  dir.create(output_dir, showWarnings = FALSE)

  
  attr(object, "metadata") <- metadata
  attr(object, "summary_oject") <- summary_object
  
  saveRDS(object,  file.path(output_dir,"tar.qs"))
  file.copy(from= template, to = file.path(output_dir, paste(output_file,"qmd", sep=".")), overwrite = TRUE)

  # pptx template
  if (format %in% c("pptx","all")){
    # if default template we will also use powerpoint wlds template (or override
    # with user provided one)
    has_pptx_template <- FALSE
    if (default_template | !default_pptx_template) {
      #pptx_template <- "targeter-report.pptx"
      tmp_pptx_reference_doc <- "pptx-template-reference.pptx"
      file.copy(
        from = pptx_reference_doc,
        to = file.path(output_dir, tmp_pptx_reference_doc),
        overwrite = TRUE
      )
      has_pptx_template <- TRUE
      pptx_reference_doc <- tmp_pptx_reference_doc
    }
  }

  file.copy(from = brandfile, to = file.path(output_dir, "_brand.yml"), overwrite = TRUE)

  meta_yml_params <- list(
    object = "tar.qs",
    fullplot_which_plot = fullplot_which_plot,
    title = title,
    author = author,
    tables = as.character(tables)
  )

  if (render){
    quarto::quarto_render(
      input = output_dir,
      execute_params = meta_yml_params,
      debug = debug,
      output_format = format, as_job = FALSE 
    )1
    cat("\nPresentation generated in folder:.", output_dir ,"\n")
    invisible(file.path(output_dir, output_file))

  }
  invisible(TRUE)

}

# library(data.table)
# data(adult)
# object <- targeter(adult, target = "ABOVE50K")
# qs::qsave(object, "tmp/object.qs")
# slidify(object, delete_temporary_files = TRUE)
# slidify(object, delete_temporary_files = TRUE, format="revealjs", tables=TRUE, fullplot_which_plot = "1:4")
# slidify(object, delete_temporary_files = FALSE, tables=TRUE, format = "beamer")
