#' Create a presentation from a targeter report object
#'
#' This function generates a presentation (slides), currently targeting
#' powerpoint (other formats to come).
#'
#' @param object - an object of class targeter
#' @param metadata - not used currently
#' @param format - character, one of "pptx", "revealjs"
#' @param template - character. path to a Quarto qmd document that will be used
#' to generate presentation. If NULL (default), we will use targeter package
#' default template.
#' @param pptx_reference_doc - character. Path to a default PPTX template 
#' that will be used as Q"uarto reference-doc
#' If NULL (default), we will use targeter
#' package default templates for PPTX. 
#' If empty string "", we won't use any template.
#' @param revealjs_template - character. Path to a default revealjs
#'  template If empty string (default), no revealjs template will be used.
#' @param output_dir - character. Output directory - default to working
#' directory
#' @param output_file - character, name of output file (with extension). if NULL
#' (default), slidify will generate a temporary name.
#' @param tmpdir - path to a temporarry folder tp handle quarto rendering, 
#' By default, a call to `tempdir()` is performed.
#' @param delete_temporary_files - logical. Whether we delete temporary files or
#' not once presentation  is generated. Default: TRUE. On might want to use
#' FALSE for debugging purpose.
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
#' @param ... - not yet used.
#' @return inviisibly returns path to the generated presentation
#' @export slidify
#' @importFrom assertthat assert_that
#' @importFrom quarto quarto_render

#' @examples
#' \dontrun{
#' tar <- targeter(adult, target ="ABOVE50K", analysis_name="Analyse",
#' naming_conventions=FALSE)
#' slidfy(tar)
#' }

slidify <- function(
  object,
  metadata = NULL,
  format = "pptx",
  template = NULL, # default QMD template for slides
  pptx_reference_doc = NULL, # for pptx format, default template,
  revealjs_template = "", # <TODO> prepare a revealjs template
  output_dir = ".",
  output_file = NULL,
  tmpdir = tempdir(), # tmpdir = "/hackdata/share/_tmp"
  delete_temporary_files = TRUE,
  title = object$analysis,
  author = getOption("targeter.author", "welovedatascience targeter"),
  fullplot_which_plot = "1:2",
  tables = FALSE,
  logo = NULL,
  ...
) {
  
  ## handled default values 
  default_pptx_template <- is.null(pptx_reference_doc)
  if (is.null(pptx_reference_doc)) {
    pptx_reference_doc <- file.path(
      find.package("targeter", lib.loc = .libPaths()),
      "ressources",
      "slidify-pptx-targeter-template.pptx"
    )
  }



  default_template <- is.null(template)
  if (is.null(template)) {
    template <- file.path(
      find.package("targeter", lib.loc = .libPaths()),
      "ressources",
      "slidify-template.qmd"
    )
  }
  if (is.null(logo)){
    if (default_template)
    logo <- file.path(
      find.package("targeter", lib.loc = .libPaths()),
      "ressources",
      "wlds-logo.png"
    )
  }
  assertthat::assert_that(
    inherits(object, "targeter"),
    msg = "The class of object must to be 'targeter'."
  )
  assertthat::assert_that(
    inherits(metadata, "data.frame") | is.null(metadata),
    msg = "The parameter metadata must be either NULL (no metadata) or a data.frame"
  )
  if (format == "pptx") {
    assertthat::assert_that(
      is.character(pptx_template),
      msg = "pptx template must be a character string giving the path of a powerpoint template"
    )
    assertthat::assert_that(
      file.exists(pptx_template),
      msg = "powerpoint template provided file does not exist"
    )
  }
  assertthat::assert_that(
    file.exists(template),
    msg = "template file does not exist"
  )

  
  temp <- basename(tempfile(pattern = "targeter-quarto-tmp"))
  dir.create(file.path(tmpdir, temp))
  tmpdir <- file.path(tmpdir, temp)
  file_extension <- switch(format, pptx = "pptx", revealjs = "html")
  tmp_file_outfile <- paste(temp, file_extension, sep = ".")

  # save tmp  object in tmpdir
  tmp_file_object <- paste(temp, "tar.qs", sep = ".")
  qs::qsave(object, file.path(tmpdir, tmp_file_object))

  # prepare tmp_quarto
  tmp_file_quarto <- paste(temp, "qmd", sep = ".")

  if (format == "pptx"){
    # if default template we will also use powerpoint wlds template (or override
    # with user provided one)
    if (default_template | !default_pptx_template) {
      #pptx_template <- "targeter-report.pptx"
      tmp_pptx_reference_doc <- paste(paste0(temp, "-template"), "pptx", sep = ".")
      file.copy(
        from = pptx_reference_doc,
        to = file.path(tmpdir, tmp_pptx_reference_doc),
        overwrite = TRUE
      )

      quarto_template <- paste(readLines(template), collapse = "\n")
      quarto_template <- gsub(
        "$TARGETER_PARAM_template-pptx$",
        tmp_pptx_reference_doc,
        quarto_template,
        fixed = TRUE
      )
      cat(
        quarto_template,
        file = file.path(tmpdir, tmp_file_quarto),
        append = FALSE
      )
    } else {
      file.copy(template, file.path(tmpdir, tmp_file_quarto))
    }
  }  else if (format == "revealjs") {
    
    if (revealjs_template != ""){
      file.copy(
        from = revealjs_template,
        to = file.path(tmpdir, basename(revealjs_template)),
        overwrite = TRUE
      )
    }
      quarto_template <- paste(readLines(template), collapse = "\n")
      quarto_template <- gsub(
        "$TARGETER_PARAM_template_revealjs$",
        basename(revealjs_template),
        quarto_template,
        fixed = TRUE
      )
      cat(
        quarto_template,
        file = file.path(tmpdir, tmp_file_quarto),
        append = FALSE
      )
    }

      
  } else {
    file.copy(template, file.path(tmpdir, tmp_file_quarto))
  }
  
  meta_yml_params <- list(
    object = tmp_file_object,
    fullplot_which_plot = fullplot_which_plot,
    title = title,
    author = author,
    tables = as.character(tables)
  )

  
  quarto::quarto_render(
    input = file.path(tmpdir, tmp_file_quarto),
    output_file = tmp_file_outfile,
    execute_dir = tmpdir,
    execute_params = meta_yml_params,
    debug = TRUE,
    output_format = format
  )

  if (is.null(output_file)) {

    output_file <- paste0("targeter-report-", 
    format(Sys.time(), format = "%Y-%m-%d_%H%M%S"),
    ".", file_extension)
    
  }
  if (file.exists(tmp_file_outfile)) {
    file.copy(
      from = tmp_file_outfile,
      to = file.path(output_dir, output_file),
      overwrite = TRUE
    )
    file.remove(tmp_file_outfile)
  }

  if (delete_temporary_files) {
    # https://stackoverflow.com/questions/9296377/automatically-delete-files-folders
      unlink(tmpdir, recursive = TRUE, force = TRUE)

  }
  cat("\nPresentation generated in file:.", file.path(output_dir, output_file),"\n")
  invisible(file.path(output_dir, output_file))
}

# temp <- basename(tempfile(pattern = "targeter-quarto-tmp"))
# file_objec
# t <-paste(temp, ".tar.qs", sep=".")
# qs::qsave(object, file_object)

# file_yaml <- write_quarto_yaml(tarfocus, outfile = paste(temp, "yml", sep="."))
# file_quarto <- paste(temp, "qmd", sep=".")

# system(command = paste("cat", file_yaml,quarto_template, ">", file_quarto))
# system(command = paste0("quarto render ", basename(file_quarto)," --output ",outfile," -P object:",basename(file_object)

# }

# library(data.table)
# data(adult)
# object <- targeter(adult, target = "ABOVE50K")
# qs::qsave(object, "tmp/object.qs")
# slidify(object, delete_temporary_files = TRUE)

