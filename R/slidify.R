#' Create a presentation from a targeter report object
#' 
#' This function generates a presentation (slides), currently targeting 
#' powerpoint (other formats to come).
#' 
#' @param object - an object of class targeter
#' @param format - character, default: pptx (currently the only format provided)
#' @param template - character. path to a Quarto qmd document that will be used 
#' to generate presentation. If NULL (default), we will use targeter package 
#' default template.
#' @param pptx_template - charcater. Path to a default PPTX that will be used
#' as Quarto->>PPTX generation. If NULL (default), we will use targeter
#' package default PPTX template.
#' @param generate_yaml - boolean, defauly TRUE. For targeter defaults template
#' we separated YAML header as we generate programmatically it (some values
#' in YAML depend on parameters)
#' @param output_dir - character. Output directory - default to working 
#' directory
#' @param output_file - character, name of output file (with extension). if NULL
#' (default), slidify will generate a temporary name.
#' @param metadata - not used currently
#' @param delete_temporary_files - logical. Whether we delete temporary files or
#' not once presentation  is generated. Default: TRUE. On might want to use 
#' FALSE for debugging purpose.
#' @return inviisibly returns path to the generated presentation
#' @export slidify
#' @importFrom assertthat assert_that

#' @examples
#' \dontrun{
#' tar <- targeter(adult, target ="ABOVE50K", analysis_name="Analyse",
#' naming_conventions=FALSE)
#' slidfy(tar)
#' }

slidify <- function(
  object,
  format = "pptx",
  template = NULL, # default template
  pptx_template = NULL, # for pptx format, default template
  generate_yaml = TRUE, # for targeter default template we do generate a YAML
  output_dir = ".",
  output_file = NULL,
  metadata = NULL,
  delete_temporary_files = TRUE,
  ...
) {
  ##test

  if (is.null(pptx_template)) {
    pptx_template <- file.path(
      find.package("targeter", lib.loc = .libPaths()),
      "ressources",
      "targeter-report.pptx"
    )
  }

  if (is.null(template)) {
    generate_yaml = TRUE
    template <- file.path(
      find.package("targeter", lib.loc = .libPaths()),
      "ressources",
      "pptx-quarto.qmd"
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
      is.character(template),
      msg = "template must be a character string giving the path of a powerpoint template"
    )
  }
  assertthat::assert_that(
    file.exists(template),
    msg = "template file does not exist"
  )

  write_quarto_yaml <- function(
    object,
    format = 'pptx',
    outfile = tempfile(fileext = ".yml"),
    author = getOption('targeter.author', 'welovedatascience targeter package'),
    pptx_template = file.path(
      find.package("targeter", lib.loc = .libPaths()),
      "ressources",
      "targeter-report.pptx"
    ),
    ...
  ) {
    assertthat::assert_that(
      inherits(object, "targeter"),
      msg = "Needs a targeter object"
    )

    cat("---\n", file = outfile, append = FALSE)
    cat(
      paste0('title: "', object$analysis, '"\n'),
      file = outfile,
      append = TRUE
    )
    cat(paste0('author: "', author, '"\n'), file = outfile, append = TRUE)
    cat(paste0('execute:\n  echo: false\n'), file = outfile, append = TRUE)
    cat(paste0("params:\n"), file = outfile, append = TRUE)
    cat(paste0("  object: ''\n"), file = outfile, append = TRUE)
    cat(paste0("  fullplot_which_plot: '1:2'\n"), file = outfile, append = TRUE)
    cat("format:\n", file = outfile, append = TRUE)
    cat("  ", format, ":\n", file = outfile, append = TRUE)
    if (format == "pptx") {
      assertthat::assert_that(
        file.exists(pptx_template),
        msg = "pptx template file does not exists"
      )
      cat(
        paste0('    reference-doc: "', pptx_template, '"\n'),
        file = outfile,
        append = TRUE
      )
      cat(paste0("    df-format: kable\n"), file = outfile, append = TRUE)
    }
    cat("---\n\n", file = outfile, append = TRUE)
    invisible(outfile)
  }

  file_extension <- switch(format, pptx = "pptx")
  # temp <- "targeter-quarto-tmp2a5644777c6933"

  temp <- basename(tempfile(pattern = "targeter-quarto-tmp"))

  tmp_file_outfile <- paste(temp, file_extension, sep = ".")
  tmp_file_object <- paste(temp, "tar.qs", sep = ".")

  qs::qsave(object, tmp_file_object)

  tmp_file_quarto <- paste(temp, "qmd", sep = ".")

  if (generate_yaml) {
    cat('\nOK\n')
    tmp_file_yaml <- write_quarto_yaml(
      object,
      pptx_template = pptx_template,
      outfile = paste(temp, "yml", sep = ".")
    )
    system(
      command = paste("cat", tmp_file_yaml, template, ">", tmp_file_quarto)
    )
  } else {
    file.copy(from = template, to = tmp_file_quarto)
  }

  launch_quarto <- system(
    intern = TRUE,
    command = paste0(
      "quarto render ",
      tmp_file_quarto,
      " --output ",
      tmp_file_outfile,
      " -P object:",
      basename(tmp_file_object)
    )
  )

  if (is.null(output_file)) {
    output_file <- basename(
      tempfile(
        pattern = "targeter-report-",
        fileext = paste0(".", file_extension)
      )
    )
  }
  if (file.exists(tmp_file_outfile)) {
    file.copy(
      from = tmp_file_outfile,
      to = file.path(output_dir, output_file),
      overwrite = TRUE
    )
  }

  if (delete_temporary_files){
      tempfiles <- list.files(pattern = temp, full.names = TRUE)
      file.remove(tempfiles)
      
  }
  cat("\nPresentation generated.\n")
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

# slidify(tar)
