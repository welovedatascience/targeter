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
#' @param quarto_root_dir (default "./targeter-reports") Path to a  root folder
#'  where  quarto project folder will be avaible. Default to "." so that
#' calling targeter first time in an analyses project will initiate everything
#' for user.
#' @param quarto_targeters_project_dir (default "targeter-reports", within
#' quarto_root_dir) Path to a folder where all targeter reports will be stored.
#' Using a pre-existing projetc allows benfiting from its settings.
#' (you might  want to put brand_file to empty or use your own brand file)
#' @param quarto_project_template - character. Path to a default Quarto template
#' that will be used as Quarto reference-doc for all formats. If NULL (default),
#' we will use targeter package default templates.
#' @param quarto_project_brandfile - character. Path to a default Quarto brandfile
#' that will be used as Quarto reference-doc for all formats. If NULL (default),
#' we will use targeter package default brandfile.
#' @param targeter_sub_folder - character. Output directory - default to create a
#' new dedicated directory in working folder.
#' @param output_file - character, name of output file (without file extension).
#' default value: index
#' @param title - character, title for the presentation. Overide default title
#' that consists in taking title for targeter object 'analysis' slot.
#' @param author - character: presentatiopn author.
#' @param fullplot_which_plot - character, selection of plots (cf `fullplot`
#' documentation)
#' @param show_tables - logical whetheer to print tables in seprate slides in
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
#'  and saved in the target_sub_folder. If FALSE, the report will be generated but not
#'  rendered. Useful for debugging.
#' @param debug - logical. If TRUE, the function will print debug information 
#' (passed to Quarto call).
#' @param freeze - logical: default TRUE. If TRUE, the report will be frozen
#' (no more changes can be made). If FALSE, the report will be editable. 
#' Frozen reports renders can always be bypassed by explicit call to quarto. 
#' @param custom_fields - list of custom fields to be added to the report. 
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
## @importFrom quarto quarto_render
#' @importFrom pacman p_load
#' @importFrom data.table fwrite
#' @importFrom yaml as.yaml
#' @examples
#' \dontrun{
#' tar <- targeter(adult, target ="ABOVE50K", analysis_name="Analyse",
#' naming_conventions=FALSE)
#' report(tar,  author = "this is me")
#' report(tar,  format= c("pptx","pdf","docx"),author = "this is me")
#' }

report <- function(
  object,
  summary_object = NULL,
  metadata = NULL,
  metadata_vars= list(varname="variable",label="label"),
  format = "html",
  nmax = 100, #todo implement
  template = NULL, # default QMD template for slides
  quarto_root_dir = ".",
  quarto_targeters_project_dir = "targeter-reports",
  quarto_project_template = getOption(
    "targeter.template",
    file.path(
      find.package("targeter", lib.loc = .libPaths()),
      "ressources",
      "_targeter_project_default.yml"
    )
  ),
  quarto_project_brandfile = getOption(
    "targeter.brandfile",
    file.path(
      find.package("targeter", lib.loc = .libPaths()),
      "ressources",
      "_targeter_brand_default.yml"
    )
  ),
  targeter_sub_folder = paste0(
    "targeter-report-",
    format(Sys.time(), format = "%Y-%m-%d_%H%M%S")
  ),
  pptx_reference_doc = NULL, # for pptx format, default template,
  # revealjs_template = "", # todo prepare a revealjs template
  output_file = "index",
  title = object$analysis,
  author = getOption("targeter.author", "welovedatascience targeter"),
  fullplot_which_plot = "1:2",
  fullplot_numvar_as = "bin",
  show_tables = FALSE,
  show_toc = TRUE,
  show_details = TRUE,
  debug = FALSE,
  render = TRUE,
  logo = NULL,
  freeze = TRUE,
  verbose = FALSE,
  custom_fields = list("report_type" = "targeter", "freeze" = freeze),
   # todo implement
  ... # additional parameters to be passed to quarto
) {
  assertthat::assert_that(
    pacman::p_load("quarto"),
    msg = "quarto package and runtime are required."
  )

  # todo cover all parameters  assert tests
  assertthat::assert_that(
    is.character(format),
    msg = "format must be a character"
  )
  # format <- match.arg(format, c(""pptx","revealjs","beamer"), several.ok = FALSE)

  assertthat::assert_that(
    inherits(object, "targeter"),
    msg = "The class of object must to be 'targeter'."
  )
  assertthat::assert_that(
    inherits(metadata, "data.frame") | is.null(metadata),
    msg = "The parameter metadata must be either NULL (no metadata) or a data.frame"
  )
  if (("pptx" %in% format) && !is.null(pptx_reference_doc)) {
    assertthat::assert_that(
      is.character(pptx_reference_doc),
      msg = "pptx template must be a character string giving the path of a powerpoint template"
    )
    assertthat::assert_that(
      file.exists(pptx_reference_doc),
      msg = "powerpoint template provided file does not exist"
    )
    if (verbose)cat("\n- For pptx format, we will use template:",pptx_reference_doc,"\n")
  }

  assertthat::assert_that(
    is.character(quarto_project_brandfile), msg = "brandfile must be a character string")

   if (quarto_project_brandfile != "") {
    assertthat::assert_that(
      file.exists(quarto_project_brandfile),
      msg = "brandfile must be a valid file"
    )
    if (verbose)cat("\n- Using brandfile:",quarto_project_brandfile,"\n")
   }
  
  assertthat::assert_that(
    is.character(targeter_sub_folder),
    msg = "target_sub_folder must be a character string giving the path of the output directory"
  )
  # todo: test if valid string foe a folder name

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
  if (is.null(logo)) {
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
  if (verbose)cat("\n- Using template:",template,"\n")

  # quarto_root_dir = ".",
  # quarto_targeters_project_dir = "targeter-reports",
  # quarto_project_template = getOption("targeter.template", file.path(
  #     find.package("targeter", lib.loc = .libPaths()),
  #     "ressources",
  #     "_targeter_project_default.yml"
  #   )),
  # quarto_project_brandfile = getOption("targeter.brandfile", file.path(

  assertthat::assert_that(
    is.character(quarto_root_dir),
    msg = "quarto_root_dir must be a charcter string giving path to a folder"
  )
  assertthat::assert_that(
    dir.exists(quarto_root_dir),
    msg = "quarto_root_dir must be a valid directory"
  )
  assertthat::assert_that(
    is.character(quarto_targeters_project_dir),
    msg = "quarto_targeters_project_dir must be a charcter string giving path to a folder"
  )

  assertthat::assert_that(
    is.character(quarto_project_brandfile),
    msg = "quarto_project_brandfile must be a charcter string giving path to a file or being empty (no brand file used)"
  )


  if (!dir.exists(file.path(quarto_root_dir,quarto_targeters_project_dir))) {
    if (verbose)cat("\n- Creating quarto project folder:",file.path(quarto_root_dir,quarto_targeters_project_dir),"\n")
    # we must create a quarto project to store targeters reports
    quarto::quarto_create_project(
      name = quarto_targeters_project_dir,
      dir = quarto_root_dir,
      no_prompt = TRUE,
      quiet = TRUE
    )

    # copy quarto project template there
    assertthat::assert_that(
      is.character(quarto_project_template),
      msg = "quarto_project_template must be a charcter string giving path to a file"
    )
    assertthat::assert_that(
      file.exists(quarto_project_template),
      msg = "quarto_project_template must be a valid file"
    )
    file.copy(
      from = quarto_project_template,
      to = file.path(
        quarto_root_dir,
        quarto_targeters_project_dir,
        paste(quarto_targeters_project_dir, "qmd", sep = ".")
      ),
      overwrite = TRUE
    )
    # cp _brand file

    if (quarto_project_brandfile != "") {
      assertthat::assert_that(
        file.exists(quarto_project_brandfile),
        msg = "quarto_project_brandfile must be a valid file"
      )
      file.copy(
        from = quarto_project_brandfile,
        to = file.path(
          quarto_root_dir,
          quarto_targeters_project_dir,
          "_brand.yml"
        ),
        overwrite = TRUE
      )
    }
    quarto::quarto_render(
      input = file.path(quarto_root_dir, quarto_targeters_project_dir),
      debug = debug,
      as_job = FALSE
    )
    cat(" - Done.\n")
  } else {
    
    if (verbose)cat("\n- Using existing quarto project folder:",file.path(
        quarto_root_dir,
        quarto_targeters_project_dir
      )
      ,"\n")
    assertthat::assert_that(
      quarto::is_using_quarto(file.path(
        quarto_root_dir,
        quarto_targeters_project_dir
      )),
      msg = "Provided quarto project folder is not a valid quarto project"
    )
  }

  target_path <- file.path(
    quarto_root_dir,
    quarto_targeters_project_dir,
    targeter_sub_folder
  )
  if (verbose)cat("\n- using  targeter report folder:",target_path,"\n")
  if (!dir.exists(target_path)) {
    dir_created <- dir.create(target_path, showWarnings = FALSE)
    assertthat::assert_that(
      dir_created,
      msg = "Could not create targeter report folder"
    )
  }
  
  attr(object, "metadata") <- metadata
  if (is.null(summary_object)) summary_object <- summary(object)
  saveRDS(summary_object, file.path(target_path, "tar_summary.rds"))
  
  
  saveRDS(object, file.path(target_path, "tar.rds"))
  template_copied <- file.copy(
    from = template,
    to = file.path(target_path, paste(output_file, "qmd", sep = ".")),
    overwrite = TRUE
  )
  assertthat::assert_that(template_copied, msg = "Could not copy template file")

  # pptx template
  if (("pptx" %in% format) |("all" %in% format)) {
    # if default template we will also use powerpoint wlds template (or override
    # with user provided one)
    has_pptx_template <- FALSE
    if (default_template | !default_pptx_template) {
      #pptx_template <- "targeter-report.pptx"
      tmp_pptx_reference_doc <- "report-template.pptx"
      file.copy(
        from = pptx_reference_doc,
        to = file.path(target_path, tmp_pptx_reference_doc),
        overwrite = TRUE
      )
      has_pptx_template <- TRUE
      pptx_reference_doc <- tmp_pptx_reference_doc
    }
  }

  if (!is.null(custom_fields)){
    # search in template and replace by yaml equivalent
    temp <- readLines(file.path(target_path, paste(output_file, "qmd", sep = ".")))
    if (any(grepl("##{{custom-fields}}##", temp, fixed=TRUE))){
      cat("Yes.")
      class(custom_fields) <- c("verbatim", class(custom_fields))
      # see yaml::as.yaml documentation
        # custom handler with verbatim output to change how logical vectors are
  # emitted
      out_yaml <- yaml::as.yaml(custom_fields,
        handlers = list(logical = yaml::verbatim_logical))


      temp <- gsub("##{{custom-fields}}##", out_yaml, temp, fixed=TRUE)
      print(temp[49])

      cat(temp,
      sep = "\n", 
      file = file.path(target_path, paste(output_file, "qmd", sep = ".")), 
      append= FALSE)
    }
  }

  meta_yml_params <- list(
    object = "tar.rds",
    summary_object = "tarsum.rds",
    fullplot_which_plot = fullplot_which_plot,
    fullplot_numvar_as = fullplot_numvar_as,
    metadata_var_field = metadata_vars$varname,
    metadata_var_label = metadata_vars$label,
    title = title,
    author = author,
    show_tables = as.character(show_tables),
    show_toc = as.character(show_toc),
    show_details = as.character(show_details)


    # todo decision trees
  )
  pandoc_args <- c()
  if (has_pptx_template) {
    pandoc_args <- c(
      "--reference-doc" = file.path(pptx_reference_doc)
    )
  }
  if (render) {
    quarto::quarto_render(
      input = target_path,
      execute_params = meta_yml_params,
      debug = debug,
      output_format = format,
      as_job = FALSE,
      pandoc_args = pandoc_args
    )
    cat("\nPresentation generated in folder:.", target_path, "\n")
    invisible(file.path(target_path))
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
