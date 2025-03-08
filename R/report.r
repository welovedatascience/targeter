#' @rdname report
#' @title Generate reports for targeter's analyses
#'
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
#' @param show_toc - logical. If TRUE (default), a table of content will be
#' generated. If FALSE, no table of content will be generated.
#' @param show_details - logical. If TRUE (default), some more information will be
#' displayed in the report.
#' @param verbose - logical. If TRUE, the function will print some information
#' along the process
#' @param fullplot_numvar_as - character, one of "bin" or "value"
#' (cf `fullplot` documentation)
#' @param metadata_vars - list of two character strings: varname and varlabel.
#' Default to varname="variable", varlabel="label". Used to specify the columns in
#' metadata that contains variable names and labels.
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
#' }1

#' @keywords targeter report

#' @export
report <- function(object, ...) {
  UseMethod("report")
}

#' @rdname report
#' @export
report.targeter <- function(
  object,
  summary_object = NULL,
  metadata = NULL,
  metadata_vars = list(varname = "variable", varlabel = "label"),
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
    format(Sys.time(), format = "%Y-%m-%d_%H%M%S"),
    "-targeter-report"
  ),
  pptx_reference_doc = NULL, # for pptx format, default template,
  # revealjs_template = "", # todo prepare a revealjs template
  output_file = "index",
  title = object$analysis,
  author = getOption("targeter.author", "wlds targeter"),
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
  custom_fields = list("freeze" = freeze),
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
    if (verbose)
      cat(
        "\n- For pptx format, we will use template:",
        pptx_reference_doc,
        "\n"
      )
  }

  assertthat::assert_that(
    is.character(quarto_project_brandfile),
    msg = "brandfile must be a character string"
  )

  if (quarto_project_brandfile != "") {
    assertthat::assert_that(
      file.exists(quarto_project_brandfile),
      msg = "brandfile must be a valid file"
    )
    if (verbose) cat("\n- Using brandfile:", quarto_project_brandfile, "\n")
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
      "report-template.pptx"
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
  if (verbose) cat("\n- Using template:", template, "\n")

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

  if (!dir.exists(file.path(quarto_root_dir, quarto_targeters_project_dir))) {
    if (verbose)
      cat(
        "\n- Creating quarto project folder:",
        file.path(quarto_root_dir, quarto_targeters_project_dir),
        "\n"
      )
    # we must create a quarto project to store targeters reports
    quarto::quarto_create_project(
      name = quarto_targeters_project_dir,
      type = "website",
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
        paste("index.qmd", sep = ".")
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
    if (verbose)
      cat(
        "\n- Using existing quarto project folder:",
        file.path(
          quarto_root_dir,
          quarto_targeters_project_dir
        ),
        "\n"
      )
    assertthat::assert_that(
      quarto::is_using_quarto(file.path(
        quarto_root_dir,
        quarto_targeters_project_dir
      )),
      msg = "Provided quarto project folder is not a valid quarto project"
    )
  }

  # subdir reports
  reports_path <- dir.create(
    file.path(
      quarto_root_dir,
      quarto_targeters_project_dir,
      "reports"
    ),
    showWarnings = FALSE
  )

  target_path <- file.path(
    quarto_root_dir,
    quarto_targeters_project_dir,
    "reports",
    targeter_sub_folder
  )
  target_path <- tolower(target_path) # potential quarto bug for listings
  if (verbose) cat("\n- using  targeter report folder:", target_path, "\n")
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
  has_pptx_template <- FALSE

  if (default_template || !default_pptx_template) {
      #pptx_template <- "targeter-report.pptx"
      tmp_pptx_reference_doc <- "report-template.pptx"
      pptx_copied <- file.copy(
        from = pptx_reference_doc,
        to = file.path(target_path, tmp_pptx_reference_doc),
        overwrite = TRUE
      )
      assertthat::assert_that(
        pptx_copied,
        msg = "Could not copy pptx template file"
      )
  }

  if (("pptx" %in% format) || ("all" %in% format)) {
    # if default template we will also use powerpoint wlds template (or override
    # with user provided one)
      has_pptx_template <- TRUE
      pptx_reference_doc <- tmp_pptx_reference_doc
    }
  }

  yaml <- list(title = title, author = author, date = format(Sys.Date()))
  
  custom_fields[["reference-doc"]] <- pptx_reference_doc
  
  if (!is.null(custom_fields)) {
    assertthat::assert_that(
      !is.null(names(custom_fields)),
      msg = "custom fields parameter must be a named list."
    )
    yaml[names(custom_fields)] <- custom_fields
  }
  # search in template and replace by yaml equivalent
  temp <- readLines(file.path(
    target_path,
    paste(output_file, "qmd", sep = ".")
  ))
  if (any(grepl("##{{targeter-yaml}}##", temp, fixed = TRUE))) {
    class(custom_fields) <- c("verbatim", class(custom_fields))
    # see yaml::as.yaml documentation
    # custom handler with verbatim output to change how logical vectors are
    # emitted

    out_yaml <- yaml::as.yaml(
      yaml,
      handlers = list(logical = yaml::verbatim_logical)
    )

    temp <- gsub("##{{targeter-yaml}}##", out_yaml, temp, fixed = TRUE)

    cat(
      temp,
      sep = "\n",
      file = file.path(target_path, paste(output_file, "qmd", sep = ".")),
      append = FALSE
    )
  }

  meta_yml_params <- list(
    object = "tar.rds",
    summary_object = "tar_summary.rds",
    fullplot_which_plot = fullplot_which_plot,
    fullplot_numvar_as = fullplot_numvar_as,
    metadata_var_field = metadata_vars$varname,
    metadata_var_label = metadata_vars$varlabel,
    show_tables = as.character(show_tables),
    show_toc = as.character(show_toc),
    show_details = as.character(show_details)

    -
  )
  # pandoc_args <- c()

  if (render) {
    quarto::quarto_render(
      input = target_path,
      execute_params = meta_yml_params,
      debug = debug,
      output_format = format,
      as_job = FALSE #,pandoc_args = pandoc_args
    )
    cat("\nPresentation generated in folder:.", target_path, "\n")
    invisible(file.path(target_path))
  }
  invisible(TRUE)
}


#' @title report.tartree
#' @description This function creates an automatic report according to a
#'  predefined template in the package or a user-generated template.
#' Create a report from a targeter tree object
#'
#' @param object - an object of class targeter tartree
#' @param metadata - data.frame - if metadata is  loaded in R environment,
#' label of the variables can be used. Default value (NULL) corresponds to
#' no metadata available.
#' The label will be used for the title and the x-axis of the graph.
#' @param format - Target output format (defaults to "html").
#'  The option "all" will render all formats.
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
#' @param metadata_vars - list of two character strings: varname and varlabel.
#' Default to varname="variable", varlabel="label". Used to specify the columns in
#' metadata that contains variable names and labels.
#' @param render - logical. If TRUE (default), the report will be rendered
#'  and saved in the target_sub_folder. If FALSE, the report will be generated but not
#'  rendered. Useful for debugging.
#' @param debug - logical. If TRUE, the function will print debug information (passed to Quarto call).
#' @param freeze - logical: default TRUE. If TRUE, the report will be frozen
#' (no more changes can be made). If FALSE, the report will be editable.
#' Frozen reports renders can always be bypassed by explicit call to quarto.
#' @param verbose - logical. If TRUE, the function will print some information
#' along the process
#' @param custom_fields - list of custom fields to be added to the report.
#' @param ...  additional parameters to be passed to quarto, for instance
#' allowing to pass/overwite any YAML set-up
#' @return invisibly returns path to the generated specific file (unique format)
#' or folder (several formats)

#' @rdname report
#' @export

report.tartree <- function(
  object,
  metadata = NULL,
  metadata_vars = list(varname = "variable", varlabel = "label"),
  format = "html",
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
    "targeter-report-tree-",
    format(Sys.time(), format = "%Y-%m-%d_%H%M%S")
  ),
  pptx_reference_doc = NULL, # for pptx format, default template,
  # revealjs_template = "", # todo prepare a revealjs template
  output_file = "index",
  title = paste0(
    "Decision tree for target '",
    attr(object, "target"),
    "' on data:",
    attr(object, "tar_object")$description_data
  ),
  author = getOption("targeter.author", "wlds targeter"),
  render = TRUE,
  freeze = TRUE,

  debug = FALSE,
  verbose = FALSE,
  custom_fields = list("freeze" = freeze),
  # todo implement
  ... # additional parameters to be passed to quarto
) {
  cat("\n Yes report tartree\n")
  assertthat::assert_that(
    pacman::p_load("quarto"),
    msg = "quarto package and runtime are required."
  )
  deps <- c("explore", "rpart", "dplyr", "pROC")
  if (getOption("targeter.auto_install_deps", FALSE)) {
    pacman::p_load(char = deps)
  }
  assertthat::assert_that(
    all(pacman::p_load(char = deps, install = FALSE)),
    msg = paste(
      'some of targeter following optional packages required for decision trees are not available:',
      paste(deps, collapse = ",")
    )
  )
  #

  assertthat:::assert_that(
    inherits(object, "tartree"),
    msg = "object must to be a tartree object"
  )

  #  todo cover all parameters  assert tests
  assertthat::assert_that(
    is.character(format),
    msg = "format must be a character"
  )
  # format <- match.arg(format, c(""pptx","revealjs","beamer"), several.ok = FALSE)

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
    if (verbose)
      cat(
        "\n- For pptx format, we will use template:",
        pptx_reference_doc,
        "\n"
      )
  }

  assertthat::assert_that(
    is.character(quarto_project_brandfile),
    msg = "brandfile must be a character string"
  )

  if (quarto_project_brandfile != "") {
    assertthat::assert_that(
      file.exists(quarto_project_brandfile),
      msg = "brandfile must be a valid file"
    )
    if (verbose) cat("\n- Using brandfile:", quarto_project_brandfile, "\n")
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
      "report-template.pptx"
    )
  }
  # cat(pptx_reference_doc)

  default_template <- is.null(template)
  if (is.null(template)) {
    template <- file.path(
      find.package("targeter", lib.loc = .libPaths()),
      "ressources",
      "report-tartree-template.qmd"
    )
  }

  assertthat::assert_that(
    file.exists(template),
    msg = "template file does not exist"
  )
  if (verbose) cat("\n- Using template:", template, "\n")

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

  if (!dir.exists(file.path(quarto_root_dir, quarto_targeters_project_dir))) {
    if (verbose)
      cat(
        "\n- Creating quarto project folder:",
        file.path(quarto_root_dir, quarto_targeters_project_dir),
        "\n"
      )
    # we must create a quarto project to store targeters reports
    quarto::quarto_create_project(
      name = quarto_targeters_project_dir,
      dir = quarto_root_dir,
      type = "website",
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
        paste("index", "qmd", sep = ".")
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
    if (verbose)
      cat(
        "\n- Using existing quarto project folder:",
        file.path(
          quarto_root_dir,
          quarto_targeters_project_dir
        ),
        "\n"
      )
    assertthat::assert_that(
      quarto::is_using_quarto(file.path(
        quarto_root_dir,
        quarto_targeters_project_dir
      )),
      msg = "Provided quarto project folder is not a valid quarto project"
    )
  }


  # subdir reports
  reports_path <- dir.create(
    file.path(
      quarto_root_dir,
      quarto_targeters_project_dir,
      "reports"
    ),
    showWarnings = FALSE
  )

  target_path <- file.path(
    quarto_root_dir,
    quarto_targeters_project_dir,
    "reports",
    targeter_sub_folder
  )
  target_path <- tolower(target_path) # potential quarto bug for listings
  
  if (verbose) cat("\n- using  targeter report folder:", target_path, "\n")
  if (!dir.exists(target_path)) {
    dir_created <- dir.create(target_path, showWarnings = FALSE)
    assertthat::assert_that(
      dir_created,
      msg = "Could not create targeter report folder"
    )
  }

  print(target_path)
  attr(object, "metadata") <- metadata
  saveRDS(object, file.path(target_path, "tar_mod.rds"))

  template_copied <- file.copy(
    from = template,
    to = file.path(target_path, paste(output_file, "qmd", sep = ".")),
    overwrite = TRUE
  )
  assertthat::assert_that(template_copied, msg = "Could not copy template file")

  # pptx template
  has_pptx_template <- FALSE

  if (default_template || !default_pptx_template) {
      #pptx_template <- "targeter-report.pptx"
      tmp_pptx_reference_doc <- "report-template.pptx"
      pptx_copied <- file.copy(
        from = pptx_reference_doc,
        to = file.path(target_path, tmp_pptx_reference_doc),
        overwrite = TRUE
      )
      assertthat::assert_that(
        pptx_copied,
        msg = "Could not copy pptx template file"
      )
  }

  if (("pptx" %in% format) || ("all" %in% format)) {
    # if default template we will also use powerpoint wlds template (or override
    # with user provided one)
      has_pptx_template <- TRUE
      pptx_reference_doc <- tmp_pptx_reference_doc
    }
  }


  yaml <- list(title = title, author = author, date = format(Sys.Date()))
  custom_fields[["reference-doc"]] <- pptx_reference_doc

if (!is.null(custom_fields)) {
    assertthat::assert_that(
      !is.null(names(custom_fields)),
      msg = "custom fields parameter must be a named list."
    )
    yaml[names(custom_fields)] <- custom_fields
  }
  # search in template and replace by yaml equivalent
  
  
  
   if (any(grepl("##{{targeter-yaml}}##", temp, fixed = TRUE))) {
    class(custom_fields) <- c("verbatim", class(custom_fields))
    # see yaml::as.yaml documentation
    # custom handler with verbatim output to change how logical vectors are
    # emitted

    out_yaml <- yaml::as.yaml(
      yaml,
      handlers = list(logical = yaml::verbatim_logical)
    )

    temp <- gsub("##{{targeter-yaml}}##", out_yaml, temp, fixed = TRUE)

    cat(
      temp,
      sep = "\n",
      file = file.path(target_path, paste(output_file, "qmd", sep = ".")),
      append = FALSE
    )
  }

  meta_yml_params <- list(
    model = "tar_mod.rds",
    metadata_var_field = metadata_vars$varname,
    metadata_var_label = metadata_vars$varlabel
  )
  # pandoc_args <- c()

  # infile <- file.path(target_path, paste(output_file, "qmd", sep = "."))
  print(infile)
  if (render) {
    quarto::quarto_render(
      input = target_path,
      execute_params = meta_yml_params,
      debug = debug,
      output_format = format,
      as_job = FALSE #,pandoc_args = pandoc_args
    )
    cat(
      "\nTargeter decision tree report generated in folder:.",
      target_path,
      "\n"
    )
    invisible(file.path(target_path))
  }
  invisible(TRUE)
}
