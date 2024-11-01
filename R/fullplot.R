#' @title fullplot
#' @description autoplot method for crossvar OR targeter object 
#' (with a variable specification).
#' draws the combination of 4 selected plots/

#' @param x object of class 'crossvar' or 'targeter'
#' @param var character, used when x is a targeter object to select the variable
#' of interest (crossvar in x$profiles slot)
#' @param numvar_as character, one of 'value' (default) and 'bin'
#' @param metadata data.frame - if metadata is  loaded in R environment, label
#' of the variables can be used. Default value (NULL) corresponds to no metadata
#' available.
#' The label will be used for the title and the x-axis of the graph.
#' @param print_NA boolean - By default, the value is TRUE. If FALSE, the
#' missing values of the variable are not printed.
#' @param only_target_ref_level boolean - By default, the value is FALSE.
#' If TRUE, only print the values for the target TRUE.
#' @param lim_y  boolean - By default, the value is TRUE.
#' The axis y for the proportion is limited between 0 and 100.
#' @param title boolean: do we plot default generated title Y/N or character
#' (override title) - default: TRUE
#' @param subtitle boolean: do we plot default generated subtitle Y/N
#' or character (override subtitle)- default: TRUE
#' @param which_plot integer: which plots must contain the full plot
#' using: binary targets:  1 for counts, 2 for percentages, 3 for WOE and
#' 4 for quadrant. For numeric targets: 1 for boxplots, 2 for counts, 3 for
#' WoE and 4 for quadrant plot.
#' Default: 1:2
#'
#'
#' @return The function returns a ggplot graph
#' @seealso
#' \itemize{
#' \item \code{\link{plot.crossvar}}
#' \item \code{\link{quadrant_plot}}
#' }


#' @importFrom assertthat assert_that
#' @importFrom  ggplot2 ggplot
#' @importFrom ggplot2 theme_void
#' @importFrom patchwork wrap_plots
#' @importFrom patchwork plot_annotation
#'
#' @examples
#' CN <- crossvar(adult, target = "AGE", var = "SEX")
#' fullplot(CN, title = "this is my title", subtitle = FALSE,  which_plot = 1:4)
#' @export
fullplot <- function(x,
                     var = NULL,
                     numvar_as = c("value", "bin"),
                     metadata = NULL,
                     print_NA = TRUE,
                     only_target_ref_level = TRUE,
                     lim_y = TRUE,
                     title = TRUE,
                     subtitle = TRUE,
                     which_plot = 1:2) {
  assertthat::assert_that(inherits(x, what = c("crossvar", "targeter")),
    msg = "x must be a crossvar or a targeter object"
  )

  numvar_as <- match.arg(numvar_as, choices = c("value", "bin"), several.ok = FALSE)

  if (inherits(x, "targeter")) {
    assertthat::assert_that(!is.null(var),
      msg = "x being a targeter object, var must be provided"
    )
    assertthat::assert_that(is.character(var),
      msg = "var must be a character parameter"
    )


    assertthat::assert_that(var %in% names(x$profiles),
      msg = "var is not present in list of x profiles/crossvar"
    )

    x <- x$profiles[[var]]
  }

  # var_label = label(var, metadata = metadata)

  if (x$target_type %in% c("binary", "categorical")) {
    g1 <- plot(x,
      metadata = metadata,
      print_NA = print_NA,
      numvar_as = numvar_as,
      title = FALSE,
      do_plot = FALSE
    )

    g2 <- plot(x,
      show = "props",
      type = "l",
      metadata = metadata,
      print_NA = print_NA,
      only_target_ref_level = only_target_ref_level,
      numvar_as = numvar_as,
      title = FALSE,
      do_plot = FALSE
    )


    g3 <- plot_woe(x,
      metadata = metadata,
      title = FALSE,
      do_plot = FALSE
    )

    if (x$variable_type %in% c("character")) {
      g4 <- quadrant_plot(x, metadata = metadata, title = FALSE)
      L_quad <- TRUE
    } else {
      g4 <- ggplot2::ggplot() +
        ggplot2::theme_void()
      L_quad <- FALSE
    }

    # g34 <- gridExtra::arrangeGrob(g3, g4, ncol=2)



    ## add title
    if (is.logical(title)) {
      if (title) {
        title <- paste(
          label(x$targetname, metadata = metadata),
          label(x$varname, metadata = metadata),
          sep = " explained by: "
        )
      } else {
        title <- NULL
      }
    }
    if (is.logical(subtitle)) {
      if (subtitle) {
        subtitle <- c()
        if (1 %in% which_plot) subtitle <- c(subtitle, "Frequencies (N)")
        if (2 %in% which_plot) {
          subtitle <-  subtitle <- c(subtitle, "Percentages (%)")
        }
        if (3 %in% which_plot) {
          subtitle <-  subtitle <- c(subtitle, "WoE")
        }
        if ((4 %in% which_plot) & L_quad) {
          subtitle <- subtitle <- c(subtitle, "Quadrant plot (N/%)")
        }
        subtitle <- paste(subtitle, collapse = " | ")
      } else {
        subtitle <- NULL
      }
    }


  } else if (x$target_type %in% ("numeric")) {
    g1 <- plot.crossvar(x,
      show = "boxplot",
      metadata = metadata,
      print_NA = TRUE, do_plot = FALSE,
      numvar_as = numvar_as,
      title = FALSE
    )


    g2 <- plot.crossvar(x,
      show = "count",
      type = "bars",
      metadata = metadata,
      print_NA = TRUE, do_plot = FALSE,
      numvar_as = numvar_as,
      title = FALSE
    )

    g3 <- plot.crossvar(x,
      show = "woe",
      type = "bars",
      metadata = metadata,
      print_NA = TRUE, do_plot = FALSE,
      numvar_as = numvar_as,
      title = FALSE
    )


    if (x$variable_type %in% c("character")) {
      g4 <- quadrant_plot(x, metadata = metadata, title = FALSE)
      L_quad <- TRUE
    } else {
      # g4 <- ggplot() + theme_void()
      g4 <- plot.crossvar(x,
        show = "avg",
        type = "line",
        metadata = metadata,
        print_NA = TRUE, do_plot = FALSE,
        numvar_as = "value",
        title = FALSE
      )
      L_quad <- FALSE
    }

    ## add title
    if (is.logical(title)) {
      if (title) {
        title <- paste(
          label(x$targetname, metadata = metadata),
          label(x$varname, metadata = metadata),
          sep = " explained by: "
        )
      } else {
        title <- NULL
      }
    }
    if (is.logical(subtitle)) {
      if (subtitle) {

        subtitle <- c()
        if (1 %in% which_plot) subtitle <- c(subtitle, "Boxplot")
        if (2 %in% which_plot) {
          subtitle <- c(subtitle, "Frequencies (N)")
        }
        if (3 %in% which_plot) {
          subtitle <- c(subtitle, "WoE")
        }
        if ((4 %in% which_plot) & L_quad) {
          subtitle <- c(subtitle, "Quadrant plot (N/avg)")
        }
        subtitle <- paste(subtitle, collapse = " | ")
      } else {
        subtitle <- NULL
      }
    }
  } else {
    stop("target type not handled by fullplot")
  }
  expr <- paste("g", which_plot, sep = "", collapse = ",")
  expr <- paste0("patchwork::wrap_plots(", expr, ")")
  allplots <- eval(parse(text = expr))
  allplots <- allplots + patchwork::plot_annotation(
    title = title,
    subtitle = subtitle
  )
  return(allplots)
}