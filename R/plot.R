# TODO fix plot numvar_as value
#' @title plot.crossvar
#' @description plot method for crossvar object.
#' plot is the main function that will automatically dispatch to plot.crossvar_binary, plot.crossvar_categorical or plot.crossvar_continuous,
#'  depending on target type.

#' @param x object of class "crossvar"
#' @param ... parameter to pass to plot.crossvar function specific for the target type.
#'
#'
#'
#' @return The function returns a ggplot graph
#' @seealso
#' \itemize{
#' \item \code{\link{crossvar}}
#' \item \code{\link{summary.crossvar}}
#' \item \code{\link{quadrant_plot}}
#' }
#' @importFrom gridExtra grid.arrange
#' @importFrom  ggplot2 ggplot
#'
#' @examples
#' t <- crossvar(adult,target="ABOVE50K", var="AGE",)
#' plot(t,"counts")
#' plot(t,"counts",type="line")
#' plot(t,"props",print_NA = FALSE, only_target_ref_level = TRUE)
#' @method plot crossvar
#' @rdname plot.crossvar
#' @export
#'
#'
#'
plot.crossvar <- function(x, ...) {
  if (x$target_type %in% c('binary', 'categorical')) {
    plot.crossvar_categorical(x, ...)
  } else if (x$target_type %in% ('numeric')) {
    plot.crossvar_numeric(x, ...)
  }
}

#' @method plot crossvar_binary

plot.crossvar_binary <- function(x, ...) {
  plot.crossvar_categorical(x, ...)
}

#' @param x object of class "crossvar"
#' @param show character - determine on which data considered. By default, the value is "counts".
#'  This parameter can only take the following values:
#' \itemize{
#' \item "counts" : the data used is the contingency table created by the function crossvar.
#' \item "props" : the data used is the proportion table created by the function crossvar.
#' \item "index" : the data used is the index table created by the function crossvar.
#' }
#' @param type character - determine the type of graph. By default, the value is "auto".
#'  This parameter can only take the following values:
#' \itemize{
#' \item "auto" : "counts" shows a bar plot and "props" shows a line plot.
#' \item "bars" : shows a bar plot.
#' \item "line": shows a line plot.
#' }
#' @param metadata data.frame - if metadata is  loaded in R environment, label of the variables can be used. Default value (NULL) corresponds to no metadata available.
#' The label will be used for the title and the x-axis of the graph.
#' @param print_NA boolean - By default, the value is TRUE. If FALSE, the missing values of the variable are not printed.
#' @param title boolean: do we plot default generated title Y/N or character (override title) - default: TRUE
#' @param do_plot boolean - whether to effectively show the plot or not (internal use to combine plots)
#' @param ... additional parameter
#' @rdname plot.crossvar
#'
#' @method plot crossvar_numeric

#' @importFrom data.table setnames

plot.crossvar_numeric <- function(
  x,
  show = c("boxplot", "median", "count", "avg", "woe"),
  type = c("auto", "bars", "line"),
  numvar_as = c("bin", "value"),
  metadata = NULL,
  print_NA = TRUE,
  title = TRUE,
  do_plot = TRUE,
  ...
) {
  assertthat::assert_that(
    inherits(x, "crossvar"),
    msg = "the parameter x must to be an object of class crossvar"
  )
  assertthat::assert_that(
    inherits(show, "character"),
    msg = "the parameter show must to be a character"
  )
  assertthat::assert_that(
    inherits(type, "character"),
    msg = "the parameter type must to be a character"
  )
  assertthat::assert_that(
    inherits(metadata, "data.frame") | is.null(metadata),
    msg = "The parameter metadata must be either NULL (no metadata) or a data.frame"
  )
  assertthat::assert_that(
    inherits(print_NA, "logical"),
    msg = "The parameter print_NA must to be boolean (TRUE/FALSE)"
  )

  options(scipen = 999) ## remove scientifical notation in numbers

  ##this command allows to make shortcut during the function call and to be sure to enter the good values
  show <- match.arg(
    show,
    c("boxplot", "median", "count", "avg", "woe"),
    several.ok = FALSE
  )
  type <- match.arg(type, c("auto", "bars", "line"), several.ok = FALSE)
  numvar_as <- match.arg(numvar_as, c("bin", "value"), several.ok = FALSE)

  if (type == "auto") {
    type <- "bars"
    if (show %in% c("median", "avg") & numvar_as == "value") type <- 'line'
  }

  if (show == 'woe') {
    return(
      plot_woe(
        x,
        metadata = metadata,
        print_NA = print_NA,
        numvar_as = numvar_as,
        title = title,
        do_plot = do_plot
      )
    )
  }
  df <- x$stats
  ##reorder the table:
  # df <- df[match(x$orderlabel, rownames(df)),, drop=FALSE]

  ## add the level with the rownames
  dfm <- cbind(level = rownames(df), df)

  ## we test if there exists missing on the variable !
  existmissing_var <- any(dfm$level == "[Missing]")

  if (show == "boxplot") {
    yrange <- c(min(df$bxp_min), max(df$bxp_max))
  } else {
    if (show %in% c("median", "avg")) {
      yrange <- range(df[[show]])
    } else if (show %in% "count") {
      yrange <- c(0, max(df[[show]]))
    }
  }

  if (show %in% "count") {
    data.table::setnames(dfm, 'count', 'N')
  }
  if (existmissing_var) {
    ## we take only the class [Missing]
    dfm.NA <- dfm[dfm$level == "[Missing]", ]
    ## we suppress the class [Missing]
    dfm <- dfm[dfm$level != "[Missing]", ]
  }

  if (numvar_as == 'value' & x$variable_type == "numeric") {
    # replace labels per bin center values
    dfm[['level']] <- as.numeric(x$numcenters[dfm[['level']]])
  }

  if (!is.numeric(dfm[['level']])) {
    dfm$level <- factor(dfm$level, levels = x$orderlabel)
  }

  plotValues <- function(dfm, forNA = FALSE) {
    if (show == "boxplot") {
      p1 <- ggplot2::ggplot(
        dfm,
        # Draw multiple ggplot2 boxplots
        ggplot2::aes(
          x = level,
          ymin = bxp_min,
          lower = q25,
          middle = median,
          upper = q75,
          ymax = bxp_max,
          group = level
        )
      ) +
        ggplot2::geom_boxplot(stat = "identity", show.legend = !forNA)
      p1 <- p1 + ggplot2::theme_bw()
    } else {
      # median or avg
      if (show == "median") {
        p1 <- ggplot2::ggplot(dfm, ggplot2::aes(x = level, y = median))
      } else if (show == "avg") {
        p1 <- ggplot2::ggplot(dfm, ggplot2::aes(x = level, y = avg))
      } else if (show == "count") {
        p1 <- ggplot2::ggplot(dfm, ggplot2::aes(x = level, y = N))
      }
      p1 <- p1 + ggplot2::theme_bw()
      if (type == "bars") {
        ## create bar plot
        p1 = p1 + ggplot2::geom_bar(stat = "identity", show.legend = !forNA)
        p1 = p1 +
          ggplot2::theme(
            legend.position = "top",
            legend.title = ggplot2::element_blank()
          ) ##change the position of the legend + suppress title legend
        p1 <- p1 + ggplot2::scale_y_continuous(labels = scales::number)
      } else {
        ## lines
        if (forNA) {
          p1 = p1 + ggplot2::geom_point(show.legend = FALSE)
        } else {
          p1 = p1 +
            ggplot2::geom_line(group = 1) +
            ggplot2::theme(
              legend.position = "top",
              legend.title = ggplot2::element_blank()
            )
        }
      }
    }

    if (show %in% c('boxplot', 'avg', 'median')) {
      #
      p1 = p1 +
        ggplot2::geom_hline(
          yintercept = x$target_stats$avg,
          linetype = "solid",
          color = "darkorchid"
        )
    }
    ## add a label to the x axis : the variable name. If there is a metadata file, it puts the label of the variabe
    if (forNA) {
      p1 <- p1 + ggplot2::xlab(" \n  ")
    } else {
      if (is.null(metadata)) {
        p1 = p1 + ggplot2::xlab(x$varname)
      } else {
        p1 <- p1 + ggplot2::xlab(label(x$varname, metadata))
      }
    }

    ## we put the names for the axis x in the way vertical
    p1 <- p1 +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))

    suppressMessages(p1 <- p1 + ggplot2::ylim(yrange)) # change y range
    invisible(p1)
  }

  allplots <- plotValues(dfm) # Begin with non-NA values

  if (existmissing_var & print_NA == TRUE) {
    plotNA <- plotValues(dfm.NA, forNA = TRUE)

    allplots <- gridExtra::arrangeGrob(
      allplots,
      plotNA,
      widths = c(8, 2),
      nrow = 1
    )
  } else {
    # Only main plot
    ## add title
    if (is.logical(title)) {
      if (title) {
        str_title <- label(x$targetname, metadata = metadata)
        allplots <- allplots + ggplot2::ggtitle(str_title)
      }
    } else if (is.character(title)) {
      allplots <- allplots + ggplot2::ggtitle(title)
    }
  }
  if (do_plot) plot(allplots)
  allplots
}

#' @param x object of class "crossvar"
#' @param show character - determine on which data considered. By default, the value is "counts".
#'  This parameter can only take the following values:
#' \itemize{
#' \item "counts" : the data used is the contigency table created by the function crossvar.
#' \item "props" : the data used is the proportion table created by the function crossvar.
#' \item "index" : the data used is the index table created by the function crossvar.
#' }
#' @param type character - determine the type of graph. By default, the value is "auto".
#'  This parameter can only take the following values:
#' \itemize{
#' \item "auto" : "counts" shows a bar plot and "props" shows a line plot.
#' \item "bars" : shows a bar plot.
#' \item "line": shows a line plot.
#' }
#' @param metadata data.frame - if metadata is  loaded in R environment, label of the variables can be used. Default value (NULL) corresponds to no metadata available.
#' The label will be used for the title and the x-axis of the graph.
#' @param print_NA boolean - By default, the value is TRUE. If FALSE, the missing values of the variable are not printed.
#' @param target_NA boolean - By default, the value is TRUE. If FALSE, the missing values of the target are not printed.
#' @param only_target_ref_level boolean - By default, the value is FALSE. If TRUE, only print the values for the target TRUE.
#' @param lim_y  boolean - By default, the value is TRUE. The axis y for the proportion is limited between 0 and 100.
#' @param numvar_as character, one of "bin" (default),"value": how to display (binned) numeric explanatory variable. 'Bin' will display bins side by side without taking into accounts real values of variable whereas as 'value' will center all bars at bins centers.
#' @param title boolean: do we plot default generated title Y/N or character (override title) - default: TRUE
#' @param do_plot boolean - whether to effectively show the plot or not (internal use to combine plots)
#' @param ... additional parameter
#' @rdname plot.crossvar

#' @importFrom data.table melt
plot.crossvar_categorical <- function(
  x,
  show = c("counts", "props", "index", "woe"),
  type = c("auto", "bars", "line"),
  metadata = NULL,
  print_NA = TRUE,
  target_NA = TRUE,
  only_target_ref_level = FALSE,
  lim_y = TRUE,
  numvar_as = c("bin", "value"),
  title = TRUE,
  do_plot = TRUE,
  ...
) {
  ##option show allows to work on contigent table (counts) or percentage table (props)
  ##option type allows to select the geometry of the graph
  ##option print_NA : if it's equal to false, the values where the variable is NA is not printed
  ##option target_NA: if it's equal to false, the category NA of the target is not printed
  ##option only_target_ref_level : if it's equal to true, we print only the values "TRUE"

  ##test
  assertthat::assert_that(
    inherits(x, "crossvar"),
    msg = "the parameter x must to be an object of class crossvar"
  )
  assertthat::assert_that(
    inherits(show, "character"),
    msg = "the parameter show must to be a character"
  )
  assertthat::assert_that(
    inherits(type, "character"),
    msg = "the parameter type must to be a character"
  )
  assertthat::assert_that(
    inherits(metadata, "data.frame") | is.null(metadata),
    msg = "The parameter metadata must be either NULL (no metadata) or a data.frame"
  )
  assertthat::assert_that(
    inherits(print_NA, "logical"),
    msg = "The parameter print_NA must to be boolean (TRUE/FALSE)"
  )
  assertthat::assert_that(
    inherits(target_NA, "logical"),
    msg = "The parameter target_NA must to be boolean (TRUE/FALSE)"
  )
  assertthat::assert_that(
    inherits(only_target_ref_level, "logical"),
    msg = "The parameter only_target_ref_level must to be boolean (TRUE/FALSE)"
  )
  assertthat::assert_that(
    inherits(lim_y, "logical"),
    msg = "The parameter lim_y must to be boolean (TRUE/FALSE)"
  )

  options(scipen = 999) ## remove scientifical notation in numbers

  ## by default, we take the first values for the variables show and type
  # show = show[1]  ## replaced by using 'severl.ok=FALSE in match.arg
  # type = type[1]  ## replaced by using 'severl.ok=FALSE in match.arg
  ##this command allows to make shortcut during the function call and to be sure to enter the good values
  show <- match.arg(
    show,
    c("counts", "props", "index", "woe"),
    several.ok = FALSE
  )
  type <- match.arg(type, c("auto", "bars", "line"), several.ok = FALSE)

  numvar_as <- match.arg(numvar_as, c("bin", "value"), several.ok = FALSE)
  if (numvar_as == 'value' & x$variable_type != 'numeric') numvar_as <- 'bin'

  if (show == 'woe' & is.null(x$woe)){
    stop("No WOE for this profile object, maybe it is a categorical target?")
  }
  ## Definiton of a default association between the show and the type parameters.
  if (type == "auto") {
    type <- switch(
      show,
      "counts" = "bars",
      "props" = "line",
      "index" = "line",
      "woe" = "bars"
    )
  }

  if (x$target_type == 'binary') {
    target_stats <- x$target_stats
    target_values <- target_stats[['value']]
  }
  if (x$target_type %in% c('binary', 'categorical')) {
    target_ref <- as.character(x$target_reference_level)
    if (is.na(target_ref)) target_ref <- 'NA'
  }

  ## selection of the appropriate tables
  df <- as.data.frame.matrix(x[[show]])

  # df <- df[, which(!colnames(df) %in% c('variable'))]
  # ##reorder the table:
  # df <- df[x$orderlabel,, drop=FALSE]

  # print(df)
  ## add the level with the rownames
  df <- cbind(level = rownames(df), df)

  if (show == 'woe') {
    return(
      plot_woe(
        x,
        metadata = metadata,
        print_NA = print_NA,
        numvar_as = numvar_as
      )
    )
  }

  ## we suppress the first value possible for the target if it's binary
  if (only_target_ref_level == TRUE) {
    # df <- df[-2]
    df <- df[, c('level', target_ref)]
    lim_y = FALSE
  }

  ## we put all values in one column instead of several columns (number of columns: numbers of differents values of the target)
  suppressWarnings(dfm <- data.table::melt(as.data.table(df), id = "level"))
  ## we rename the column by "Target"
  names(dfm)[2] <- "target"
  dfm$target <- as.character(dfm$target) # re-encode as character

  if (show == "props") {
    dfm$value = round(dfm$value * 100, 2)
  }

  if (!target_NA) {
    ## remove any record where target is missing
    dfm <- dfm[dfm$target != "[Missing]", ]
  }
  ## we test if there exists missing on the variable !
  existmissing_var <- any(dfm$level == "[Missing]")

  if (existmissing_var) {
    ## we take only the class [Missing]
    dfm.NA <- dfm[dfm$level == "[Missing]", ]
    ## we suppress the class [Missing]
    dfm <- dfm[dfm$level != "[Missing]", ]
  }

  ##
  if (numvar_as == 'value') {
    # replace labels per bin center values
    numcenters <- x$numcenters
    names(numcenters) <- x$orderlabel
    dfm[['level']] <- as.numeric(numcenters[dfm[['level']]])
  }

  if (x$target_type == 'binary') {
    # prepar objets for legend/colors
    vbreaks <- unique(c(dfm[[2]], '[Missing]'))
    # print(vbreaks)
    vlabels <- vbreaks
    if (only_target_ref_level) {
      vbreaks <- c(target_ref, '[Missing]')
      vcolors <- c('firebrick1', 'black')
      # print(vbreaks)
      vlabels <- vbreaks
      names(vcolors) <- c(target_ref, '[Missing]')
    } else {
      vcolors <- c('#6495ED', 'firebrick1', 'black')
      nontarget <- vbreaks[!vbreaks %in% c(target_ref, '[Missing]')]
      names(vcolors) <- c(nontarget, target_ref, '[Missing]')
    }
    if (!target_NA) {
      ## remove option for NA for target
      vbreaks <- vbreaks[vbreaks != '[Missing]']
      vlabels <- vbreaks
      vcolors <- vcolors[-length(vcolors)]
    }
    # print(vcolors)
  }

  if (!is.numeric(dfm[['level']])) {
    dfm$level <- factor(dfm$level, levels = x$orderlabel)
  }
  ## creation of the graphics
  plotValues <- function(dfm, forNA = FALSE) {
    ## p1: graph for non missing

    p1 <- ggplot2::ggplot(
      dfm,
      ggplot2::aes(x = level, y = value)
    ) +
      ggplot2::theme_bw()

    #p1 <- ggplot2::ggplot(dfm,ggplot2::aes(x = level,y = value))

    # print(type)
    if (type == "bars") {
      ## create bar plot
      p1 = p1 +
        ggplot2::geom_bar(
          stat = "identity",
          ggplot2::aes(fill = target),
          show.legend = !forNA
        ) +
        ggplot2::theme(
          legend.position = "top",
          legend.title = ggplot2::element_blank()
        ) ##change the position of the legend + suppress title legend

      ##change label and color
      if (x$target_type == "binary") {
        if (!forNA) {
          p1 = p1 +
            ggplot2::scale_fill_manual(
              breaks = vbreaks,
              labels = vlabels,
              values = vcolors
            )
        } else {
          p1 = p1 + ggplot2::scale_fill_manual("legend", values = vcolors)
        }
      }

      p1 <- p1 + ggplot2::scale_y_continuous(labels = scales::number)
    } else {
      # print(x$target_type)
      ## lines
      if (forNA) {
        p1 = p1 +
          ggplot2::geom_point(
            ggplot2::aes(group = target, col = target),
            show.legend = FALSE
          )
        if (x$target_type == 'binary') {
          p1 = p1 + ggplot2::scale_colour_manual("legend", values = vcolors)
        }
      } else {
        if (x$target_type == 'binary') {
          nlevels <- length(table(dfm$level))
          # print(dfm)
          # print(nlevels)
          if (nlevels == 1) {
            # print("ok")
            p1 = p1 +
              ggplot2::geom_point(ggplot2::aes(group = 1, col = target)) ##change label and colour
          } else {
            p1 = p1 +
              ggplot2::geom_line(ggplot2::aes(group = target, col = target)) ##change label and colour
          }

          p1 = p1 +
            ggplot2::scale_color_manual(
              breaks = vbreaks,
              labels = vlabels,
              values = vcolors
            )
        }
        p1 <- p1 +
          ggplot2::theme(
            legend.position = "top",
            legend.title = ggplot2::element_blank()
          )
      }
    }

    ## add a label to the y axis
    if (show == "counts") {
      if (forNA == FALSE) {
        p1 = p1 + ggplot2::ylab("N")
      } else {
        p1 <- p1 + ggplot2::theme(axis.title.y = ggplot2::element_blank())
      }
    } else {
      p1 = p1 + ggplot2::ylab("Perc.")
      if (x$target_type == 'binary') {
        ## add the pourcentage of the target

        target_ref_perc <- target_stats[
          target_stats$value == x$target_reference_level,
        ][['perc']]
        p1 = p1 +
          ggplot2::geom_hline(
            yintercept = round((target_ref_perc) * 100, 2),
            linetype = "solid",
            color = "darkorchid"
          )

        x_text <- ifelse(
          is.numeric(dfm$level),
          min(dfm$level, na.rm = TRUE) +
            0.05 *
              (max(dfm$level, na.rm = TRUE) - min(dfm$level, na.rm = TRUE)),
          1
        )

        ##x$target_reference_level

        p1 <- p1 +
          ggplot2::annotate(
            geom = "text",
            y = round(target_ref_perc * 100, 2),
            label = paste0(round(target_ref_perc * 100, 2)),
            x = x_text,
            colour = "darkorchid",
            angle = 0,
            vjust = 1.2,
            size = 3.5
          )
      }
      if (lim_y == TRUE) {
        p1 = p1 + ggplot2::ylim(c(0, 100))
      }
    }

    ## add a label to the x axis : the variable name. If there is a metadata file, it puts the label of the variabe
    if (forNA) {
      p1 <- p1 + ggplot2::xlab(" \n  ")
    } else {
      if (is.null(metadata)) {
        p1 = p1 + ggplot2::xlab(x$varname)
      } else {
        p1 <- p1 + ggplot2::xlab(label(x$varname, metadata))
      }
    }

    ## we put the names for the axis x in the way vertical
    p1 <- p1 +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))

    ##change axis x label
    if (any(c(TRUE, FALSE) %in% unique(dfm[, 1]))) {
      p1 = p1 +
        ggplot2::scale_x_discrete(
          breaks = c("FALSE", "TRUE", "[Missing]"),
          labels = c("FALSE", "TRUE", "MISSING")
        )
    } else if (any(c(0, 1) %in% unique(dfm[, 1]))) {
      p1 = p1 +
        ggplot2::scale_x_discrete(
          breaks = c(0, 1, "[Missing]"),
          labels = c("FALSE", "TRUE", "MISSING")
        )
    }
    invisible(p1)
  }

  ## title for the graphics
  allplots <- plotValues(dfm) # Begin with non-NA values

  if (existmissing_var & print_NA == TRUE) {
    plotNA <- plotValues(dfm.NA, forNA = TRUE)
    allplots <- gridExtra::arrangeGrob(
      allplots,
      plotNA,
      widths = c(8, 2),
      nrow = 1
    )
  } else {
    # Only main plot
    ## add title
    if (is.logical(title)) {
      if (title) {
        str_title <- label(x$targetname, metadata = metadata)
        allplots <- allplots + ggplot2::ggtitle(str_title)
      }
    } else if (is.character(title)) {
      allplots <- allplots + ggplot2::ggtitle(title)
    }
  }
  if (do_plot) plot(allplots)
  allplots
}

#' @title WOE graph for binary/continuous targets
#' @description For binary/continuous targets, WOE (Weight of Evidence) statistics are computed.
#' This function plots a WOE barplot (or lines) for an explanatory variable, based on a crossvar object.
#' @param x a crossvar object, as found in profiles slot of a targeter object.
#' @param metadata data.frame - if metadata is  loaded in R environment, label of the variables can be used. Default value (NULL) corresponds to no metadata available.
#' The label will be used for the title and the x-axis of the graph.
#' @param print_NA boolean. Should we display WOE for NA values of the explanatory variable. Default: TRUE
#' @param numvar_as character. How should continuous explanatory variables be displayed:
#' #'\itemize{
#'\item bin (default) - display WOE for adjacent binning without respecting variable raw values
#'\item value - plot WOE using centers of binning classes, thus respecting variable raw values
#'}
#' @param title boolean: do we plot default generated title Y/N or character (override title) - default: TRUE
#' @param do_plot boolean - whether to effectively show the plot or not (internal use to combine plots)
#' @param ... extra parameters (not used currently)
#' @return plot a graph and returns its ggplot2 object
#' @examples
#' \dontrun{
#' if(interactive()){
#'  t <- crossvar(adult, target='ABOVE50K', var='AGE')
#'  plot_woe(t)
#'  plot_woe(t, numvar_as='value')
#'  }
#' }
#' @seealso
#'  \code{\link[targeter]{crossvar}}
#' @rdname plot_woe
#' @export
#' @importFrom ggplot2 ggplot aes theme_bw geom_bar theme element_blank scale_y_continuous ylim xlab element_text ggtitle
#' @importFrom scales number
#' @importFrom gridExtra grid.arrange
plot_woe <- function(
  x,
  metadata = NULL,
  print_NA = TRUE,
  numvar_as = c('bin', 'value'),
  title = TRUE,
  do_plot = TRUE,
  ...
) {
  # type <- match.arg(type, c("bars","line"), several.ok = FALSE)
  numvar_as <- match.arg(numvar_as, c('bin', 'value'), several.ok = FALSE)

  ## selection of the appropriate tables
  df <- as.data.frame.matrix(x[['woe']])
  ##reorder the table:
  # df <- df[match(x$orderlabel, rownames(df)),, drop=FALSE]
  ## add the level with the rownames
  dfm <- cbind(level = rownames(df), df)

  ## we test if there exists missing on the variable !
  existmissing_var <- any(dfm$level == "[Missing]")

  yrange <- range(dfm$WOE)

  if (existmissing_var) {
    ## we take only the class [Missing]
    dfm.NA <- dfm[dfm$level == "[Missing]", ]
    ## we suppress the class [Missing]
    dfm <- dfm[dfm$level != "[Missing]", ]
  }
  if (numvar_as == 'value' & x$variable_type == 'numeric') {
    # replace labels per bin center values
    dfm[['level']] <- as.numeric(x$numcenters[dfm[['level']]])
  }

  if (!is.numeric(dfm[['level']])) {
    dfm$level <- factor(dfm$level, levels = x$orderlabel)
  }
  ## creation of the graphics
  plotValues <- function(dfm, forNA = FALSE) {
    ## p1: graph for non missing
    ##add order + theme to graphics
    if (x$woe_cluster & !forNA) {
      p1 <- ggplot2::ggplot(
        dfm,
        ggplot2::aes(x = level, y = WOE, fill = as.factor(cluster))
      )
    } else {
      p1 <- ggplot2::ggplot(
        dfm,
        ggplot2::aes(x = level, y = WOE)
      )
    }
    p1 <- p1 + ggplot2::theme_bw()
    ## create bar plot
    p1 = p1 +
      ggplot2::geom_bar(stat = "identity", show.legend = FALSE, group = 1) +
      ggplot2::theme(
        legend.position = "top",
        legend.title = ggplot2::element_blank()
      ) ##change the position of the legend + suppress title legend

    p1 <- p1 + ggplot2::scale_y_continuous(labels = scales::number)
    # p1 <- p1 +  ggplot2::scale_fill_manual(values= color)

    ## format y axis
    suppressMessages(p1 <- p1 + ggplot2::ylim(yrange))

    ## add a label to the x axis : the variable name. If there is a metadata file, it puts the label of the variabe
    if (forNA) {
      p1 <- p1 + ggplot2::theme(axis.title.y = ggplot2::element_blank())
      p1 <- p1 + ggplot2::xlab(" \n  ")
    } else {
      if (is.null(metadata)) {
        p1 = p1 + ggplot2::xlab(x$varname)
      } else {
        p1 <- p1 + ggplot2::xlab(label(x$varname, metadata))
      }
    }

    ## we put the names for the axis x in the way vertical
    p1 <- p1 +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))
    invisible(p1)
  }

  ## title for the graphics
  str_title <- NULL
  if (is.logical(title)) {
    if (title) {
      str_title <- paste(x$targetname, 'explained by', x$varname)
    } else {
      str_title <- NULL
    }
  } else str_title <- title
  allplots <- plotValues(dfm) # Begin with non-NA values

  if (existmissing_var & print_NA == TRUE) {
    plotNA <- plotValues(dfm.NA, forNA = TRUE)
    allplots <- gridExtra::arrangeGrob(
      allplots,
      plotNA,
      widths = c(8, 2),
      nrow = 1
    )
  }
  if (!is.null(str_title)) allplots <- allplots + ggplot2::ggtitle(str_title)

  if (do_plot) plot(allplots)

  allplots
}

#' @title quadrant_plot
#' @description quadrant method for crossvar object, displays explanatory variable categories (or bin) in a quadrant plot crossing category size (N) and target (percentage for binary target or average for continuous target).
#'
#'This function allows to generate a quadrant graphic on an object of class "crossvar".
#' @param x object of class "crossvar"
#' @param metadata data.frame - if metadata is  loaded in R environment, label of the variables can be used. Default value (NULL) corresponds to no metadata available.
#' The label will be used for the title and the x-axis of the graph.
#' @param max_ncat maximum number of values/categories that will be displayed ( additional will be collapsed) - default: 15
#' @param print_NA boolean: whether to display or not the NA possible category (default: TRUE)
#' @param title either boolean or character. If boolean: do we plot (default generated ) title, if character, provided title will be used
#'
#' @return The function returns a graph.
#'
#' @seealso
#' \itemize{
#' \item \code{\link{crossvar}}
#' \item \code{\link{summary.crossvar}}
#' \item \code{\link{plot.crossvar}}
#' }
#' @examples
#' t <- crossvar(adult,"ABOVE50K","WORKCLASS")
#' quadrant_plot(t)
#' quadrant_plot(t, max_ncat=2)
#' @export quadrant_plot
#' @importFrom assertthat assert_that
#' @importFrom data.table setorder rbindlist
#' @importFrom ggplot2 ggplot aes geom_point scale_color_manual theme_bw geom_hline geom_text ggtitle xlab ylab theme element_text scale_x_continuous
#' @importFrom ggrepel geom_text_repel
#' @importFrom scales label_comma
quadrant_plot <- function(
  x,
  metadata = NULL,
  max_ncat = 15,
  print_NA = TRUE,
  title = TRUE
) {
  ##test
  assertthat::assert_that(
    inherits(x, "crossvar"),
    msg = "the parameter x must to be an object of class crossvar"
  )
  assertthat::assert_that(
    inherits(metadata, "data.frame") | is.null(metadata),
    msg = "The parameter metadata must be either NULL (no metadata) or a data.frame"
  )
  assertthat::assert_that(
    requireNamespace('ggrepel', quietly = TRUE),
    msg = 'ggrepel package required'
  )
  assertthat::assert_that(
    requireNamespace('scales', quietly = TRUE),
    msg = 'scales package required'
  )

  assertthat::assert_that(
    x$target_type %in% c('binary', 'numeric'),
    msg = 'quadrant plots work with binary or numeric targets'
  )

  ##recover needed data

  if (x$target_type == 'binary') {
    target_level <- as.character(x$target_reference_level)

    count <- x$counts[, target_level, drop = FALSE]
    colnames(count) <- "N"
    count$names <- rownames(x$count)

    prop <- x$props[, target_level, drop = FALSE]
    prop <- as.data.frame.matrix(prop)
    colnames(prop) <- "Y"
    prop$names <- rownames(x$prop)
    prop$Y <- round(100 * prop$Y, 2)

    ##group all data in one table
    all <- merge(x = count, y = prop, by = "names", all = TRUE)
  } else {
    all <- x$stats[, c('count', 'avg')]
    colnames(all) <- c('N', 'Y')
    all$names <- rownames(all)
  }

  #all$Pourcentage_Mod <- round(all[,2]/x$comptage_cible[2]*100,2)

  if (!print_NA) {
    all <- all[all$names != '[Missing]', ]
  }

  if (nrow(all) > max_ncat) {
    # collapse categories
    data.table::setorder(all, -N)
    top <- all[1:max_ncat, ]
    remaining <- all[-(1:max_ncat), , drop = FALSE]

    if (print_NA) {
      # force NA to be there
      if (!("[Missing]") %in% top$names) {
        top <- data.table::rbindlist(list(top, all[all$names == "[Missing]", ]))
        # remove from remaining
        remaining <- remaining[remaining$names != "[Missing]", ]
      }
    }
    if (nrow(remaining) > 0) {
      ## collapse stats
      remaining_N <- sum(remaining$N)
      remaining_Y <- sum(remaining$Y * remaining$N) / remaining_N
      if (is.nan(remaining_Y)) remaining_Y <- 0 # fix
      collapsed <- data.frame(
        names = "[Other]",
        N = remaining_N,
        Y = remaining_Y
      )
      all <- rbindlist(list(top, collapsed), use.names = TRUE)
    } else {
      all <- top
    }
  }

  all$color <- ifelse(
    all$names == '[Missing]',
    'darkred',
    ifelse(all$names == '[Other]', 'darkblue', 'black')
  )

  col <- as.character(all$color)
  names(col) <- all$color

  ##creation of plot
  p <- ggplot2::ggplot(all, ggplot2::aes(N, Y, label = names, colour = color))
  p1 <- p + ggplot2::geom_point() + ggplot2::scale_color_manual(values = col)

  #p1 <- p + ggplot2::geom_point(ggplot2::aes(size = Pourcentage_Mod))
  ## changement label
  #p1 <- p1 + ggplot2::labs(size="Pourcentage de profils par modalité")
  ##add label to the point. Advantage the function gives the best position for the label
  suppressWarnings(
    p1 <- p1 + ggrepel::geom_text_repel(size = 2.5, max.overlaps = 16)
  )
  ##theme
  p1 <- p1 + ggplot2::theme_bw()

  ##add horizontal line
  if (x$target_type == 'binary') {
    target_perc <- x$target_stats[
      as.character(x$target_stats$value) == target_level,
    ][['perc']]
    target_mean <- round(100 * target_perc, 2)
  } else {
    target_mean <- x$target_stats[['avg']]
  }

  p1 <- p1 +
    ggplot2::geom_hline(
      yintercept = target_mean,
      linetype = "solid",
      color = "darkorchid"
    )
  # p1 <- p1 + ggplot2::geom_text(
  #   ggplot2::aes(y=target_mean,
  #                label=prettyNum(target_mean),
  #                x=min(N)+0.95*(max(N)-min(N))), colour="darkorchid", angle=0, vjust = 1.2, size=3.5)

  ## add title for axis x and axis y
  p1 <- p1 + ggplot2::xlab("N records")

  ylabel <- ifelse(
    x$target_type == 'binary',
    "Target penetration  (%)",
    "Target mean"
  )
  p1 <- p1 + ggplot2::ylab(ylabel)

  ## we put the names for the axis x in the way vertical
  p1 <- p1 +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))

  p1 <- p1 +
    ggplot2::scale_x_continuous(label = scales::label_comma(accuracy = 1))

  ##hide legend
  p1 <- p1 + ggplot2::theme(legend.position = "none")

  ## add title

  str_title <- NULL
  if (is.logical(title)) {
    if (title) {
      str_title <- paste(x$targetname, 'explained by', x$varname)
    } else {
      str_title <- NULL
    }
  } else str_title <- title

  if (!is.null(str_title)) p1 <- p1 + ggplot2::ggtitle(str_title)

  ## return graph
  return(p1)
}


#' @method plot targeter
#' @export
plot.targeter <- function(x, var = names(x$profiles)[1], ...) {
  plot(x$profiles[[var]], ...)
}