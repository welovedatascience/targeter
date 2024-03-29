
# to prevent checks of data.table used variables
# see:  ?globalVariables
if(getRversion() >= "3.1.0") utils::globalVariables("..cn")



#' @title explore
#' @description shiny small app to explore a targeter object.
#' @param object a targeter object
#' @param summary_object optional: pre-computed summary object
#' (see \code{\link{summary.targeter}}), Default: NULL
#' @param metadata optional: metadata to use to display variables labels,
#' default: NULL
#' @param display character, one of 'dialog', 'browser':
#' how to open the shiny app/gadget (default: dialog)
#' @param ... additional parameters to be used by summary.targeter
#' function (when no summary object are passed)
#' @return NULL (side effect: open gadget)
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   x <- targeter(adult, target = "ABOVE50K")
#'   explore(x)
#' }
#' }
#' @seealso
#'  \code{\link[targeter]{report}}
#' @rdname explore
#' @export
#' @importFrom miniUI miniPage gadgetTitleBar miniButtonBlock
#' miniTitleBarButton miniContentPanel
#' @importFrom shiny fluidRow column plotOutput renderPlot
#' observeEvent runGadget dialogViewer stopApp
#' @importFrom DT DTOutput renderDataTable
#' @importFrom gridExtra grid.arrange
#' @importFrom shinybusy show_modal_spinner
#' @importFrom htmlwidgets JS
explore <- function(
    object,
    summary_object = NULL,
    metadata = NULL,
    display = c("dialog", "browser"),
    ...) {
  display <- match.arg(display, c("dialog", "browser"), several.ok = FALSE)

  if (is.null(summary_object)) {
    summary_object <- summary(object, ...)
  }
  ui <- miniUI::miniPage(
    shiny::tags$head(
      shiny::tags$style(shiny::HTML(
        ".excluded { color: rgb(211,211,211); font-style: italic; }"
      ))
    ),
    miniUI::gadgetTitleBar("targeter explorer",
      left = miniUI::miniButtonBlock(
        miniUI::miniTitleBarButton("report_html", "html"),
        miniUI::miniTitleBarButton("report_pdf", "pdf"),
        miniUI::miniTitleBarButton("report_word", "word")
      ),
      right = miniUI::miniTitleBarButton("done", "Stop", primary = TRUE)
    ),
    miniUI::miniContentPanel(
      shiny::fluidRow(shiny::column(width = 12, DT::DTOutput("dt_summary"))),
      shiny::fluidRow(
        shiny::column(width = 6, shiny::plotOutput(("plot_1"))),
        shiny::column(width = 6, shiny::plotOutput(("plot_2")))
      )
    )
  )
  ## server ------

  server <- function(input, output, session) {
    # Define reactive expressions, outputs, etc.
    output$dt_summary <- DT::renderDataTable(
      {
        if (object$target_type == "binary") {
          df2 <- as.data.frame(summary_object[
            ,
            c(
              "varname", "vartype", "IV", "index.max.level", "index.max.count",
              "index.max.props", "index.max.index"
            )
          ])
          df2$index.max.index <- round(df2$index.max.index, 2)
          df2$index.max.props <- round(df2$index.max.props * 100, 2)
          colnames(df2) <- c("variable", "type", "IV", "Level",
          "#Records", "%Records", "Index")
        } else {
          df2 <- summary_object
          cn <- colnames(df2)
          cn <- cn[!(cn %in% c("targetname"))]
          df2 <- df2[, ..cn]
        }
        df2$IV <- round(df2$IV, 3)
        if (!is.null(metadata)) {
          df2 <- cbind(data.frame(
            Label = sapply(df2$variable, label, metadata = metadata),
            stringsAsFactors = FALSE), df2)
        }

        ## https://www.r-bloggers.com/2019/06/useful-callbacks-for-dt-in-shiny/
        rowNames <- FALSE # whether to show row names in the table
        colIndex <- as.integer(rowNames)

        callback <- c(
          sprintf("table.on('click', 'td:nth-child(%d)', function(){",
          colIndex + 1),
          "  var td = this;",
          "  var cell = table.cell(td);",
          "  if(cell.data() === 'ok'){",
          "    cell.data('remove');",
          "  } else {",
          "    cell.data('ok');",
          "  }",
          "  var $row = $(td).closest('tr');",
          "  $row.toggleClass('excluded');",
          "  var excludedRows = [];",
          "  table.$('tr').each(function(i, row){",
          "    if($(this).hasClass('excluded')){",
          "      excludedRows.push(parseInt($(row).attr('id').split('_')[1]));",
          "    }",
          "  });",
          "  Shiny.setInputValue('excludedRows', excludedRows);",
          "})"
        )
        restore <- c(
          "function(e, table, node, config) {",
          "  table.$('tr').removeClass('excluded').each(function(){",
          sprintf("    var td = $(this).find('td').eq(%d)[0];", colIndex),
          "    var cell = table.cell(td);",
          "    cell.data('ok');",
          "  });",
          "  Shiny.setInputValue('excludedRows', null);",
          "}"
        )
        render <- c(
          "function(data, type, row, meta){",
          '  if(type === "display"){',
          '    var color = data === "ok" ? "forestgreen" : "red";',
          '    return "<span style=\\\"color:" + color +',
          '         "; font-size:18px\\\"><i class=\\\"glyphicon glyphicon-" +',
          '         data + "\\\"></i></span>";',
          "  } else {",
          "    return data;",
          "  }",
          "}"
        )

        dat <- cbind(Selected = "ok", df2, 
          id = paste0("row_", seq_len(nrow(df2))))

        DT::datatable(dat,
          rownames = rowNames,
          filter = "top",
          autoHideNavigation = TRUE,
          extensions = c("Select", "Buttons"),
          selection = "single",
          callback = htmlwidgets::JS(callback),
          options = list(
            pageLength = 6,
            rowId = htmlwidgets::JS(sprintf(
              "function(data){return data[%d];}",
              ncol(dat) - 1 + colIndex
            )),
            columnDefs = list(
              list(visible = FALSE, targets = ncol(dat) - 1 + colIndex),
              list(className = "dt-center", targets = "_all"),
              list(className = "notselectable", targets = colIndex),
              list(targets = colIndex, render = htmlwidgets::JS(render))
            ),
            dom = "tp",
            buttons = list(
              "copy", "csv",
              list(
                extend = "collection",
                text = "Select all",
                action = htmlwidgets::JS(restore)
              )
            ),
            select = list(
              style = "single",
              selector = "td:not(.notselectable)"
            )
          )
        )
      },
      server = FALSE
    )
    #
    output$plot_1 <- shiny::renderPlot({
      if (!is.null(input$dt_summary_row_last_clicked)) {
        var <- summary_object[input$dt_summary_row_last_clicked, ][["varname"]]
        iprofile <- object$profiles[[var]]

        if (iprofile$target_type %in% c("binary", "categorical")) {
          g1 <- plot.crossvar(iprofile,
            metadata = metadata,
            print_NA = TRUE,
            numvar_as = "value"
          )

          g2 <- plot.crossvar(iprofile,
            show = "props",
            type = "l",
            metadata = metadata,
            print_NA = TRUE,
            only_target_ref_level = TRUE,
            numvar_as = "value"
          )
        } else {
          # numeric target
          g1 <- plot.crossvar(iprofile,
            show = "boxplot",
            metadata = metadata,
            print_NA = TRUE,
            numvar_as = "value"
          )

          g2 <- plot.crossvar(iprofile,
            show = "count",
            type = "b",
            metadata = metadata,
            print_NA = TRUE,
            numvar_as = "value"
          )
        }

        gridExtra::grid.arrange(g1, g2, ncol = 2)
      }
    })

    output$plot_2 <- shiny::renderPlot({
      if (!is.null(input$dt_summary_row_last_clicked)) {
        var <- summary_object[input$dt_summary_row_last_clicked, ][["varname"]]
        iprofile <- object$profiles[[var]]
        g3 <- plot_woe(iprofile, metadata = metadata)

        g4 <- ggplot2::ggplot() +
          ggplot2::theme_void()
        if (iprofile$target_type == "binary") {
          if (iprofile$variable_type %in% c("character")) {
            g4 <- quadrant_plot(iprofile, metadata = metadata)
          }
        }
        if (iprofile$target_type == "numeric") {
          g4 <- plot.crossvar(iprofile, show = "avg",
            type = "l", print_NA = TRUE)
        }
        gridExtra::grid.arrange(g3, g4, ncol = 2)
      }
    })

    # When the Done button is clicked, return a value
    shiny::observeEvent(input$done, {
      returnValue <- "Done"
      shiny::stopApp(returnValue)
    })

    shiny::observeEvent(input$report_html, {
      selvars <- summary_object[
        setdiff(1:nrow(summary_object), input[["excludedRows"]]),][["varname"]]
      if (length(selvars) > 1) {
        summary_object <- summary_object[summary_object$varname %in% selvars, ]
        # S <<- summary_object
        shinybusy::show_modal_spinner() # show the modal window
        targeter::report(
          object = object,
          template = NULL,
          summary_object = summary_object,
          browse = TRUE,
          ntop = 0,
          metadata = metadata,
          force_vars = selvars,
          output_format = "html"
        )
        shinybusy::remove_modal_spinner()
      } else {
        shinybusy::report_failure(
          title = "Problem",
          text = "No variable selected")
      }
    })

    shiny::observeEvent(input$report_pdf, {
      selvars <- summary_object[
        setdiff(1:nrow(summary_object), input[["excludedRows"]]),][["varname"]]
      if (length(selvars) > 1) {
        summary_object <- summary_object[summary_object$varname %in% selvars, ]
        shinybusy::show_modal_spinner() # show the modal window
        targeter::report(
          object = object,
          template = NULL,
          summary_object = summary_object,
          browse = TRUE,
          ntop = 0,
          metadata = metadata,
          force_vars = selvars,
          output_format = "pdf"
        )
        shinybusy::remove_modal_spinner()
      } else {
        shinybusy::report_failure(
          title = "Problem",
          text = "No variable selected")
      }
    })


    shiny::observeEvent(input$report_word, {
      selvars <- summary_object[
        setdiff(1:nrow(summary_object),input[["excludedRows"]]), ][["varname"]]
      if (length(selvars) > 1) {
        summary_object <- summary_object[summary_object$varname %in% selvars, ]
        shinybusy::show_modal_spinner() # show the modal window
        targeter::report(
          object = object,
          template = NULL,
          summary_object = summary_object,
          browse = TRUE,
          ntop = 0,
          metadata = metadata,
          force_vars = selvars,
          output_format = "word"
        )
        shinybusy::remove_modal_spinner()
      } else {
        shinybusy::report_failure(
          title = "Problem",
          text = "No variable selected")
      }
    })
  }

  display <- match.arg(display, c("dialog", "browser"))

  shiny::runGadget(ui, server, viewer = shiny::dialogViewer("targeter-explore",
  width = 2000, height = 1800))
  # shiny::runGadget(ui, server, viewer = shiny::browserViewer())
}
