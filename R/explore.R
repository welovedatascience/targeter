# library(shiny)
# library(miniUI)
# library(ggplot2)
explore <- function(object, summary_object=NULL,metadata=NULL, ...) {

  if (is.null(summary_object)){
    summary_object <- summary(object,...)
  }
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("targeter explorer"),
    miniUI::miniContentPanel(
      shiny::fluidRow(shiny::column(width=6,
                     ## DT
                     DT::DTOutput('dt_summary')
                     ),

               shiny::column(width=6,
                      shiny::fluidRow(plotOutput(('plot_1'))),
                      shiny::fluidRow(plotOutput(('plot_2'))))
      )
    )
  )

  server <- function(input, output, session) {
    # Define reactive expressions, outputs, etc.
    output$dt_summary <- DT::renderDataTable({

      if (object$target_type == 'binary'){
        df2 <- as.data.frame(summary_object[,
                                            c("varname","vartype","IV","index.max.level","index.max.count",
                                              "index.max.props","index.max.index")])
        df2$index.max.index = round(df2$index.max.index,2)
        df2$index.max.props = round(df2$index.max.props *100, 2)
        colnames(df2) <- c("variable","type","IV","Level","#Records","%Records","Index")
      } else {
        df2 <- summary_object
        cn <- colnames(df2)
        cn <- cn[!(cn %in% c('targetname'))]
        df2 <- df2[, ..cn]
      }
      df2$IV <- round(df2$IV,3)
      if (!is.null(metadata)){
        df2 <- cbind(data.frame(Label=sapply(df2$variable,label, metadata=metadata),stringsAsFactors = FALSE),df2)
      }
      DT::datatable(df2, rownames = TRUE, filter="top",

                    extensions = "Select", selection = "multiple",
                      autoHideNavigation = TRUE,
                    options=list(
                      columnDefs = list(
                        list(targets = 0, orderable = FALSE, className = "select-checkbox")),
                        select = list(
                          style = "multi", selector = "td:first-child"
                        ),
                      pageLength=15,deferRender = TRUE))
    }, server=FALSE
    )


    output$plot_1 <- shiny::renderPlot({
      if (!is.null(input$dt_summary_row_last_clicked)){
        var <- summary_object[input$dt_summary_row_last_clicked,][['varname']]
        iprofile <- object$profiles[[var]]
        g1 <- plot.crossvar(iprofile,
                            metadata=metadata,
                            print_NA=TRUE,
                            numvar_as = 'value')

        g2 <- plot.crossvar(iprofile,
                      show="props",
                      type="l",
                      metadata=metadata,
                      print_NA=TRUE,
                      only_target_ref_level =TRUE,
                      numvar_as = 'value')

        gridExtra::grid.arrange(g1, g2, ncol=2)


      }
    })

    output$plot_2 <- shiny::renderPlot({
      if (!is.null(input$dt_summary_row_last_clicked)){
        var <- summary_object[input$dt_summary_row_last_clicked,][['varname']]
        iprofile <- object$profiles[[var]]
        g3 <- plot_woe(iprofile, metadata=metadata)

        if (iprofile$variable_type %in% c('character')) {
          g4 <- quadrant_plot(iprofile, metadata=metadata)
        } else {
          g4 <- ggplot() + theme_void()

        }
        gridExtra::grid.arrange(g3, g4, ncol=2)


      }
    })


    # When the Done button is clicked, return a value
    shiny::observeEvent(input$done, {
      returnValue <- "Done"
      stopApp(returnValue)
    })
  }

  # runGadget(ui, server, viewer = dialogViewer("targeter-explore", width=2000, height=1600))
  miniUI::runGadget(ui, server, viewer = miniUI::browserViewer())
}
