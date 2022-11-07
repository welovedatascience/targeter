
autoplot <- function(x,
                     var=NULL,
                     numvar_as=c('value','bin'),
                     metadata = NULL,
                     print_NA =  TRUE,
                     only_target_ref_level = TRUE,
                     lim_y = TRUE,
                     title = TRUE,
                     subtitle = TRUE,
                     ...){
  assertthat::assert_that(inherits(x, what=c('crossvar','targeter')),
      msg='x must be a crossvar or a targeter object')

  numvar_as <- match.arg(numvar_as, choices=c('value','bin'), several.ok = FALSE)

  if (inherits(x, 'targeter')){

    assertthat::assert_that(!is.null(var),
      msg='x being a targeter object, var must be provided')
    assertthat::assert_that(is.character(var),
      msg='var must be a character parameter')


    assertthat::assert_that(var %in% names(x$profiles),
      msg='var is not present in list of x profiles/crossvar')

    x <- x$profiles[[var]]

  }

  # var_label = label(var, metadata = metadata)

  if (x$target_type %in% c('binary','categorical')){
        g1 <- plot(x,
                   metadata=metadata,
                   print_NA=print_NA,
                   numvar_as = numvar_as,
                   title = FALSE,
                   do_plot=FALSE
                   )

        g2 <- plot(x,
                 show="props",
                 type="l",
                 metadata=metadata,
                 print_NA=print_NA,
                 only_target_ref_level =only_target_ref_level,
                 numvar_as = numvar_as,
                 title = FALSE,
                 do_plot=FALSE)

        # g12 <- gridExtra::arrangeGrob(g1, g2, ncol=2)


        g3 <- plot_woe(x,
                       metadata=metadata,
                       title = FALSE,
                       do_plot=FALSE)

        if (x$variable_type %in% c('character')) {
          g4 <- quadrant_plot(x, metadata=metadata, title=FALSE)
          L_quad <- TRUE
        } else {
          g4 <- ggplot() + theme_void()
          L_quad <- FALSE
        }

        # g34 <- gridExtra::arrangeGrob(g3, g4, ncol=2)



        ## add title
        if (is.logical(title)){
          if (title) {
            title <- paste(
              label(x$targetname, metadata=metadata),
              label(x$varname, metadata=metadata),
              sep = ' explained by: '
            )
          }
          else title <- NULL

        }
        if (is.logical(subtitle)){
          if (subtitle) {
            subtitle <- 'Frequencies (N), percentage/target pen (%), WOE'
            if (L_quad) subtitle <- paste(subtitle, ', quadrant (N/%)')
          }
          else subtitle <- NULL

        }


        allplots <- patchwork::wrap_plots(g1, g2, g3, g4)
        allplots <- allplots  + patchwork::plot_annotation(
          title = title,
          subtitle= subtitle
      )

  }  else if (x$target_type %in% ('numeric')){
    plot.crossvar_numeric(x,...)
  } else stop('target type not handled by autoplot')

  return(allplots)

}
