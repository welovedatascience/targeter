targeter_big <- function(data, target, by_nvars = 250, verbose = TRUE, ...) {
  if (ncol(data) > by_nvars) vars <- names(data)
  by_groups <- (1 + seq_along(vars) %/% by_nvars)
  groups <- unique(by_groups)
  if (verbose) cat("\nNumber of groups to be processed:", length(groups))
  out_tar <- vector(mode = 'list', length(groups))

  for (igroup in groups) {
    if (verbose) {
      cat('\nProcessing group:', igroup, " over ", length(groups))
    }
    # TMP_I <<- igroup
    ivars <- vars[by_groups == igroup]
    ivars <- unique(c(target, ivars))
    data_igroup <- data[, ..ivars]
    # TMP_DATA <<- data_igroup
    itar <- try(targeter::targeter(
      data_igroup,
      target = target,
      verbose = FALSE,
      ...
    ))
    if (!inherits(itar, "try-error")) {
      out_tar[[igroup]] <- itar
      cat(' - Done.')
    } else {
      cat(" - Nothing to do")
      out_tar[[igroup]] <- NULL
    }
  }
  out_tar <- out_tar[sapply(out_tar, function(check) !is.null(check))]

  tar <- targeter:::tbind(out_tar)
  return(tar)
}
