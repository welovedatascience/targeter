tbind <- function(list_targeters, check=TRUE){
  assertthat::assert_that(
    is.list(list_targeters),
    msg = 'list_targeter must be list of targeter objects')

  if (check){
    is_targeter <- sapply(list_targeters, function(el) inherits(el, "targeter"))
    assertthat::assert_that(
      all(is_targeter),
      msg="list_targeters contains elements that are not targeter objects")
  }

    n <- length(list_targeters)
  assertthat::assert_that(n>0, msg="list_targeter list is empty")
  all <- unlist(list_targeters, recursive = FALSE)

  


  #https://stackoverflow.com/questions/18538977/combine-merge-lists-by-elements-names
  all <- list_targeters[[1]]
  all$profiles <- do.call(mapply, c(FUN=c, lapply(list_targeters, `[`, 'profiles')))
  names(all$profiles) <-   unlist(sapply(list_targeters, function(t) names(t$profiles)))

  return(all)
}
