#' @title targeter
#' @description For each variable, the function crosses two variables: a target to be explained and an explanatory variable.
#'For this purpose, these variables are converted in categorical variables by a binning process and the statistics
#'are derived.
#'\itemize{
#'\item The contingency table gives the counts of each class of the explanatory variable for each modality of the target.
#'\item A proportion is calculated as the count of profiles per class and target modality is divided by the sum of profiles by class (=count/sum of row counts).
#'\item An index is calculated as the proportion of profiles per class and modality divided by the proportion of the modality of all profiles (=proportion / (sum of column counts/sum of all counts)).
#'\itemize{
#'\item If the value of the index is high (more than one), it implies that
#'for this subpopulation is over-represented for this variable.
#'\item If the value of the index is equal to 1, the criterium is not significant
#'\item If the value of the index is less than 1, it implies that
#'for this subpopulation is sub-represented for this variable.
#'}
#'\item Weight of Evidence and Information Value are derived for binary and continuous targets.
#'}
#'
#' @param data data - data.table or data.frame.
#' @param description_data text on the description of data.
#' @param target character - name of the variable to explain.
#' @param target_type character: type of target - one of 'autoguess' (default), 'binary','categorical' (>2 modalities) or 'numeric'.
#' ExpParameter expansion if applied so that one could also use 'a' or 'b','c' or 'n'
#' @param target_reference_level character or numeric. For categorical or (especially) binary targets, level / value of special
# interest. If `NULL``default` one will try to infer from target content. Typically, this would be values such as
#' TRUE or 1 or 'bad' for binary targets.
#' that
#' @param description_target text on the description of target.
#' @param analysis_name  name of the analysis.
#' @param select_vars  a list of explanatory variables. By default, NULL and all columns are considered.
#' @param exclude_vars  a list of variables to exclude from the analysis.
#' @param naming_conventions boolean - by default TRUE. It means that a certain naming convention is respected.
#' @param nbins The nbins is by default 10. It is the number of quantiles to be considered.
#' @param binning_method character, one of 'quantile' (default) or 'clustering', 'smart' (parameter expansion usable).
#' Method used to derive the `nbins` buckets for the continuous explanatory variables.
#' @param useNA Two values are possible : "ifany" and "no". By default, the package option is "ifany".
#' \itemize{
#' \item The value "ifany" takes in consideration the missing values if there are any.
#' \item The value "no" doesn't take in consideration the missing values in any case.
#' }
#' @param verbose - boolean (default FALSE). If TRUE some more information is displayed in console. Could be useful
#' when using package on big data.
#' @param dec - integer : the number of decimal for numeric variable. By default, the value is 2.
#' @param order_label character - this option output an order used for the plot function. The parameter can only take the following values:
#' \itemize{
#' \item "auto": for the numeric variable, the order is alphabetic. For the categorical variable, the order is the count
#' \item "alpha": order alphabetic
#' \item "count": order decreasing of the number of observations by class
#' \item "props : order decreasing by the proportion of the second value of the target
#' \item "means : order decreasing by the target mean (continuous target)
#' }
#' @param cont_target_trim numeric (default 0.01). For continuous targets, it is desirable to trim
#' extreme values before computing WOE. This is the trimming factor in percentage (between 0: no trim and <1).
#' @param bxp_factor (default) 1.5 for continuous target, coefficient to be used to compute boxplot whiskers.
#' @param num_as_categorical_nval (default: 5). If a variable has less than num_as_categorical_nval distinct values,
#' it will be considered and used as categorical and not numeric.
#' @param autoguess_nrows - integer (default 1000). Numbers of rows to be used to guess the variables types.
#' Special value 0 could be provided and then all rows be be used.
#' @param woe_alternate_version character. Specify in which context WOE alternate definition will be used
#' See vignette on  methodology. Possible values are 'if_continuous' (default) or 'always'.
#' @param woe_shift numeric (default) 0.01. Shifting value in WOE computation to prevent issues when one bucket
#' contains 0\% or 100\% of target. For binary target, some people also propose to use 0.5.
#' @param woe_post_cluster boolean (default FALSE). Once WOE are computed, on could cluster values to see if this
#'  this could be adequate to group together some modalities/buckets. Clusters would be used in graphics. See vignette on methodology.
#' @param woe_post_cluster_n integer (default: 6). If woe_post_cluster is TRUE, number of clusters to be used.
#' @param smart_quantile_by numeric, for binning method 'smart', quantile step - default y step of 0.01.


#'
#'
#' @return The function returns a list of class "targeter".
#' This list is composed of this following elements:
#' \itemize{
#' \item dataname - name of the analyzed dataset.
#' \item description_data text about the description of data
#' \item target - name of the target.
#' \item target_type - Target type, one of autoguess (default)/binary/numeric/categorical (text is expended so "b" also works for binary)
#' \item description_target text about the description of target
#' \item analysis - name of the analysis.
#' \item date - date of the analysis.
#' \item profiles : list of elements containing the result the individual crossing per variable.
#' See crossvar class documentation.
#' }
#'
#' @export
#' @seealso
#' \itemize{
#' \item \code{\link{summary.targeter}}
#' \item \code{\link{plot.crossvar}}
#' \item \code{\link{summary.crossvar}}
#' \item \code{\link{report}}
#' }
#' @examples
#' targeter(adult,target ="ABOVE50K")
#' @importFrom stats quantile median
#' @importFrom data.table dcast


## <idea> check for WOE monotonicity
targeter <- function(data,
                     description_data =NULL,
                     target,
                     target_type = c("autoguess","binary","categorical","numeric"),
                     target_reference_level=NULL, # NULL: auto will check for 1, TRUE
                     description_target = NULL,
                     analysis_name=NULL,
                     select_vars=NULL,
                     exclude_vars=NULL,
                     nbins=12,
                     binning_method=c("quantile","clustering","smart"), # todo: tree (min size)+ constrained clustrering  https://cran.r-project.org/web/packages/scclust/scclust.pdf
                     naming_conventions=getOption("targeter.use_naming_conventions"),
                     useNA = getOption("targeter.useNA"), #option package by default
                     verbose=FALSE,
                     dec = 2,
                     order_label = c("auto","alpha","count","props","means"),
                     cont_target_trim=0.01,
                     bxp_factor=1.5,
                     num_as_categorical_nval = 5,
                     autoguess_nrows = 1000, # 0: use all rows
                     woe_alternate_version = c("if_continuous","always"),
                     woe_shift=0.01,
                     woe_post_cluster=FALSE,
                     woe_post_cluster_n=6,
                     smart_quantile_by=0.01
){

  ##test
  assertthat::assert_that(inherits(data,"data.frame") | inherits(data, "data.table"), msg = "Data must to be a data.frame or a data.table")
  assertthat::assert_that(inherits(target,"character"), msg = "Target must to be a character")
  assertthat::assert_that(target %in% colnames(data), msg = "Target doesn't exist in the dataset")
  assertthat::assert_that(length(target)==1,msg="Only one target is admitted")
  assertthat::assert_that(length(unique(data[[target]])) > 1, msg = "The target contains only one value.")
  assertthat::assert_that(inherits(select_vars,"character")| is.null(select_vars), msg = "select_vars must to be a character")
  assertthat::assert_that(inherits(exclude_vars,"character")| is.null(exclude_vars), msg = "exclude_vars must to be a character")
  assertthat::assert_that(inherits(analysis_name,"character")|is.null(analysis_name), msg = "analysis_name must to be a character")
  assertthat::assert_that(inherits(naming_conventions,"logical"),msg = "The parameter naming_convetions accepts only TRUE and FALSE like values")
  assertthat::assert_that(inherits(verbose,"logical"),msg="The parameter verbose accepts only TRUE and FALSE like values")
  assertthat::assert_that(inherits(description_target,"character")|is.null(description_target), msg = "description_target must to be a character or NULL")
  assertthat::assert_that(inherits(description_data,"character")|is.null(description_data), msg = "description_data must to be a character or NULL")
  assertthat::assert_that(inherits(dec,"integer")| inherits(dec,"numeric"),msg ="The parameter dec must to be an integer")
  assertthat::assert_that(inherits(order_label,"character"), msg = "The parameter order_label must to be character")
  assertthat::assert_that(inherits(useNA,"character"), msg = "The parameter UseNA must to be character")
  assertthat::assert_that(inherits(nbins,"numeric")|inherits(nbins,"integer"), msg = "The parameter nbins must to be numeric")
  ## <todo>conditions on other parameters to be added : cont_target_trim....
  ##retrieve the name of the data
  dataname <- deparse(substitute(data))
  data <- data.table::setDT(data)
  msg <- vector(mode='list')

  # <20210322> rename target if variable name=="target" (trouble with dt get(target))
  if (target=="target"){
    msg <- c(msg, c("INFO"="Silently renaming reserved word target as `...target`"))
    setnames(data,"target","...target")
    target <- "...target"
  }
  target_type <- match.arg(target_type, c("autoguess","binary","categorical","numeric"), several.ok = FALSE)
  woe_alternate_version <- match.arg(woe_alternate_version,c("if_continuous","always"), several.ok = FALSE)

  ##analysis name
  if (is.null(analysis_name)) analysis_name <- paste("Analysis of ", target, "on data:", dataname)

  ## By default, the variable NA take the value "ifany"
  useNA <- useNA[1]
  ## the variable useNA can only  accept the two values
  ##<todo>: introduce a yes or always
  useNA <- match.arg(useNA, c("ifany", "no"), several.ok = FALSE)

  binning_method <- match.arg(binning_method,c("quantile","clustering","smart"),several.ok = FALSE)
  if (binning_method=="clustering") assertthat::assert_that(requireNamespace('Ckmeans.1d.dp', quietly = TRUE), msg = 'Ckmeans.1d.dp package required for clustering method.')
  if (woe_post_cluster) {
    assertthat::assert_that(requireNamespace('Ckmeans.1d.dp', quietly = TRUE), msg = 'Ckmeans.1d.dp package required for WOE post clustering.')
    assertthat::assert_that(requireNamespace('clustering.sc.dp', quietly = TRUE), msg = 'clustering.sc.dp package required for WOE post clustering.')

  }
  ## By default the variable order_label is equal to auto
  # order_label <- order_label[1]
  ## the variable order_lable can only accept this following values
  order_label <- match.arg(order_label,c("auto","alpha","count","props","means"),several.ok = FALSE)
  # cat("\n",order_label ,"\n")

  ## list all variables to cross with

  if(is.null(select_vars)){
    select_vars <- names(data)
    if(naming_conventions == TRUE){
      select_vars <- select_vars[substr(select_vars,1,2) %in% c("L_","N_","M_","F_","J_", "C_","R_","P_")]
    }
  }

  ## exclude target in all ways
  select_vars <- select_vars[select_vars != target]
  ## exclude specific variables in input of the function
  select_vars <- select_vars[!(select_vars %in%  exclude_vars)]

  ## check1:  columns name in colnames(data)
  check <- select_vars%in% colnames(data)
  if(any(!check)){
    vars <- select_vars[!check]
    varlist <- paste(vars, collapse = " ")
    msg <- c(msg,
             list(WARNING=
                    paste("Some variables not present in dataframe -
                          they are removed from analyses:",varlist)))
    select_vars <- select_vars[check]
  }


  ## check 2 (if naming convention): do we respect naming conventions
  if(naming_conventions){
    check <- !check_naming_conventions(data[,..select_vars])[,1] ## first check on LETTER_
    if(any(!check)){
      vars <- select_vars[!check]
      varlist <- paste(vars, collapse = " ")
      msg <- c(msg,
               list(
                 WARNING=paste("Some variables do not respect naming conventions -
                               they are removed from analyses:",varlist)))
      select_vars <-select_vars[check]
    }}


  ## ensuring correct variables names (R valid for names in profiles slot + unique)
  # explanatory variables
  cn <- select_vars
  nn <- make.names(cn, unique = TRUE)
  if (any(cn !=nn)){
    old_names <- cn[cn!=nn]
    new_names <- nn[cn!=nn]
    positions <- which(cn!=nn)
    info <- paste(cn[positions],nn[positions], sep='->', collapse = '\t')
    msg <- c(msg, list(WARNING=paste(
      "silently changing some variables names to be valid unique R names:", info,'\n\tPlease note it might lead to issues with metadata and that unique valid R names should be used (see make.names(x, unique=TRUE))'))
    )
    colnames(data)[positions] <- new_names  # we don't use setnames as we might have duplicated names, setnames would raise a warning and not necessary do what we want
    select_vars <- nn
  }
  # target
  cn <- target
  nn <- make.names(target)
  if (any(cn !=nn)){
    old_names <- cn[cn!=nn]
    new_names <- nn[cn!=nn]
    info <- paste(cn,nn, sep='->', collapse = '\t')
    msg <- c(msg, list(WARNING=paste(
      "silently changing target names to be valid  R name:", info, '\n\tPlease note it might lead to issues with metadata and that unique valid R names should be used (see make.names(x, unique=TRUE))'))
    )
    setnames(data, old_names, new_names)
    target <- nn
  }


  ## target type
  if (target_type == "autoguess"){
    target_type <- dt_vartype_autoguess_onevar(data, target, num_as_categorical_nval)
    if (target_type=="unimode") {
      msg <- c(msg, list(ERROR="target has a unique value"))
      ## display messages and stop
      cat(paste(names(msg),msg, sep=":" , collapse = "\n"))
      stop()
    }
    if (target_type=="unknown") {
      msg <- c(msg, list(ERROR="target has an unknown type"))
      ## display messages and stop
      cat(paste(names(msg),msg, sep=":" , collapse = "\n"))
      stop()
    }
    msg <- c(msg, list(INFO=paste("target",target,"detected as type:", target_type)))
  }

  if (target_type=='binary' & is.null(target_reference_level)){
    cl <- class(data[[target]])[1]
    if (cl=='logical'){
      target_reference_level <- TRUE
      msg <- c(msg, list(INFO=paste("target is logical, automatic chosen level: TRUE; override using `target_reference_level`")))

    } else if (cl %in% c('numeric','integer')) {
      target_reference_level <- 1
      msg <- c(msg, list(INFO=paste("binary target contains number, automatic chosen level: 1; override using `target_reference_level`")))

    } else if (cl %in% c('factor','character')){
      target_reference_level <- data[1, ][[target]]
      msg <- c(msg, list(INFO=paste("target is character/factor, automatic chosen level: ", target_reference_level,"; override using `target_reference_level`")))

    }
  }
  if (target_type=='categorical' & is.null(target_reference_level)){
    target_reference_level <- data[[target]][1] # totally arbitrary
    msg <- c(msg, list(INFO=paste("target is categorical. As no target_reference_level was provided, one value is taken arbitrary:",target_reference_level," override using `target_reference_level`")))
  }


  ##separation numeric/character variables

  ## numeric variables
  if(naming_conventions == TRUE){
    num_vars <- select_vars[substr(select_vars,1,2) %in% c("N_","M_","F_","J_","R_","P_")]
  } else {
    if (autoguess_nrows==0) autoguess_nrows <- nrow(data)
    autoguess_nrows <- min(autoguess_nrows, nrow(data))
    data_types <- dt_vartype_autoguess(data[1:autoguess_nrows,..select_vars], num_as_categorical_nval)
    cl <- sapply(data[,..select_vars], function(x)class(x)[1])
    to_convert <- names(data_types)[data_types!="numeric" & (cl %in% c("numeric","integer"))]
    # print(cl)
    # print(data_types)
    # print(to_convert)
    if (length(to_convert)>0){
      for (ivar in to_convert){
        if (verbose){cat("\nconverting:", ivar, " as character.")}
        data[,(ivar):=as.character(get(ivar))]

      }
    }
    num_vars <- names(data_types)[data_types == "numeric"]

  }
  ## character/catogerical/other variables
  other_vars <- select_vars[!(select_vars %in% num_vars)]

  # check: remaining variables (after naming conventions)
  if (length(c(num_vars, other_vars))==0){
    cat("\n")
    cat(paste(names(msg),msg, sep=":" , collapse = "\n"))
    cat("\n")
    stop("\nNo explanatory variable remaining. Check variables and naming conventions if used.")
  }



  ##use to stock the values of the quantiles for each variable
  cutpoints_list <- vector(mode="list", length=length(num_vars))
  names(cutpoints_list) <- num_vars
  cutcenter_list <- vector(mode="list", length=length(num_vars))
  names(cutcenter_list) <- num_vars
# print(cutcenter_list)

  ##function to cut in classes.
  ## binning > quantile ----
  binning_quantile <- function(x, nbins, variable) {
    ## put a vector from 0 to 1 by 1/nbins
    quantiles = seq(0, 1, length.out = nbins+1)
    ## take the value of the quantile for the variable x
    cutpoints = unname(unique(stats::quantile(x, quantiles, na.rm = TRUE)))

    centers <- cutpoints[-length(cutpoints)]+diff(cutpoints/2)
    cutcenter_list[[variable]] <<- centers

    ## values of the quantiles for the variables
    cutpoints_list[[variable]] <<- cutpoints
    ## find the interval containing each element of x in cutpoints
    findInterval(x,cutpoints, rightmost.closed=TRUE)
  }

  ## binning > clustering ----
  binning_clustering <- function(x, nbins, variable){

    cl_centers <- Ckmeans.1d.dp::Ckmeans.1d.dp(x[!is.na(x)], k=nbins)$centers
    cutcenter_list[[variable]] <<- cl_centers
    cutpoints <- sort(unique(c(cl_centers[-length(cl_centers)]+diff(cl_centers/2), range(x, na.rm=TRUE))))
    cutpoints_list[[variable]] <<- cutpoints
    ## find the interval containing each element of x in cutpoints
    findInterval(x,cutpoints, rightmost.closed=TRUE)

  }
  ## binning > smart ----
  binning_smart <- function(x, nbins, variable){
    # if (verbose)cat("\n smart binning:", variable)
    quantiles <- seq(0, 1, length.out = 1+round(1/smart_quantile_by))
    nqu <- length(quantiles)
    qu_indices <- (1:nqu) %/% (nqu/nbins)
    ## take the value of the quantile for the variable x
    qu <- quantile(x, quantiles, na.rm = TRUE)
    qmin <- qu[1]
    qmax <- qu[nqu]

    qu[qu == -Inf] <- min(x[is.finite(x)], na.rm=TRUE)
    qu[qu == Inf] <- max(x[is.finite(x)], na.rm=TRUE)

    uqu <- qu[names(qu)[!duplicated(qu_indices)]]
    new_nbin <- min(c(length(unique(uqu)),nqu))
    # cl_centers <- Ckmeans.1d.dp::Ckmeans.1d.dp(x[!is.na(x)], k=new_nbin)$centers
    cl_centers <- clustering.sc.dp::clustering.sc.dp(matrix(qu,ncol=1), k=new_nbin)$centers[,1]
    cutcenter_list[[variable]] <<- cl_centers
    cutpoints <- sort(unique(c(cl_centers[-length(cl_centers)]+diff(cl_centers/2), qmin, qmax)))

    cutpoints_list[[variable]] <<- cutpoints
    ## find the interval containing each element of x in cutpoints
    findInterval(x,cutpoints, rightmost.closed=TRUE)
  }


  binning_foos <- list(
    quantile = binning_quantile,
    clustering = binning_clustering,
    smart=binning_smart)

  binning_foo <- binning_foos[[binning_method]] 

  ## for numeric variables, we must to apply the function quickCut to cut the variable into classes.
  ## create a list called txt
  ## An element looks like this for variable1
  ## variable1 = quickCut(variable1, nbins = nbins, variable="variable1")

  txtQuickcut <- paste0(paste0(num_vars,"=binning_foo(",num_vars,",nbins=",nbins,", variable='",num_vars,"')"), collapse=",")

  ## for character variables no pre treatment is needed
  ## we select only the variables
  ## we add this element to the list txt created below

  txt <- paste0("data[,.(",target )

  if (length(other_vars)>0){

    txt <- paste0(txt,",", paste(other_vars, collapse=","))
  }
  if (length(num_vars)>0) txt <- paste0(txt,",",txtQuickcut)
  txt <- paste0(txt,")]")
  if (verbose) cat("\nCutting code:\n",txt,"\n\n")


  ## evaluation of each comand in the list txt.
  ## dataCut is composed of character variables from data without modification
  ##and numeric variables but into classes
  dataCut <- eval(parse(text=txt))

  # TMP<<- dataCut
  if(verbose){cat("\nPreliminary cut performed\n")}


  ## target stats ----

  if (target_type == "numeric"){
    # compute target stats
    probs <- c(seq(0.1, 0.9, by=0.1),0.01, 0.05, 0.25, 0.75, 0.95, 0.99)
    probs <- probs[order(probs)]
    txt <- paste("data[,.(
        avg=mean(get(target), na.rm = TRUE)
        ,std=sd(get(target), na.rm = TRUE)
        ,min=min(get(target), na.rm=TRUE)
        ,"
                 ,paste("q",round(100*probs),"=quantile(get(target),",probs, ",na.rm=TRUE)",
                        sep="", collapse=","),
                 ",max=max(get(target), na.rm=TRUE)
       ,sum=sum(get(target), na.rm=TRUE)
       ,count=.N
       ,nNA=sum(is.na(get(target)))",")]")
    target_stats <- eval(parse(text=txt))
    target_stats[, percNA:=nNA/count]


  } else {


    txt <- paste("data[,.(
       count=.N
       ,nNA=sum(is.na(get(target)))","), by=",target,"]")
    target_stats <- eval(parse(text=txt))
    target_stats[, percNA:=nNA/sum(count)]
    target_stats[, perc:=count/sum(count)]
    setnames(target_stats, target, "value")


  }

  crossvars <- vector(mode="list", length=length(select_vars))
  names(crossvars) <- select_vars


  for (variable in select_vars){

    if (verbose) cat("\nCrossing with:", variable)

    ## creation of the returned values in the vector out
    cross <- list()
    cross$varname = variable
    cross$targetname = target

    # print(variable)
    ## we count the number of observations for each target for each variables
    # txt <- paste0("all[,.(N=sum(N)),by=.(",variable,",",target,")]")

    if (target_type %in% c("binary","categorical")){
      # compute N
      txt <- paste0("dataCut[,.(N=.N),by=.(",variable,",",target,")]")
      x <- eval(parse(text=txt))
      ## we rename the columns by variable and target
      colnames(x)[1:2] <- c(variable,target)
      ## we have one column with all values for the target, but we want a table with one column for the first value of the target, a second column for the second value of the target and so on
      ## we transform the table to have the values of the target in column
      tab=data.table::dcast(x, get(variable)~get(target),value.var="N")
      # tab[, variable:=as.character(variable)]
      # tab <- as.data.frame(tab)


      ## if there is missing values in variable, we put it to missing instead of NA
    } else if (target_type == "numeric"){
      txt <- paste0("dataCut[,.(
                count = .N,
                varsum = sum(get(target), na.rm=TRUE),
                avg = mean(get(target),na.rm=TRUE),
                std = sd(get(target), na.rm=TRUE),
                q25 = quantile(get(target), prob=c(0.25), na.rm=TRUE),
                median = quantile(get(target), prob=c(0.5), na.rm=TRUE),
                q75 = quantile(get(target), prob=c(0.75), na.rm=TRUE)),
                by=.(",variable,")]")
      tab <- eval(parse(text=txt))
      setnames(tab, variable, "variable")
      # tab[, variable:=as.character(variable)]




    }

    ## option NA, applies for any type of target
    if(useNA == "no"){
      ## <!><20210315>moved here, previous position!! No object tab so far
      tab <- tab[!is.na(tab$variable),]
    }

    if (target_type == "numeric"){

      # first variables addition
      tab[, `:=`(
        qrange  = q75-q25   # IQ range
      )]

      # second variables addition
      tab[, `:=`(
        bxp_min = q25 - bxp_factor * qrange,
        bxp_max = q75 + bxp_factor * qrange
      )]


    }
    treat_tab_labels <- function(tab, variable, is_numeric, cutpoints_list){

      ## table labels, maintained as data.frame rownames
      # replace label for NA / /Missing
      ## derive names with intervals from object
      if(is_numeric){
        cp <- prettyNum(cutpoints_list[[variable]],digits=dec)
        nbi <- length(cp)-1 ## number of intervals
        if (nbi>0){
          labels <- data.table::data.table(
            val=1:nbi,
            ...label=paste0('[',letters[1:nbi],'] from ', cp[1:(length(cp)-1)], ' to ',cp[2:length(cp)] )
          )

        } else {
          labels <- data.table::data.table(
            val=0,
            ...label=paste0('===', cp[1])
          )

        }
        label_NA <- data.table::data.table(val=NA, ...label='[Missing]')
        labels <- data.table::rbindlist(list(label_NA, labels))
        tab <- merge(tab, labels, by.x='variable', by.y='val', all.x=TRUE)
        tab <- as.data.frame(tab)
        rownames(tab) <- tab$...label
        tab <- tab[,-ncol(tab)] # remove ...label
        tab
      } else {
        tab <- as.data.frame(tab)
        tab[[1]] <- as.character(tab[[1]])
        tab[[1]][is.na(tab[[1]])] <- "[Missing]"
        rownames(tab) <- tab[[1]]
      }
      # ## we drop the column variable
      tab <- tab[, -1, drop=FALSE]

      #   cutpoints_list[[variable]] <-
      #   nb <- length(cutpoints_list[[variable]]) ## number of values
      #   lab <- vector(length = nb-1) ##vector to stock label
      #   let <- c(letters)[1:nb]
      #   for(i in 1:nb -1 ){
      #     if(i < nb-1){
      #       lab[i] <- paste0("[",let[i],"] from ", cutpoints_list[[variable]][i], " to ",cutpoints_list[[variable]][i+1])
      #     }else{
      #       lab[i] <- paste0("[",let[i],"] from ", cutpoints_list[[variable]][i], " to ",cutpoints_list[[variable]][i+1])}}
      #   ## to add the extreme label
      #   #        label <- c(paste("[a] moins de",cutpoints_list[[variable]][1]),lab, paste("[",let[length(let)],"] plus de",cutpoints_list[[variable]][nb]))
      #   ##combine label with numbers
      #   label <- lab
      #   if (length(label)==0){
      #     label <- paste0("==",cutpoints_list[[variable]])
      #     lab_all <- as.data.frame(cbind(0,label),stringsAsFactors = FALSE)
      #   }
      #   else {
      #     lab_all <- as.data.frame(cbind(seq(1:(nb-1)),label),stringsAsFactors = FALSE)
      #   }
      #
      #   ##associate the label to tab
      #   tab <- merge(tab,lab_all,by.x="variable",by.y="V1",all.x=TRUE)
      #   ##associate the good label
      #   for(i in 1:length(rownames(tab))){
      #     if(!is.na(tab$label[i])){
      #       # rownames(tab)[i] = label[i]
      #       rownames(tab)[i] = tab$label[i]
      #     }else{ rownames(tab)[i] = tab$variable[i]}
      #   }
      #   tab <- tab[,-ncol(tab)]
      #
      # }
      #
      #
      # ## we drop the column variable
      # tab <- tab[,-1]
      rownames(tab)[rownames(tab)==''] <- '[empty]'
      return(tab)
    }

    tab <- treat_tab_labels(tab, variable, is_numeric=(variable %in% num_vars),cutpoints_list)

    order_label_ivar <- order_label
    if(order_label == "auto"){
      if(variable %in% num_vars){
        order_label_ivar <- "alpha"
      }else{
        # cat("\ny:",order_label)
        order_label_ivar <- "props"}
    }


    if (variable %in% num_vars){
      cross$variable_type <- 'numeric'
      numcenters <-    cutcenter_list[[variable]]
      # print(numcenters)
      rn <- rownames(tab)
      rn <- rn[rn != '[Missing]']
      # print(rn)
      names(numcenters) <- rn
      cross$numcenters <- numcenters

    } else {
      cross$variable_type <- 'character'
    }

    if (target_type %in% c("binary","numeric")){

      alternate_version <- ifelse(
        woe_alternate_version=="always", TRUE,    ## ALWAYS: alternate: TRUE
        (target_type == "numeric"))             ## IF_CONTINUOUS: alternate: if target is numeric



      if (cont_target_trim>0 & target_type=="numeric"){
        # trim data to estimate WOE without outliers impacts
        min_max <- quantile(data[[target]], probs=c(cont_target_trim, 1- cont_target_trim))
        dataCut <- dataCut[get(target)>=min_max[1] & get(target)<=min_max[2],]
      }

      # DA <<- dataCut
      # VA <<- variable
      # TARGET <<- target
      woe_iv <- dt_WOE_IV(dataCut,
                          var_interest = target,
                          var_cross = variable,
                          alternate_version = alternate_version,
                          useNA = useNA,
                          woe_shift = woe_shift)

      WOE <- woe_iv$WOE[, c('variable', 'WOE'), with=FALSE]
    # print(variable)
      WOE <- treat_tab_labels(WOE, variable, is_numeric=(variable %in% num_vars),cutpoints_list)
      # print(WOE)
      IV <- woe_iv$IV

      if (woe_post_cluster){
        var_nvalues <- dataCut[!is.na(get(variable)),uniqueN(get(variable))]
        if (woe_post_cluster_n < var_nvalues){
          X <<- WOE[rownames(WOE)!='[Missing]',,drop=FALSE]

          if (variable %in% num_vars){
            # numeric variable, use sequentially constrained clustering with package clustering.sc.dp
            # cat("\nvar:", variable)
            cl <- clustering.sc.dp::clustering.sc.dp(as.matrix(X), k=woe_post_cluster_n)$cluster
          } else {
            # cat var: use "normal" clustering

            cl <- Ckmeans.1d.dp::Ckmeans.1d.dp(X$WOE, k=woe_post_cluster_n)$cluster
          }
          WOE[rownames(WOE)!='[Missing]','cluster'] <- cl
          WOE[rownames(WOE)=='[Missing]','cluster'] <- 0
        } else {
          WOE$cluster <- 1
        }
      } else {
        WOE$cluster <- 1
      }
      cross$woe_cluster <- (length(unique(WOE$cluster))>1) # has >1 cluster to be used for graphs
    } else {
      WOE=NULL
      IV <- NULL
    }

    if (target_type %in% c("binary","categorical")){
      # tot <- rowSums(tab)

      tab[is.na(tab)] <- 0

      p <- prop.table(as.table(as.matrix(tab)),margin=1)

      ## calculation of the index
      ind <- matrix( nrow = length(p[,1]),ncol = length(colnames(p)))
      for(i in seq_along(colnames(p))){
        avgpeni <- sum(tab[,i])/sum(tab)
        ind[,i] <- p[,i]/avgpeni
      }
      colnames(ind) <- colnames(p)
      rownames(ind) <- rownames(p)

      ##creation of the vector order
      if(order_label_ivar == "alpha"){
        t1 <- rownames(tab)
        orderlabel <- t1[order(t1)]
      }else if (order_label_ivar == "count" ) {
        t1 <- tab
        t1 <- cbind(t1,rowSums(t1))
        colnames(t1)[ncol(t1)] <- "total"
        orderlabel <- colnames(t1[,"total"][order(-t1[,"total"])])
      }else if (order_label_ivar %in% c("props","means"))
      {

        t1 <- p
        reflev <- as.character(target_reference_level)
        if (is.na(reflev))reflev<- "NA"
        # print("Yes")
        # print(reflev)
        wh <- which(colnames(t1)==reflev)
        # TMP <<- t1
        # print(wh)
        orderlabel <- names(t1[,wh][order(-t1[,wh])]) # note: categorical: arbitrary ordered by second column

        # print(t1)
        # print(orderlabel)
      }


      cross$counts = tab
      cross$props = p
      cross$index = ind

      if (target_type %in% c('binary','categorical')) cross$target_reference_level <-target_reference_level

      class(cross) <- c("crossvar",paste("crossvar", target_type, sep="_"), class(cross))
    } else {
      # continuous / ordinal target
      ##creation of the vector order
      t1 <- rownames(tab)

      if(order_label_ivar == "alpha"){
        orderlabel <- t1[order(t1)]
      }else if (order_label_ivar == "count" ) {
        orderlabel <- t1[order(-tab[,"count"])]
      }else if (order_label_ivar %in% c("props","means"))
      {
        orderlabel <- t1[order(-tab[,'avg'])] # note: categorical: arbitrary ordered by second column
      }
      cross$stats <- tab
      class(cross) <- c("crossvar",paste("crossvar", target_type, sep="_"), class(cross))
    }

    ## common slots
    cross$orderlabel = orderlabel
    cross$levels = rownames(tab)
    cross$target_stats = as.data.frame(target_stats)
    cross$target_type <- target_type

    cross$IV <- IV
    cross$woe = WOE

    crossvars[[variable]] <- cross
  }

  out <- list(
    dataname = dataname,
    target = target,
    description_data = description_data,
    description_target = description_target,
    target_type = target_type,
    target_stats = as.data.frame(target_stats) ,
    analysis = analysis_name,
    date = Sys.Date(),
    profiles = crossvars
  )
  if (target_type %in% c('binary','categorical')){
    out$target_reference_level <- target_reference_level
  }
  ## assign class
  class(out) <- c("targeter",class(out))

  ## display messages
  cat("\n")
  cat(paste(names(msg),msg, sep=":" , collapse = "\n"))
  cat("\n")
  ## return object
  return(out)
}

#' @method print targeter
#' @importFrom utils head
print.targeter <- function(x,...){
  cat("\nTarget profiling object with following properties:")
  cat(paste0("\n\tTarget:"), x$target, " of type:", x$target_type)
  if (x$target_type == 'binary') cat(paste0("  (target level:", x$target_reference_level),")")
  cat(paste0("\n\tRun on data:"), x$dataname, " the:", format(x$date))
  nprof <- length(x$profiles)
  vars_profile <-  utils::head(names(x$profiles),5)
  vars_profile <- paste(vars_profile, collapse=", ")
  if (nprof>5) vars_profile <- paste0(vars_profile, "...")
  cat(paste0("\n", length(x$profiles), " profiles available (",vars_profile,")"))
  cat(paste0("\nYou can access each crossing using slot $profiles[[__variable__]]. Then on it use `plot` or `summary`"))
  cat(paste0("\nYou can also directly invoke a global `summary` function on this object."))

}





