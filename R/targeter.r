# save.image(file = "/hackdata/share/_tmp/targeter-refactored.RData")
# targeter_internal
# ├── validate_inputs
# ├── process_parameters
# ├── check_dependencies
# ├── analyze_target
# │   └── dt_vartype_autoguess_onevar (external)
# ├── analyze_variables
# │   ├── detect_variables_types
# │   │   └── check_naming_conventions (external)
# │   └── filter_variables
# ├── binning_factory
# │   ├── binning_quantile
# │   ├── binning_clustering
# │   └── binning_smart
# ├── create_binning_expression
# ├── compute_target_statistics
# ├── process_crossings
# │   ├── calculate_statistics
# │   │   └── treat_tab_labels
# │   └── compute_woe_iv
# │       └── dt_WOE_IV
# └── targeter_session_info (external)

library(data.table)
# to prevent checks of data.table used variables
# see:  ?globalVariables
if (getRversion() >= "3.1.0")
  utils::globalVariables(
    c(".N", ":=", "vcount", "vsum", "WOE", "vperc", "cperc")
  )

#' @importFrom data.table setnames
dt_WOE_IV <- function(
  data,
  var_interest,
  var_cross,
  alternate_version = FALSE,
  woe_shift = 0.01,
  useNA = c("ifany", "no"),
  target_reference_level = 1
) {
  ## parameters checks
  # if (!is.null(binary)){
  #   assertthat::assert_that(is.logical(binary),
  # msg= "binary must be logical")
  # }
  assertthat::assert_that(
    inherits(data, "data.frame"),
    msg = "data must be a data frame or data table"
  )
  data.table::setDT(data)
  nm <- colnames(data)
  assertthat::assert_that(
    var_interest %in% nm,
    msg = "varinterest is not an existing 
                          variable in data"
  )

  assertthat::assert_that(
    var_cross %in% nm,
    msg = "varcount is not an existing variable in data"
  )

  useNA <- match.arg(useNA, c("ifany", "no"), several.ok = FALSE)

  vRANGE <- data[, range(get(var_interest), na.rm = TRUE)]
  #vRANGE <- data[, range(ifelse(get(var_interest)==target_reference_level,1,0), na.rm = TRUE)]
  # print(vRANGE)
  if (useNA == "no") data <- data[!is.na(var_cross), ]

  # agg <- data[, .(vcount = .N,
  #  vsum = sum((ifelse(get(var_interest)==target_reference_level,1,0) - vRANGE[1]) / diff(vRANGE))),
  #  by =c(var_cross)]

  agg <- data[,
    .(
      vcount = .N,
      vsum = sum(
        (get(var_interest) - vRANGE[1]) / diff(vRANGE)
      )
    ),
    by = c(var_cross)
  ]

  data.table::setnames(agg, var_cross, 'variable')
  if (!alternate_version) {
    # replace count with "non-events 0" or numeric "opposite
    agg[, vcount := (vcount - vsum)]
  }

  agg[,
    `:=`(
      vperc = ((vsum + woe_shift) / sum(vsum + woe_shift)),
      cperc = ((vcount + woe_shift) / sum(vcount + woe_shift))
    )
  ]
  agg[, WOE := log(vperc / cperc)]
  IV <- agg[, sum((vperc - cperc) * WOE)]
  return(list(WOE = agg, IV = IV))
}


#' Analyze target variable properties
#'
#' This function handles the detection and validation of target variable properties
#' including type detection and reference level determination.
#'
#' @param data data.table with the data to analyze
#' @param target character - name of the target variable
#' @param target_type character - specified type or "autoguess"
#' @param target_reference_level character or numeric - reference level for target
#' @param num_as_categorical_nval numeric - threshold for considering numeric as categorical
#'
#' @return list with target properties and messages
#'
#' @importFrom data.table data.table
analyze_target <- function(
  data,
  target,
  target_type = "autoguess",
  target_reference_level = NULL,
  num_as_categorical_nval = 5,
  autoguess_nrows = 1000
) {
  messages <- list()

  # Determine target type if not specified
  if (target_type == "autoguess") {
    detected_type <- dt_vartype_autoguess_onevar(
      data,
      target,
      num_as_categorical_nval
    )

    if (detected_type == "unimode") {
      messages <- c(messages, list(ERROR = "target has a unique value"))
      return(list(
        type = detected_type,
        messages = messages,
        status = "error"
      ))
    }

    if (detected_type == "unknown") {
      messages <- c(messages, list(ERROR = "target has an unknown type"))
      return(list(
        type = detected_type,
        messages = messages,
        status = "error"
      ))
    }

    messages <- c(
      messages,
      list(INFO = paste("target", target, "detected as type:", detected_type))
    )

    target_type <- detected_type
  }

  # Handle reference level determination for categorical/binary targets
  ref_level <- target_reference_level

  if (target_type == 'binary' & is.null(ref_level)) {
    cl <- class(data[[target]])[1]

    if (cl == 'logical') {
      ref_level <- TRUE
      messages <- c(
        messages,
        list(
          INFO = paste(
            "target is logical, automatic chosen level: TRUE; override using `target_reference_level`"
          )
        )
      )
    } else if (cl %in% c('numeric', 'integer')) {
      ref_level <- 1
      messages <- c(
        messages,
        list(
          INFO = paste(
            "binary target contains number, automatic chosen level: 1; override using `target_reference_level`"
          )
        )
      )
    } else if (cl %in% c('factor', 'character')) {
      ref_level <- data[1, ][[target]]
      messages <- c(
        messages,
        list(
          INFO = paste(
            "target is character/factor, automatic chosen level: ",
            ref_level,
            "; override using `target_reference_level`"
          )
        )
      )
    }
  }

  if (target_type == 'categorical' & is.null(ref_level)) {
    ref_level <- data[[target]][1] # totally arbitrary
    messages <- c(
      messages,
      list(
        INFO = paste(
          "target is categorical. As no target_reference_level was provided, one value is taken arbitrary:",
          ref_level,
          " override using `target_reference_level`"
        )
      )
    )
  }

  return(list(
    type = target_type,
    reference_level = ref_level,
    messages = messages,
    status = "ok"
  ))
}

#' Detect and filter variables for analysis
#'
#' This function handles variable type detection and filtering based on
#' naming conventions and other criteria.
#'
#' @param data data.table with data to analyze
#' @param target character - name of the target variable
#' @param select_vars character - vector of variables to include
#' @param exclude_vars character - vector of variables to exclude
#' @param naming_conventions logical - whether to enforce naming conventions
#' @param autoguess_nrows numeric - number of rows to use for type detection
#' @param num_as_categorical_nval numeric - threshold for numeric as categorical
#' @param verbose logical - whether to print verbose output
#'
#' @return list with variable classification and messages
#'
#' @importFrom data.table data.table
#' @importFrom assertthat assert_that
detect_variables_types <- function(
  data,
  target,
  select_vars = NULL,
  exclude_vars = NULL,
  naming_conventions = FALSE,
  autoguess_nrows = 1000,
  num_as_categorical_nval = 5,
  verbose = FALSE
) {
  messages <- list()
  vars <- data.table(
    variable = sort(unique(c(select_vars, exclude_vars, names(data))))
  )
  vars[, is_in_data := (variable %in% names(data))]
  vars[, is_target := (variable == target)]

  # Handle reserved target name
  if (target == "target") {
    messages <- c(
      messages,
      c("INFO" = "Silently renaming reserved word target as `...target`")
    )
    setnames(data, "target", "...target")
    target <- "...target"
  }

  # Select variables to analyze
  if (is.null(select_vars)) {
    select_vars <- names(data)
    if (naming_conventions == TRUE) {
      select_vars <- select_vars[
        substr(select_vars, 1, 2) %in%
          c("L_", "N_", "M_", "F_", "J_", "C_", "R_", "P_", "O_", "Z_")
      ]
      vars[,
        respect_naming_convention := (variable %in% c(select_vars, target))
      ]
    }
  }

  # Exclude target and requested variables
  select_vars <- select_vars[select_vars != target]
  select_vars <- select_vars[!(select_vars %in% exclude_vars)]
  vars[, is_in_exclude := (variable %in% exclude_vars)]

  # Check if selected variables exist in data
  check <- select_vars %in% colnames(data)
  if (any(!check)) {
    varlist <- paste(select_vars[!check], collapse = " ")
    messages <- c(
      messages,
      list(
        WARNING = paste(
          "Some variables not present in dataframe - they are removed from analyses:",
          varlist
        )
      )
    )
    select_vars <- select_vars[check]
  }

  # Apply naming conventions if requested
  if (naming_conventions) {
    check <- !check_naming_conventions(data[, ..select_vars])[, 1]
    if (any(!check)) {
      varlist <- paste(select_vars[!check], collapse = " ")
      messages <- c(
        messages,
        list(
          WARNING = paste(
            "Some variables do not respect naming conventions - they are removed from analyses:",
            head(varlist, 3)
          )
        )
      )
      vars[, respect_naming_convention := !(variable %in% varlist)]
      select_vars <- select_vars[check]
    }
  }

  # Fix variable names to ensure valid R names
  cn <- select_vars
  nn <- make.names(cn, unique = TRUE)
  if (any(cn != nn)) {
    old_names <- cn[cn != nn]
    new_names <- nn[cn != nn]
    vars[, silently_renamed := (variable %in% old_names)]
    info <- paste(cn[cn != nn], nn[cn != nn], sep = '->', collapse = '\t')
    messages <- c(
      messages,
      list(
        WARNING = paste(
          "silently changing some variables names to be valid unique R names:",
          info
        )
      )
    )
    setnames(data, old_names, new_names)
    select_vars <- nn
  }

  # Handle target name too
  cn <- target
  nn <- make.names(target)
  if (any(cn != nn)) {
    vars[
      which(variable == target),
      silently_renamed := (variable %in% old_names)
    ]
    info <- paste(cn, nn, sep = '->', collapse = '\t')
    messages <- c(
      messages,
      list(
        WARNING = paste(
          "silently changing target names to be valid R name:",
          info
        )
      )
    )
    setnames(data, old_names, new_names)
    target <- nn
  }

  # Categorize variables by type
  vars[, var_type := ""]

  # Auto-detect variable types if not using naming conventions
  if (naming_conventions == TRUE) {
    num_vars <- select_vars[
      substr(select_vars, 1, 2) %in% c("N_", "M_", "F_", "J_", "R_", "P_")
    ]
    ord_vars <- select_vars[substr(select_vars, 1, 2) %in% c("O_")]
  } else {
    if (autoguess_nrows == 0) autoguess_nrows <- nrow(data)
    autoguess_nrows <- min(autoguess_nrows, nrow(data))
    data_types <- dt_vartype_autoguess(
      data[1:autoguess_nrows, ..select_vars],
      num_as_categorical_nval
    )

    cl <- sapply(data[, ..select_vars], function(x) class(x)[1])
    to_convert <- names(data_types)[
      data_types != "numeric" & (cl %in% c("numeric", "integer"))
    ]

    if (length(to_convert) > 0) {
      for (ivar in to_convert) {
        if (verbose) {
          cat("\nconverting:", ivar, " as character.")
        }
        data[, (ivar) := as.character(get(ivar))]
      }
    }
    num_vars <- names(data_types)[data_types == "numeric"]
    ord_vars <- names(data_types)[data_types == "ordinal"]
  }

  vars[which(variable %in% num_vars), var_type := "numeric"]
  vars[which(variable %in% ord_vars), var_type := "ordinal"]

  # Categorize remaining variables
  other_vars <- select_vars[!(select_vars %in% c(num_vars))]
  vars[which(variable %in% other_vars), var_type := "categorical"]

  # Check if we have any variables left to analyze
  dt_vars_exp <- unique(c(num_vars, ord_vars, other_vars))
  if (length(dt_vars_exp) == 0) {
    messages <- c(
      messages,
      list(
        ERROR = "No explanatory variable remaining. Check variables and naming conventions if used."
      )
    )
    vars[
      which(variable %in% c(num_vars, ord_vars, other_vars, target)),
      respect_naming_convention := TRUE
    ]
    return(list(
      numeric_vars = NULL,
      ordinal_vars = NULL,
      categorical_vars = NULL,
      all_vars = NULL,
      target = target,
      vars = vars,
      messages = messages,
      status = "error"
    ))
  }

  return(list(
    numeric_vars = num_vars,
    ordinal_vars = ord_vars,
    categorical_vars = other_vars,
    all_vars = dt_vars_exp,
    target = target,
    vars = vars,
    messages = messages,
    status = "ok",
    data = data
  ))
}

#' Create binning functions based on method
#'
#' This function creates appropriate binning functions based on the chosen method
#'
#' @param method character - binning method: "quantile", "clustering", or "smart"
#' @param nbins numeric - number of bins to create
#' @param smart_quantile_by numeric - quantile step for smart binning
#' @param verbose logical - whether to print verbose output
#'
#' @return list of binning functions and configuration
#'
#' @importFrom stats quantile
#' @importFrom data.table data.table
#' @importFrom data.table setnames
binning_factory <- function(
  method = "quantile",
  nbins = 12,
  smart_quantile_by = 0.01,
  verbose = FALSE
) {
  # Storage for cut points and centers
  cutpoints_list <- list()
  cutcenter_list <- list()

  # Binning function using quantiles
  binning_quantile <- function(x, variable) {
    # Put a vector from 0 to 1 by 1/nbins
    quantiles <- seq(0, 1, length.out = nbins + 1)
    # Take the value of the quantile for the variable x
    cutpoints <- unname(unique(stats::quantile(x, quantiles, na.rm = TRUE)))

    centers <- cutpoints[-length(cutpoints)] + diff(cutpoints / 2)
    if (length(centers) == 0) centers <- cutpoints

    # Store centers and cutpoints
    cutcenter_list[[variable]] <<- centers
    cutpoints_list[[variable]] <<- cutpoints

    # Find the interval containing each element of x in cutpoints
    return(findInterval(x, cutpoints, rightmost.closed = TRUE))
  }

  # Binning function using clustering
  binning_clustering <- function(x, variable) {
    if (verbose) cat("\nClustering binning for:", variable)

    cl_centers <- Ckmeans.1d.dp(x[!is.na(x)], k = nbins)$centers
    cutcenter_list[[variable]] <<- cl_centers

    cutpoints <- sort(
      unique(
        c(
          cl_centers[-length(cl_centers)] +
            diff(cl_centers / 2),
          range(x, na.rm = TRUE)
        )
      )
    )

    cutpoints_list[[variable]] <<- cutpoints

    # Find the interval containing each element of x in cutpoints
    return(findInterval(x, cutpoints, rightmost.closed = TRUE))
  }

  # Binning function using smart method (quantiles + clustering)
  binning_smart <- function(x, variable) {
    if (verbose) cat("\nSmart binning for:", variable)

    quantiles <- seq(0, 1, length.out = 1 + round(1 / smart_quantile_by))
    nqu <- length(quantiles)
    qu_indices <- (1:nqu) %/% (nqu / nbins)

    # Take quantile values
    qu <- quantile(x, quantiles, na.rm = TRUE)
    qmin <- qu[1]
    qmax <- qu[nqu]

    # Handle infinities
    qu[qu == -Inf] <- min(x[is.finite(x)], na.rm = TRUE)
    qu[qu == Inf] <- max(x[is.finite(x)], na.rm = TRUE)

    # Get unique values
    uqu <- qu[names(qu)[!duplicated(qu_indices)]]
    new_nbin <- min(c(length(unique(uqu)), nqu))

    # Use clustering.sc.dp for smart clustering
    cl_centers <- clustering.sc.dp(
      matrix(qu, ncol = 1),
      k = new_nbin
    )$centers[, 1]

    cutcenter_list[[variable]] <<- cl_centers

    cutpoints <- sort(
      unique(
        c(cl_centers[-length(cl_centers)] + diff(cl_centers / 2), qmin, qmax)
      )
    )

    cutpoints_list[[variable]] <<- cutpoints

    # Find the interval containing each element of x in cutpoints
    return(findInterval(x, cutpoints, rightmost.closed = TRUE))
  }

  # Select the appropriate binning function based on method
  binning_function <- switch(
    method,
    "quantile" = binning_quantile,
    "clustering" = binning_clustering,
    "smart" = binning_smart,
    binning_quantile # default
  )

  return(list(
    binning_function = binning_function,
    cutpoints_list = cutpoints_list,
    cutcenter_list = cutcenter_list
  ))
}

#' Compute statistics for target variable
#'
#' This function computes appropriate statistics based on the target type
#'
#' @param data data.table with the data
#' @param target character - name of the target variable
#' @param target_type character - type of the target variable
#'
#' @return data.table with target statistics
#'
#' @importFrom data.table data.table
#' @importFrom stats quantile sd
compute_target_statistics <- function(data, target, target_type) {
  if (target_type == "numeric") {
    # Compute stats for numeric target
    probs <- c(seq(0.1, 0.9, by = 0.1), 0.01, 0.05, 0.25, 0.75, 0.95, 0.99)
    probs <- probs[order(probs)]

    # Build quantile column expressions
    quantile_exprs <- paste(
      "q",
      round(100 * probs),
      "=quantile(get(target),",
      probs,
      ",na.rm=TRUE)",
      sep = "",
      collapse = ","
    )

    # Full expression to evaluate
    txt <- paste(
      "data[,.(
        avg=mean(get(target), na.rm = TRUE)
        ,std=sd(get(target), na.rm = TRUE)
        ,min=min(get(target), na.rm=TRUE)
        ,",
      quantile_exprs,
      ",max=max(get(target), na.rm=TRUE)
       ,sum=sum(get(target), na.rm=TRUE)
       ,count=.N
       ,nNA=sum(is.na(get(target)))",
      ")]"
    )

    target_stats <- eval(parse(text = txt))
    target_stats[, percNA := nNA / count]
  } else {
    # Compute stats for categorical/binary target
    txt <- paste(
      "data[,.(
       count=.N
       ,nNA=sum(is.na(get(target)))",
      "), by=",
      target,
      "]"
    )

    target_stats <- eval(parse(text = txt))
    target_stats[, percNA := nNA / sum(count)]
    target_stats[, perc := count / sum(count)]
    setnames(target_stats, target, "value")
  }

  return(target_stats)
}

#' Process and match function parameters
#'
#' This function validates and processes key parameters by matching them against
#' their allowed values.
#'
#' @param binning_method character - method for binning numeric variables
#' @param order_label character - order method for labels in output
#' @param target_type character - type of the target variable
#' @param woe_alternate_version character - when to use alternate WOE definition
#'
#' @return list of processed parameters
process_parameters <- function(
  binning_method,
  order_label,
  target_type,
  woe_alternate_version
) {
  # Match arguments with possible values from original function
  binning_method <- match.arg(
    binning_method,
    c("quantile", "clustering", "smart"),
    several.ok = FALSE
  )

  order_label <- match.arg(
    order_label,
    c("auto", "alpha", "count", "props", "means"),
    several.ok = FALSE
  )

  target_type <- match.arg(
    target_type,
    c("autoguess", "binary", "categorical", "numeric"),
    several.ok = FALSE
  )

  woe_alternate_version <- match.arg(
    woe_alternate_version,
    c("if_continuous", "always"),
    several.ok = FALSE
  )

  # Return processed parameters
  return(list(
    binning_method = binning_method,
    order_label = order_label,
    target_type = target_type,
    woe_alternate_version = woe_alternate_version
  ))
}

#' Check for required packages based on selected methods
#'
#' @param binning_method character - binning method selected
#' @param woe_post_cluster logical - whether post-clustering is used
#'
#' @importFrom assertthat assert_that
check_dependencies <- function(binning_method, woe_post_cluster) {
  if (binning_method == "clustering") {
    assertthat::assert_that(
      requireNamespace("cluster", quietly = TRUE),
      msg = "cluster package required for clustering binning method"
    )
  }

  if (binning_method == "smart") {
    assertthat::assert_that(
      requireNamespace("smbinning", quietly = TRUE),
      msg = "smbinning package required for smart binning method"
    )
  }

  if (woe_post_cluster) {
    assertthat::assert_that(
      requireNamespace('Ckmeans.1d.dp', quietly = TRUE),
      msg = 'Ckmeans.1d.dp package required for WOE post clustering.'
    )
    assertthat::assert_that(
      requireNamespace('clustering.sc.dp', quietly = TRUE),
      msg = 'clustering.sc.dp package required for WOE post clustering.'
    )
  }
}

#' Prepare data and select variables for analysis
#'
#' @param data data.frame or data.table - the input data
#' @param target character - name of target variable
#' @param select_vars character vector - variables to select
#' @param exclude_vars character vector - variables to exclude
#'
#' @return data.table - processed data
#'
#' @importFrom data.table as.data.table
prepare_data <- function(
  data,
  target,
  select_vars = NULL,
  exclude_vars = NULL
) {
  # Convert to data.table if needed
  if (!inherits(data, "data.table")) {
    data <- data.table::as.data.table(data)
  }

  # Select variables if specified
  if (!is.null(select_vars)) {
    # Ensure target is included
    all_vars <- unique(c(target, select_vars))
    data <- data[, ..all_vars]
  }

  # Exclude variables if specified
  if (!is.null(exclude_vars)) {
    # Make sure target is not excluded
    exclude_vars <- setdiff(exclude_vars, target)
    keep_vars <- setdiff(names(data), exclude_vars)
    data <- data[, ..keep_vars]
  }

  return(data)
}

#' Filter variables with insufficient variance
#'
#' This function removes variables that have only a single unique value,
#' as they provide no discriminatory power for analysis.
#'
#' @param data data.table with the data
#' @param numeric_vars character - vector of numeric variables
#' @param ordinal_vars character - vector of ordinal variables
#' @param categorical_vars character - vector of categorical variables
#' @param vars data.table - information on variables at analysis step
#' @param verbose logical - whether to print verbose output
#'
#' @return list with filtered variable lists and metadata
#'
#' @importFrom data.table data.table uniqueN
filter_variables <- function(
  data,
  numeric_vars,
  ordinal_vars,
  categorical_vars,
  vars,
  verbose = FALSE
) {
  messages <- list()
  all_vars <- c(numeric_vars, ordinal_vars, categorical_vars)

  # Find variables with only one unique value
  single_value_vars <- character(0)

  for (var in all_vars) {
    if (data.table::uniqueN(data[[var]], na.rm = TRUE) <= 1) {
      single_value_vars <- c(single_value_vars, var)
      if (verbose) {
        cat("\nVariable", var, "has only one unique value and will be excluded")
      }
    }
  }

  # If we found any single-value variables, remove them
  if (length(single_value_vars) > 0) {
    messages <- c(
      messages,
      list(
        WARNING = paste(
          "The following variables have been excluded because they have only one unique value:",
          paste(single_value_vars, collapse = ", ")
        )
      )
    )

    # Update variable lists
    numeric_vars <- setdiff(numeric_vars, single_value_vars)
    ordinal_vars <- setdiff(ordinal_vars, single_value_vars)
    categorical_vars <- setdiff(categorical_vars, single_value_vars)
    all_vars <- setdiff(all_vars, single_value_vars)

    # Update metadata
    vars[
      which(vars$variable %in% single_value_vars),
      dropped_one_single_value := TRUE
    ]
  }

  return(list(
    numeric_vars = numeric_vars,
    ordinal_vars = ordinal_vars,
    categorical_vars = categorical_vars,
    all_vars = all_vars,
    vars = vars,
    messages = messages
  ))
}

#' Analyze variables for profiling
#'
#' This function handles the complete process of variable detection, filtering,
#' and type assignment for the targeter analysis.
#'
#' @param data data.table with data to analyze
#' @param target character - name of the target variable
#' @param select_vars character - vector of variables to include
#' @param exclude_vars character - vector of variables to exclude
#' @param naming_conventions logical - whether to enforce naming conventions
#' @param autoguess_nrows numeric - number of rows to use for type detection
#' @param num_as_categorical_nval numeric - threshold for numeric as categorical
#' @param verbose logical - whether to print verbose output
#'
#' @return list with variable classification and metadata
#'
#' @importFrom data.table data.table setnames
#' @importFrom assertthat assert_that
analyze_variables <- function(
  data,
  target,
  select_vars = NULL,
  exclude_vars = NULL,
  naming_conventions = FALSE,
  autoguess_nrows = 1000,
  num_as_categorical_nval = 5,
  verbose = FALSE
) {
  messages <- list()

  # Initial variable detection and classification
  var_detection <- detect_variables_types(
    data = data,
    target = target,
    select_vars = select_vars,
    exclude_vars = exclude_vars,
    naming_conventions = naming_conventions,
    autoguess_nrows = autoguess_nrows,
    num_as_categorical_nval = num_as_categorical_nval,
    verbose = verbose
  )

  # Check for errors in variable detection
  if (var_detection$status == "error") {
    return(var_detection)
  }

  # Extract key information from detection results
  numeric_vars <- var_detection$numeric_vars
  ordinal_vars <- var_detection$ordinal_vars
  categorical_vars <- var_detection$categorical_vars
  all_vars <- var_detection$all_vars
  vars <- var_detection$vars
  target <- var_detection$target
  data <- var_detection$data
  messages <- c(messages, var_detection$messages)

  # Filter variables based on variance (drop variables with only one value)
  vars_filtered <- filter_variables(
    data = data,
    numeric_vars = numeric_vars,
    ordinal_vars = ordinal_vars,
    categorical_vars = categorical_vars,
    vars = vars,
    verbose = verbose
  )

  # Log any message from filtering
  if (length(vars_filtered$messages) > 0) {
    messages <- c(messages, vars_filtered$messages)
  }

  # Check if we still have enough variables after filtering
  if (length(vars_filtered$all_vars) == 0) {
    messages <- c(
      messages,
      list(
        ERROR = "No variables with sufficient variance remaining for analysis"
      )
    )

    return(list(
      numeric_vars = NULL,
      ordinal_vars = NULL,
      categorical_vars = NULL,
      all_vars = NULL,
      target = target,
      vars = vars,
      messages = messages,
      status = "error",
      data = data
    ))
  }

  # Return final results
  return(list(
    numeric_vars = vars_filtered$numeric_vars,
    ordinal_vars = vars_filtered$ordinal_vars,
    categorical_vars = vars_filtered$categorical_vars,
    all_vars = vars_filtered$all_vars,
    target = target,
    vars = vars_filtered$vars,
    messages = messages,
    status = "ok",
    data = data
  ))
}


# targeter_internal
# ├── validate_inputs()
# ├── analyze_variables()
# │   ├── detect_variables_types()
# │   └── filter_variables()
# ├── analyze_target()
# ├── binning_factory()
# │   ├── binning_quantile()
# │   ├── binning_clustering()
# │   └── binning_smart()
# ├── compute_target_statistics()
# ├── process_crossings()
# │   ├── cross_with_categorical_target()
# │   ├── cross_with_numerical_target()
# │   └── format_cross_tables()
# ├── compute_woe_iv()
# └── format_results()

#' Analyze target variable properties
#'
#' This function handles the detection and validation of target variable properties
#' including type detection and reference level determination.
#'
#' @param data data.table with the data to analyze
#' @param target character - name of the target variable
#' @param target_type character - specified type or "autoguess"
#' @param target_reference_level character or numeric - reference level for target
#' @param num_as_categorical_nval numeric - threshold for considering numeric as categorical
#'
#' @return list with target properties and messages
#'
#' @importFrom data.table data.table
analyze_target <- function(
  data,
  target,
  target_type = "autoguess",
  target_reference_level = NULL,
  num_as_categorical_nval = 5,
  autoguess_nrows = 1000
) {
  messages <- list()

  # Determine target type if not specified
  if (target_type == "autoguess") {
    detected_type <- dt_vartype_autoguess_onevar(
      data,
      target,
      num_as_categorical_nval
    )

    if (detected_type == "unimode") {
      messages <- c(messages, list(ERROR = "target has a unique value"))
      return(list(
        type = detected_type,
        messages = messages,
        status = "error"
      ))
    }

    if (detected_type == "unknown") {
      messages <- c(messages, list(ERROR = "target has an unknown type"))
      return(list(
        type = detected_type,
        messages = messages,
        status = "error"
      ))
    }

    messages <- c(
      messages,
      list(INFO = paste("target", target, "detected as type:", detected_type))
    )

    target_type <- detected_type
  }

  # Handle reference level determination for categorical/binary targets
  ref_level <- target_reference_level

  if (target_type == 'binary' & is.null(ref_level)) {
    cl <- class(data[[target]])[1]

    if (cl == 'logical') {
      ref_level <- TRUE
      messages <- c(
        messages,
        list(
          INFO = paste(
            "target is logical, automatic chosen level: TRUE; override using `target_reference_level`"
          )
        )
      )
    } else if (cl %in% c('numeric', 'integer')) {
      ref_level <- 1
      messages <- c(
        messages,
        list(
          INFO = paste(
            "binary target contains number, automatic chosen level: 1; override using `target_reference_level`"
          )
        )
      )
    } else if (cl %in% c('factor', 'character')) {
      ref_level <- data[1, ][[target]]
      messages <- c(
        messages,
        list(
          INFO = paste(
            "target is character/factor, automatic chosen level: ",
            ref_level,
            "; override using `target_reference_level`"
          )
        )
      )
    }
  }

  if (target_type == 'categorical' & is.null(ref_level)) {
    ref_level <- data[[target]][1] # totally arbitrary
    messages <- c(
      messages,
      list(
        INFO = paste(
          "target is categorical. As no target_reference_level was provided, one value is taken arbitrary:",
          ref_level,
          " override using `target_reference_level`"
        )
      )
    )
  }

  return(list(
    type = target_type,
    reference_level = ref_level,
    messages = messages,
    status = "ok"
  ))
}

#' Detect and filter variables for analysis
#'
#' This function handles variable type detection and filtering based on
#' naming conventions and other criteria.
#'
#' @param data data.table with data to analyze
#' @param target character - name of the target variable
#' @param select_vars character - vector of variables to include
#' @param exclude_vars character - vector of variables to exclude
#' @param naming_conventions logical - whether to enforce naming conventions
#' @param autoguess_nrows numeric - number of rows to use for type detection
#' @param num_as_categorical_nval numeric - threshold for numeric as categorical
#' @param verbose logical - whether to print verbose output
#'
#' @return list with variable classification and messages
#'
#' @importFrom data.table data.table
#' @importFrom assertthat assert_that
detect_variables_types <- function(
  data,
  target,
  select_vars = NULL,
  exclude_vars = NULL,
  naming_conventions = FALSE,
  autoguess_nrows = 1000,
  num_as_categorical_nval = 5,
  verbose = FALSE
) {
  messages <- list()
  vars <- data.table(
    variable = sort(unique(c(select_vars, exclude_vars, names(data))))
  )
  vars[, is_in_data := (variable %in% names(data))]
  vars[, is_target := (variable == target)]

  # Handle reserved target name
  if (target == "target") {
    messages <- c(
      messages,
      c("INFO" = "Silently renaming reserved word target as `...target`")
    )
    setnames(data, "target", "...target")
    target <- "...target"
  }

  # Select variables to analyze
  if (is.null(select_vars)) {
    select_vars <- names(data)
    if (naming_conventions == TRUE) {
      select_vars <- select_vars[
        substr(select_vars, 1, 2) %in%
          c("L_", "N_", "M_", "F_", "J_", "C_", "R_", "P_", "O_", "Z_")
      ]
      vars[,
        respect_naming_convention := (variable %in% c(select_vars, target))
      ]
    }
  }

  # Exclude target and requested variables
  select_vars <- select_vars[select_vars != target]
  select_vars <- select_vars[!(select_vars %in% exclude_vars)]
  vars[, is_in_exclude := (variable %in% exclude_vars)]

  # Check if selected variables exist in data
  check <- select_vars %in% colnames(data)
  if (any(!check)) {
    varlist <- paste(select_vars[!check], collapse = " ")
    messages <- c(
      messages,
      list(
        WARNING = paste(
          "Some variables not present in dataframe - they are removed from analyses:",
          varlist
        )
      )
    )
    select_vars <- select_vars[check]
  }

  # Apply naming conventions if requested
  if (naming_conventions) {
    check <- !check_naming_conventions(data[, ..select_vars])[, 1]
    if (any(!check)) {
      varlist <- paste(select_vars[!check], collapse = " ")
      messages <- c(
        messages,
        list(
          WARNING = paste(
            "Some variables do not respect naming conventions - they are removed from analyses:",
            head(varlist, 3)
          )
        )
      )
      vars[, respect_naming_convention := !(variable %in% varlist)]
      select_vars <- select_vars[check]
    }
  }

  # Fix variable names to ensure valid R names
  cn <- select_vars
  nn <- make.names(cn, unique = TRUE)
  if (any(cn != nn)) {
    old_names <- cn[cn != nn]
    new_names <- nn[cn != nn]
    vars[, silently_renamed := (variable %in% old_names)]
    info <- paste(cn[cn != nn], nn[cn != nn], sep = '->', collapse = '\t')
    messages <- c(
      messages,
      list(
        WARNING = paste(
          "silently changing some variables names to be valid unique R names:",
          info
        )
      )
    )
    setnames(data, old_names, new_names)
    select_vars <- nn
  }

  # Handle target name too
  cn <- target
  nn <- make.names(target)
  if (any(cn != nn)) {
    vars[
      which(variable == target),
      silently_renamed := (variable %in% old_names)
    ]
    info <- paste(cn, nn, sep = '->', collapse = '\t')
    messages <- c(
      messages,
      list(
        WARNING = paste(
          "silently changing target names to be valid R name:",
          info
        )
      )
    )
    setnames(data, old_names, new_names)
    target <- nn
  }

  # Categorize variables by type
  vars[, var_type := ""]

  # Auto-detect variable types if not using naming conventions
  if (naming_conventions == TRUE) {
    num_vars <- select_vars[
      substr(select_vars, 1, 2) %in% c("N_", "M_", "F_", "J_", "R_", "P_")
    ]
    ord_vars <- select_vars[substr(select_vars, 1, 2) %in% c("O_")]
  } else {
    if (autoguess_nrows == 0) autoguess_nrows <- nrow(data)
    autoguess_nrows <- min(autoguess_nrows, nrow(data))
    data_types <- dt_vartype_autoguess(
      data[1:autoguess_nrows, ..select_vars],
      num_as_categorical_nval
    )

    cl <- sapply(data[, ..select_vars], function(x) class(x)[1])
    to_convert <- names(data_types)[
      data_types != "numeric" & (cl %in% c("numeric", "integer"))
    ]

    if (length(to_convert) > 0) {
      for (ivar in to_convert) {
        if (verbose) {
          cat("\nconverting:", ivar, " as character.")
        }
        data[, (ivar) := as.character(get(ivar))]
      }
    }
    num_vars <- names(data_types)[data_types == "numeric"]
    ord_vars <- names(data_types)[data_types == "ordinal"]
  }

  vars[which(variable %in% num_vars), var_type := "numeric"]
  vars[which(variable %in% ord_vars), var_type := "ordinal"]

  # Categorize remaining variables
  other_vars <- select_vars[!(select_vars %in% c(num_vars))]
  vars[which(variable %in% other_vars), var_type := "categorical"]

  # Check if we have any variables left to analyze
  dt_vars_exp <- unique(c(num_vars, ord_vars, other_vars))
  if (length(dt_vars_exp) == 0) {
    messages <- c(
      messages,
      list(
        ERROR = "No explanatory variable remaining. Check variables and naming conventions if used."
      )
    )
    vars[
      which(variable %in% c(num_vars, ord_vars, other_vars, target)),
      respect_naming_convention := TRUE
    ]
    return(list(
      numeric_vars = NULL,
      ordinal_vars = NULL,
      categorical_vars = NULL,
      all_vars = NULL,
      target = target,
      vars = vars,
      messages = messages,
      status = "error"
    ))
  }

  return(list(
    numeric_vars = num_vars,
    ordinal_vars = ord_vars,
    categorical_vars = other_vars,
    all_vars = dt_vars_exp,
    target = target,
    vars = vars,
    messages = messages,
    status = "ok",
    data = data
  ))
}

#' Create binning functions based on method
#'
#' This function creates appropriate binning functions based on the chosen method
#'
#' @param method character - binning method: "quantile", "clustering", or "smart"
#' @param nbins numeric - number of bins to create
#' @param smart_quantile_by numeric - quantile step for smart binning
#' @param verbose logical - whether to print verbose output
#'
#' @return list of binning functions and configuration
#'
#' @importFrom stats quantile
#' @importFrom data.table data.table
#' @importFrom data.table setnames
binning_factory <- function(
  method = "quantile",
  nbins = 12,
  smart_quantile_by = 0.01,
  verbose = FALSE
) {
  # Storage for cut points and centers
  cutpoints_list <<- list()
  cutcenter_list <<- list()

  # Binning function using quantiles
  binning_quantile <- function(x, variable) {
    # Put a vector from 0 to 1 by 1/nbins
    quantiles <- seq(0, 1, length.out = nbins + 1)
    # Take the value of the quantile for the variable x
    cutpoints <- unname(unique(stats::quantile(x, quantiles, na.rm = TRUE)))

    centers <- cutpoints[-length(cutpoints)] + diff(cutpoints / 2)
    if (length(centers) == 0) centers <- cutpoints

    # Store centers and cutpoints
    cutcenter_list[[variable]] <<- centers
    cutpoints_list[[variable]] <<- cutpoints

    # Find the interval containing each element of x in cutpoints
    return(findInterval(x, cutpoints, rightmost.closed = TRUE))
  }

  # Binning function using clustering
  binning_clustering <- function(x, variable) {
    if (verbose) cat("\nClustering binning for:", variable)

    cl_centers <- Ckmeans.1d.dp(x[!is.na(x)], k = nbins)$centers
    cutcenter_list[[variable]] <<- cl_centers

    cutpoints <- sort(
      unique(
        c(
          cl_centers[-length(cl_centers)] +
            diff(cl_centers / 2),
          range(x, na.rm = TRUE)
        )
      )
    )

    cutpoints_list[[variable]] <<- cutpoints

    # Find the interval containing each element of x in cutpoints
    return(findInterval(x, cutpoints, rightmost.closed = TRUE))
  }

  # Binning function using smart method (quantiles + clustering)
  binning_smart <- function(x, variable) {
    if (verbose) cat("\nSmart binning for:", variable)

    quantiles <- seq(0, 1, length.out = 1 + round(1 / smart_quantile_by))
    nqu <- length(quantiles)
    qu_indices <- (1:nqu) %/% (nqu / nbins)

    # Take quantile values
    qu <- quantile(x, quantiles, na.rm = TRUE)
    qmin <- qu[1]
    qmax <- qu[nqu]

    # Handle infinities
    qu[qu == -Inf] <- min(x[is.finite(x)], na.rm = TRUE)
    qu[qu == Inf] <- max(x[is.finite(x)], na.rm = TRUE)

    # Get unique values
    uqu <- qu[names(qu)[!duplicated(qu_indices)]]
    new_nbin <- min(c(length(unique(uqu)), nqu))

    # Use clustering.sc.dp for smart clustering
    cl_centers <- clustering.sc.dp(
      matrix(qu, ncol = 1),
      k = new_nbin
    )$centers[, 1]

    cutcenter_list[[variable]] <<- cl_centers

    cutpoints <- sort(
      unique(
        c(cl_centers[-length(cl_centers)] + diff(cl_centers / 2), qmin, qmax)
      )
    )

    cutpoints_list[[variable]] <<- cutpoints

    # Find the interval containing each element of x in cutpoints
    return(findInterval(x, cutpoints, rightmost.closed = TRUE))
  }

  # Select the appropriate binning function based on method
  binning_function <- switch(
    method,
    "quantile" = binning_quantile,
    "clustering" = binning_clustering,
    "smart" = binning_smart,
    binning_quantile # default
  )

  return(list(
    binning_function = binning_function,
    cutpoints_list = cutpoints_list,
    cutcenter_list = cutcenter_list
  ))
}

#' Compute statistics for target variable
#'
#' This function computes appropriate statistics based on the target type
#'
#' @param data data.table with the data
#' @param target character - name of the target variable
#' @param target_type character - type of the target variable
#'
#' @return data.table with target statistics
#'
#' @importFrom data.table data.table
#' @importFrom stats quantile sd
compute_target_statistics <- function(data, target, target_type) {
  if (target_type == "numeric") {
    # Compute stats for numeric target
    probs <- c(seq(0.1, 0.9, by = 0.1), 0.01, 0.05, 0.25, 0.75, 0.95, 0.99)
    probs <- probs[order(probs)]

    # Build quantile column expressions
    quantile_exprs <- paste(
      "q",
      round(100 * probs),
      "=quantile(get(target),",
      probs,
      ",na.rm=TRUE)",
      sep = "",
      collapse = ","
    )

    # Full expression to evaluate
    txt <- paste(
      "data[,.(
        avg=mean(get(target), na.rm = TRUE)
        ,std=sd(get(target), na.rm = TRUE)
        ,min=min(get(target), na.rm=TRUE)
        ,",
      quantile_exprs,
      ",max=max(get(target), na.rm=TRUE)
       ,sum=sum(get(target), na.rm=TRUE)
       ,count=.N
       ,nNA=sum(is.na(get(target)))",
      ")]"
    )

    target_stats <- eval(parse(text = txt))
    target_stats[, percNA := nNA / count]
  } else {
    # Compute stats for categorical/binary target
    txt <- paste(
      "data[,.(
       count=.N
       ,nNA=sum(is.na(get(target)))",
      "), by=",
      target,
      "]"
    )

    target_stats <- eval(parse(text = txt))
    target_stats[, percNA := nNA / sum(count)]
    target_stats[, perc := count / sum(count)]
    setnames(target_stats, target, "value")
  }

  return(target_stats)
}

#' Process and match function parameters
#'
#' This function validates and processes key parameters by matching them against
#' their allowed values.
#'
#' @param binning_method character - method for binning numeric variables
#' @param order_label character - order method for labels in output
#' @param target_type character - type of the target variable
#' @param woe_alternate_version character - when to use alternate WOE definition
#'
#' @return list of processed parameters
process_parameters <- function(
  binning_method,
  order_label,
  target_type,
  woe_alternate_version
) {
  # Match arguments with possible values from original function
  binning_method <- match.arg(
    binning_method,
    c("quantile", "clustering", "smart"),
    several.ok = FALSE
  )

  order_label <- match.arg(
    order_label,
    c("auto", "alpha", "count", "props", "means"),
    several.ok = FALSE
  )

  target_type <- match.arg(
    target_type,
    c("autoguess", "binary", "categorical", "numeric"),
    several.ok = FALSE
  )

  woe_alternate_version <- match.arg(
    woe_alternate_version,
    c("if_continuous", "always"),
    several.ok = FALSE
  )

  # Return processed parameters
  return(list(
    binning_method = binning_method,
    order_label = order_label,
    target_type = target_type,
    woe_alternate_version = woe_alternate_version
  ))
}

#' Check for required packages based on selected methods
#'
#' @param binning_method character - binning method selected
#' @param woe_post_cluster logical - whether post-clustering is used
#'
#' @importFrom assertthat assert_that
check_dependencies <- function(binning_method, woe_post_cluster) {
  deps <- c()

  if (binning_method == "clustering") {
      deps <- c(deps, "Ckmeans.1d.dp")
  }

  if (binning_method == "smart") {
    deps <- c(deps, "clustering.sc.dp")
  }

  if (woe_post_cluster) {
    deps <- c(deps, "clustering.sc.dp","Ckmeans.1d.dp")
  }
  if (getOption("targeter.auto_install_deps", FALSE)) {
        pacman::p_load(char = deps)
      }
  assertthat::assert_that(
    all(pacman::p_load(char = deps, install = FALSE)),
    msg = paste(
      'some of targeter following optional packages required for decision trees are not available:',
      paste(deps, collapse = ","))
  )
    
}

#' Prepare data and select variables for analysis
#'
#' @param data data.frame or data.table - the input data
#' @param target character - name of target variable
#' @param select_vars character vector - variables to select
#' @param exclude_vars character vector - variables to exclude
#'
#' @return data.table - processed data
#'
#' @importFrom data.table as.data.table
prepare_data <- function(
  data,
  target,
  select_vars = NULL,
  exclude_vars = NULL
) {
  # Convert to data.table if needed
  if (!inherits(data, "data.table")) {
    data <- data.table::as.data.table(data)
  }

  # Select variables if specified
  if (!is.null(select_vars)) {
    # Ensure target is included
    all_vars <- unique(c(target, select_vars))
    data <- data[, ..all_vars]
  }

  # Exclude variables if specified
  if (!is.null(exclude_vars)) {
    # Make sure target is not excluded
    exclude_vars <- setdiff(exclude_vars, target)
    keep_vars <- setdiff(names(data), exclude_vars)
    data <- data[, ..keep_vars]
  }

  return(data)
}

#' Filter variables with insufficient variance
#'
#' This function removes variables that have only a single unique value,
#' as they provide no discriminatory power for analysis.
#'
#' @param data data.table with the data
#' @param numeric_vars character - vector of numeric variables
#' @param ordinal_vars character - vector of ordinal variables
#' @param categorical_vars character - vector of categorical variables
#' @param vars data.table - metadata about variables
#' @param verbose logical - whether to print verbose output
#'
#' @return list with filtered variable lists and metadata
#'
#' @importFrom data.table data.table uniqueN
filter_variables <- function(
  data,
  numeric_vars,
  ordinal_vars,
  categorical_vars,
  vars,
  verbose = FALSE
) {
  messages <- list()
  all_vars <- c(numeric_vars, ordinal_vars, categorical_vars)

  # Find variables with only one unique value
  single_value_vars <- character(0)

  for (var in all_vars) {
    if (data.table::uniqueN(data[[var]], na.rm = TRUE) <= 1) {
      single_value_vars <- c(single_value_vars, var)
      if (verbose) {
        cat("\nVariable", var, "has only one unique value and will be excluded")
      }
    }
  }

  # If we found any single-value variables, remove them
  if (length(single_value_vars) > 0) {
    messages <- c(
      messages,
      list(
        WARNING = paste(
          "The following variables have been excluded because they have only one unique value:",
          paste(single_value_vars, collapse = ", ")
        )
      )
    )

    # Update variable lists
    numeric_vars <- setdiff(numeric_vars, single_value_vars)
    ordinal_vars <- setdiff(ordinal_vars, single_value_vars)
    categorical_vars <- setdiff(categorical_vars, single_value_vars)
    all_vars <- setdiff(all_vars, single_value_vars)

    # Update metadata
    vars[
      which(vars$variable %in% single_value_vars),
      dropped_one_single_value := TRUE
    ]
  }

  return(list(
    numeric_vars = numeric_vars,
    ordinal_vars = ordinal_vars,
    categorical_vars = categorical_vars,
    all_vars = all_vars,
    vars = vars,
    messages = messages
  ))
}

#' Analyze variables for profiling
#'
#' This function handles the complete process of variable detection, filtering,
#' and type assignment for the targeter analysis.
#'
#' @param data data.table with data to analyze
#' @param target character - name of the target variable
#' @param select_vars character - vector of variables to include
#' @param exclude_vars character - vector of variables to exclude
#' @param naming_conventions logical - whether to enforce naming conventions
#' @param autoguess_nrows numeric - number of rows to use for type detection
#' @param num_as_categorical_nval numeric - threshold for numeric as categorical
#' @param verbose logical - whether to print verbose output
#'
#' @return list with variable classification and metadata
#'
#' @importFrom data.table data.table setnames
#' @importFrom assertthat assert_that
analyze_variables <- function(
  data,
  target,
  select_vars = NULL,
  exclude_vars = NULL,
  naming_conventions = FALSE,
  autoguess_nrows = 1000,
  num_as_categorical_nval = 5,
  verbose = FALSE
) {
  messages <- list()

  # Initial variable detection and classification
  var_detection <- detect_variables_types(
    data = data,
    target = target,
    select_vars = select_vars,
    exclude_vars = exclude_vars,
    naming_conventions = naming_conventions,
    autoguess_nrows = autoguess_nrows,
    num_as_categorical_nval = num_as_categorical_nval,
    verbose = verbose
  )

  # Check for errors in variable detection
  if (var_detection$status == "error") {
    return(var_detection)
  }

  # Extract key information from detection results
  numeric_vars <- var_detection$numeric_vars
  ordinal_vars <- var_detection$ordinal_vars
  categorical_vars <- var_detection$categorical_vars
  all_vars <- var_detection$all_vars
  vars <- var_detection$vars
  target <- var_detection$target
  data <- var_detection$data
  messages <- c(messages, var_detection$messages)

  # Filter variables based on variance (drop variables with only one value)
  vars_filtered <- filter_variables(
    data = data,
    numeric_vars = numeric_vars,
    ordinal_vars = ordinal_vars,
    categorical_vars = categorical_vars,
    vars = vars,
    verbose = verbose
  )

  # Log any message from filtering
  if (length(vars_filtered$messages) > 0) {
    messages <- c(messages, vars_filtered$messages)
  }

  # Check if we still have enough variables after filtering
  if (length(vars_filtered$all_vars) == 0) {
    messages <- c(
      messages,
      list(
        ERROR = "No variables with sufficient variance remaining for analysis"
      )
    )

    return(list(
      numeric_vars = NULL,
      ordinal_vars = NULL,
      categorical_vars = NULL,
      all_vars = NULL,
      target = target,
      vars = vars,
      messages = messages,
      status = "error",
      data = data
    ))
  }

  # Return final results
  return(list(
    numeric_vars = vars_filtered$numeric_vars,
    ordinal_vars = vars_filtered$ordinal_vars,
    categorical_vars = vars_filtered$categorical_vars,
    all_vars = vars_filtered$all_vars,
    target = target,
    vars = vars_filtered$vars,
    messages = messages,
    status = "ok",
    data = data
  ))
}


#' Calculate statistics for variable crossing with target
#'
#' This function computes appropriate statistics based on the target type and variable type
#' for analyzing relationships between explanatory variables and target variables.
#'
#' @param data data.table with the binned data
#' @param variable character - name of the variable to analyze
#' @param target character - name of the target variable
#' @param target_type character - type of the target variable (binary, categorical, numeric)
#' @param useNA character - how to handle NA values ("ifany" or "no")
#' @param bxp_factor numeric - factor for boxplot whiskers calculation
#' @param variable_type character - type of the variable ("numeric" or "character")
#' @param order_label character - method for ordering labels
#'
#' @return list with statistics and metadata
#'
#' @importFrom data.table data.table setnames dcast
#' @importFrom stats sd quantile
calculate_statistics <- function(
  data,
  variable,
  target,
  target_type,
  useNA = "ifany",
  bxp_factor = 1.5,
  variable_type = "character",
  order_label = "auto"
) {
  # Initialize result list
  cross <- list()
  cross$varname <- variable
  cross$targetname <- target

  # Process based on target type
  if (target_type %in% c("binary", "categorical")) {
    # Compute counts by variable and target
    txt <- paste0("data[,.(N=.N),by=.(", variable, ",", target, ")]")
    x <- eval(parse(text = txt))

    # Rename columns
    colnames(x)[1:2] <- c(variable, target)

    # Reshape to have target values as columns
    tab <- data.table::dcast(x, get(variable) ~ get(target), value.var = "N")
  } else if (target_type == "numeric") {
    # Compute statistics for numeric target
    txt <- paste0(
      "data[,.(
          count = .N,
          varsum = sum(get(target), na.rm=TRUE),
          avg = mean(get(target),na.rm=TRUE),
          std = sd(get(target), na.rm=TRUE),
          q25 = quantile(get(target), prob=c(0.25), na.rm=TRUE),
          median = quantile(get(target), prob=c(0.5), na.rm=TRUE),
          q75 = quantile(get(target), prob=c(0.75), na.rm=TRUE)),
          by=.(",
      variable,
      ")]"
    )
    tab <- eval(parse(text = txt))
    setnames(tab, variable, "variable")
  }

  # Handle NA based on user preference
  if (useNA == "no") {
    tab <- tab[!is.na(tab$variable), ]
  }

  # Additional calculations for numeric target
  if (target_type == "numeric") {
    # Calculate interquartile range
    tab[, `:=`(qrange = q75 - q25)]

    # Calculate boxplot min and max
    tab[, `:=`(
      bxp_min = q25 - bxp_factor * qrange,
      bxp_max = q75 + bxp_factor * qrange
    )]
  }

  # Format table labels
  tab <- treat_tab_labels(
    tab,
    variable,
    is_numeric = (variable_type == "numeric"),
    cutpoints_list
  )

  # Set variable type in result
  cross$variable_type <- variable_type

  # Process according to target type
  if (target_type %in% c("binary", "categorical")) {
    # Ensure no NAs in the table
    tab[is.na(tab)] <- 0

    # Calculate proportions by row
    p <- prop.table(as.table(as.matrix(tab)), margin = 1)

    # Calculate proportions by column
    p_col <- prop.table(as.table(as.matrix(tab)), margin = 2)

    # Calculate index (proportion relative to overall proportion)
    ind <- matrix(nrow = length(p[, 1]), ncol = length(colnames(p)))
    for (i in seq_along(colnames(p))) {
      avgpeni <- sum(tab[, i]) / sum(tab)
      ind[, i] <- p[, i] / avgpeni
    }
    colnames(ind) <- colnames(p)
    rownames(ind) <- rownames(p)

    # Store results
    cross$counts <- tab
    cross$props <- p
    cross$index <- ind
    cross$pcol <- p_col
  } else {
    # For numeric target, just store the statistics table
    cross$stats <- tab
  }

  # Determine ordering of labels based on specified method
  order_label_var <- order_label
  if (order_label == "auto") {
    if (variable_type == "numeric") {
      order_label_var <- "alpha"
    } else {
      order_label_var <- "props"
    }
  }

  # Create ordered labels
  if (target_type %in% c("binary", "categorical")) {
    if (order_label_var == "alpha") {
      orderlabel <- rownames(tab)[order(rownames(tab))]
    } else if (order_label_var == "count") {
      tab_total <- cbind(tab, rowSums(tab))
      colnames(tab_total)[ncol(tab_total)] <- "total"
      orderlabel <- rownames(tab)[order(-tab_total[, "total"])]
    } else if (order_label_var == "props") {
      # For binary/categorical, order by first column proportion
      t1 <- p
      orderlabel <- rownames(t1)[order(-t1[, 1])]
    }
  } else {
    # For numeric target
    if (order_label_var == "alpha") {
      orderlabel <- rownames(tab)[order(rownames(tab))]
    } else if (order_label_var == "count") {
      orderlabel <- rownames(tab)[order(-tab[, "count"])]
    } else if (order_label_var %in% c("props", "means")) {
      orderlabel <- rownames(tab)[order(-tab[, "avg"])]
    }
  }

  # Store ordering and levels
  cross$orderlabel <- orderlabel
  cross$levels <- rownames(tab)

  return(cross)
}

#' Process table labels for display
#'
#' Helper function to format variable labels for display
#'
#' @param tab data.table - table to process
#' @param variable character - variable name
#' @param is_numeric logical - whether variable is numeric
#' @param cutpoints_list list - cutpoints for numeric variables
#' @param dec integer - decimals to show
#'
#' @return processed table with formatted row names
#'
#' @importFrom data.table data.table rbindlist setnames
treat_tab_labels <- function(
  tab,
  variable,
  is_numeric,
  cutpoints_list,
  dec = 2
) {
  if (is_numeric) {
    cp <- prettyNum(cutpoints_list[[variable]], digits = dec)
    nbi <- length(cp) - 1 # number of intervals

    if (nbi > 0) {
      labels <- data.table::data.table(
        val = 1:nbi,
        ...label = paste0(
          '[',
          letters[1:nbi],
          '] from ',
          cp[1:(length(cp) - 1)],
          ' to ',
          cp[2:length(cp)]
        )
      )
    } else {
      labels <- data.table::data.table(
        val = 0,
        ...label = paste0('===', cp[1])
      )
    }

    # Add NA label
    label_NA <- data.table::data.table(val = NA, ...label = '[Missing]')
    labels <- data.table::rbindlist(list(label_NA, labels))

    # Merge with original table
    tab <- merge(tab, labels, by.x = 'variable', by.y = 'val', all.x = TRUE)
    tab <- as.data.frame(tab)
    rownames(tab) <- tab$...label
    tab <- tab[, -ncol(tab)] # remove ...label
  } else {
    # Handle character variables
    tab <- as.data.frame(tab)
    tab[[1]] <- as.character(tab[[1]])
    tab[[1]][is.na(tab[[1]])] <- "[Missing]"
    rownames(tab) <- tab[[1]]
  }

  # Drop the variable column
  tab <- tab[, -1, drop = FALSE]

  # Handle empty row names
  rownames(tab)[rownames(tab) == ''] <- '[empty]'

  return(tab)
}

#' Core target profiling function
#'
#' This function performs the target profiling analysis, generating insights about
#' the relationships between a target variable and explanatory variables.
#' It handles all steps of the analysis pipeline including variable detection,
#' binning, statistic calculation, and result formatting.
#'
#' @param data data.frame or data.table - data to analyze
#' @param description_data character - description of the dataset
#' @param target character - name of the target variable
#' @param target_type character - type of target: "autoguess", "binary", "categorical", "numeric"
#' @param target_reference_level any - reference level for binary/categorical targets
#' @param description_target character - description of the target variable
#' @param analysis_name character - name for the analysis
#' @param select_vars character - vector of variables to include
#' @param exclude_vars character - vector of variables to exclude
#' @param nbins numeric - number of bins for numeric variables
#' @param binning_method character - method for binning: "quantile", "clustering", "smart"
#' @param naming_conventions logical - whether to enforce naming conventions
#' @param useNA character - how to handle NA values
#' @param verbose logical - whether to print verbose output
#' @param dec numeric - number of decimal places for display
#' @param order_label character - method for ordering labels
#' @param cont_target_trim numeric - trimming factor for continuous targets
#' @param bxp_factor numeric - factor for boxplot whiskers
#' @param num_as_categorical_nval numeric - threshold for treating numeric as categorical
#' @param autoguess_nrows numeric - rows to use for type detection
#' @param woe_alternate_version character - when to use alternate WOE definition
#' @param woe_shift numeric - shift value for WOE calculation
#' @param woe_post_cluster logical - whether to cluster WOE values
#' @param woe_post_cluster_n numeric - number of WOE clusters
#' @param smart_quantile_by numeric - quantile step for smart binning
#'
#' @return object of class "targeter" with profiling results
#'
#' @importFrom data.table data.table setDT setnames
#' @importFrom assertthat assert_that
targeter_internal <- function(
  data,
  dataname,
  description_data,
  target,
  target_type,
  target_reference_level, # NULL: auto will check for 1, TRUE
  description_target ,
  analysis_name ,
  select_vars ,
  exclude_vars ,
  nbins,
  binning_method ,
  naming_conventions ,
  useNA ,
  verbose,
  dec,
  order_label ,
  cont_target_trim ,
  bxp_factor ,
  num_as_categorical_nval ,
  autoguess_nrows,
  woe_alternate_version ,
  woe_shift,
  woe_post_cluster ,
  woe_post_cluster_n ,
  smart_quantile_by ,
  ...
) {
  # Step 6: Analyze variables
  if (verbose) cat("\nAnalyzing explanatory variables...")
  var_analysis <- analyze_variables(
    data = data,
    target = target,
    select_vars = select_vars,
    exclude_vars = exclude_vars,
    naming_conventions = naming_conventions,
    autoguess_nrows = autoguess_nrows,
    num_as_categorical_nval = num_as_categorical_nval,
    verbose = verbose
  )

  # Check for errors in variable analysis
  if (var_analysis$status == "error") {
    cat(paste(
      names(var_analysis$messages),
      var_analysis$messages,
      sep = ":",
      collapse = "\n"
    ))
    stop(var_analysis$messages$ERROR)
  }

  # Extract variable information
  numeric_vars <- var_analysis$numeric_vars
  ordinal_vars <- var_analysis$ordinal_vars
  categorical_vars <- var_analysis$categorical_vars
  all_vars <- var_analysis$all_vars
  vars <- var_analysis$vars
  data <- var_analysis$data
  var_messages <- var_analysis$messages

  # Step 7: Create binning functions
  if (verbose) cat("\nSetting up variable binning...")
  binning <- binning_factory(
    method = binning_method,
    nbins = nbins,
    smart_quantile_by = smart_quantile_by,
    verbose = verbose
  )
  binning_foo <- binning$binning_function
  #
  # Step 8: Create expression for binning data
  binning_expr <- create_binning_expression(
    target = target,
    num_vars = numeric_vars,
    other_vars = categorical_vars,
    binning_function = "binning_foo"
  )

  if (verbose) cat("\nApplying binning to variables...")

  # Execute binning on the data
  dataCut <- eval(parse(text = binning_expr))

  # Step 9: Compute target statistics
  if (verbose) cat("\nComputing target statistics...")
  target_stats <- compute_target_statistics(data, target, target_type)

  # Step 10: Process each variable crossing
  if (verbose) cat("\nProcessing variable crossings...")
  crossvars <- process_crossings(
    data = dataCut,
    data_original = data,
    all_vars = all_vars,
    numeric_vars = numeric_vars,
    ordinal_vars = ordinal_vars,
    categorical_vars = categorical_vars,
    target = target,
    target_type = target_type,
    target_stats = target_stats,
    target_reference_level = target_reference_level,
    useNA = useNA,
    order_label = order_label,
    cutpoints_list = cutpoints_list,
    center_list = center_list,
    dec = dec,
    verbose = verbose,
    cont_target_trim = cont_target_trim,
    bxp_factor = bxp_factor,
    woe_alternate_version = woe_alternate_version,
    woe_shift = woe_shift,
    woe_post_cluster = woe_post_cluster,
    woe_post_cluster_n = woe_post_cluster_n
  )

  # Filter out variables with only one value
  if (target_type %in% c("binary", "categorical")) {
    vars_one_value <- sapply(crossvars, function(x) nrow(x$counts) == 1)
  } else if (target_type == "numeric") {
    vars_one_value <- sapply(crossvars, function(x) nrow(x$stats) == 1)
  }

  if (any(vars_one_value)) {
    crossvars <- crossvars[which(!(vars_one_value))]
    vars[
      which(
        vars$variable %in% names(vars_one_value)[which(vars_one_value)]
      ),
      dropped_one_single_value := TRUE
    ]
  }

  # Update metadata with profile information
  vars[, has_profile := (variable %in% names(crossvars))]

  # Step 11: Format results
  if (verbose) cat("\nFormatting results...")

  # Combine messages from all steps
  all_messages <- c(target_messages, var_messages)

  # print(vars)
  # Create final output object
  out <- list(
    dataname = dataname,
    target = target,
    description_data = description_data,
    description_target = description_target,
    target_type = target_type,
    target_stats = target_stats,
    analysis_name = analysis_name,
    date = Sys.Date(),
    profiles = crossvars,
    variables = vars,
    messages = all_messages,
    session = targeter_session_info()
  )

  # Add target reference level if applicable
  if (target_type %in% c('binary', 'categorical')) {
    out$target_reference_level <- target_reference_level
  }

  # Set class for dispatching methods
  class(out) <- c("targeter", class(out))

  # Display messages
  if (verbose || length(all_messages) > 0) {
    cat("\n")
    cat(paste(names(all_messages), all_messages, sep = ":", collapse = "\n"))
    cat("\n")
  }

  return(out)
}

#' Create expression for binning data
#'
#' Helper function to create the expression for binning numeric variables
#'
#' @param target character - name of the target variable
#' @param num_vars character vector - names of numeric variables
#' @param other_vars character vector - names of other variables
#' @param binning_function character - the name of binning function to use
#'
#' @return character string with the expression to evaluate
create_binning_expression <- function(
  target,
  num_vars,
  other_vars,
  binning_function
) {
  # Start with target variable
  txt <- paste0("data[,.(", target)

  # Add categorical variables directly
  if (length(other_vars) > 0) {
    txt <- paste0(txt, ",", paste(other_vars, collapse = ","))
  }
  # binning_foo <<- binning_function
  # Add numeric variables with binning function
  if (length(num_vars) > 0) {
    txtQuickcut <- paste0(
      paste0(
        num_vars,
        "=",binning_function,"(",
        num_vars,
        ", variable='",
        num_vars,
        "')"
      ),
      collapse = ","
    )
    txt <- paste0(txt, ",", txtQuickcut)
  }

  # Complete the expression
  txt <- paste0(txt, ")]")

  return(txt)
}

#' Process all variable crossings
#'
#' This function handles all variable crossings with the target
#'
#' @param data data.table - the binned data
#' @param data_original data.table - the original unbinned data
#' @param all_vars character vector - all variables to process
#' @param numeric_vars character vector - numeric variables
#' @param ordinal_vars character vector - ordinal variables
#' @param categorical_vars character vector - categorical variables
#' @param target character - name of the target variable
#' @param target_type character - type of the target
#' @param target_stats data.table - statistics of the target
#' @param target_reference_level any - reference level for target
#' @param useNA character - how to handle NA values
#' @param order_label character - method for ordering labels
#' @param binning list - binning functions and configuration
#' @param dec integer - number of decimal places
#' @param verbose logical - whether to print verbose output
#' @param cont_target_trim numeric - trim factor for continuous targets
#' @param bxp_factor numeric - factor for boxplot whiskers
#' @param woe_alternate_version character - when to use alternate WOE
#' @param woe_shift numeric - shift value for WOE calculation
#' @param woe_post_cluster logical - whether to cluster WOE values
#' @param woe_post_cluster_n numeric - number of WOE clusters
#'
#' @return list of crossvar objects
#'
#' @importFrom data.table data.table
process_crossings <- function(
  data,
  data_original,
  all_vars,
  numeric_vars,
  ordinal_vars,
  categorical_vars,
  target,
  target_type,
  target_stats,
  target_reference_level,
  useNA,
  order_label,
  binning,
  dec,
  verbose,
  cont_target_trim,
  bxp_factor,
  woe_alternate_version,
  woe_shift,
  woe_post_cluster,
  woe_post_cluster_n
) {
  # Initialize result container
  crossvars <- vector(mode = "list", length = length(all_vars))
  names(crossvars) <- all_vars

  # Process each variable
  for (variable in all_vars) {
    if (verbose) cat("\nProcessing variable:", variable)

    # Determine variable type
    if (variable %in% numeric_vars) {
      variable_type <- "numeric"
    } else if (variable %in% ordinal_vars) {
      variable_type <- "ordinal"
    } else {
      variable_type <- "categorical"
    }

    # Calculate statistics
    cross <- calculate_statistics(
      data = data,
      variable = variable,
      target = target,
      target_type = target_type,
      useNA = useNA,
      bxp_factor = bxp_factor,
      variable_type = variable_type,
      order_label = order_label,
      cutpoints_list = binning$cutpoints_list,
      dec = dec
    )

    # Add target information
    cross$target_type <- target_type
    cross$target_stats <- target_stats

    # Calculate WOE/IV for binary and numeric targets
    if (target_type %in% c("binary", "numeric")) {
      # Determine whether to use alternate WOE version
      alternate_version <- ifelse(
        woe_alternate_version == "always",
        TRUE,
        (target_type == "numeric")
      )

      # Prepare data for WOE calculation (trim if needed)
      analysis_data <- data
      if (cont_target_trim > 0 & target_type == "numeric") {
        min_max <- quantile(
          data_original[[target]],
          probs = c(cont_target_trim, 1 - cont_target_trim)
        )
        analysis_data <- analysis_data[
          get(target) >= min_max[1] & get(target) <= min_max[2],
        ]
      }

      # Calculate WOE and IV
      woe_results <- compute_woe_iv(
        data = analysis_data,
        variable = variable,
        target = target,
        target_type = target_type,
        target_reference_level = target_reference_level,
        alternate_version = alternate_version,
        useNA = useNA,
        woe_shift = woe_shift,
        woe_post_cluster = woe_post_cluster,
        woe_post_cluster_n = woe_post_cluster_n,
        variable_type = variable_type,
        cutpoints_list = binning$cutpoints_list
      )

      cross$woe <- woe_results$WOE
      cross$IV <- woe_results$IV
      cross$woe_cluster <- woe_results$has_clusters
    }

    # Add numeric variable cutpoints information
    if (variable %in% numeric_vars) {
      cross$cutpoints <- binning$cutpoints_list[[variable]]
      cross$numcenters <- binning$cutcenter_list[[variable]]
    }

    # Set class for proper method dispatch
    class(cross) <- c(
      "crossvar",
      paste("crossvar", target_type, sep = "_"),
      class(cross)
    )

    # Store in results list
    crossvars[[variable]] <- cross
  }

  return(crossvars)
}

#' Validate inputs for targeter function
#'
#' @param data data.frame or data.table - data to analyze
#' @param target character - name of target variable
#' @param select_vars character vector - variables to select
#' @param exclude_vars character vector - variables to exclude
#' @param description_data character - data description
#' @param description_target character - target description
#' @param analysis_name character - name of analysis
#' @param useNA character - how to handle NA values
#' @param order_label character - method for ordering labels
#' @param nbins numeric - number of bins
#'
#' @return invisible(TRUE) if all validations pass
#'
#' @importFrom assertthat assert_that
validate_inputs <- function(
  data,
  target,
  select_vars,
  exclude_vars,
  description_data,
  description_target,
  analysis_name,
  useNA,
  order_label,
  nbins
) {
  # Check data type
  assertthat::assert_that(
    inherits(data, "data.frame") | inherits(data, "data.table"),
    msg = "Data must be a data.frame or a data.table"
  )

  # Check target
  assertthat::assert_that(
    inherits(target, "character"),
    msg = "Target must be a character"
  )

  assertthat::assert_that(
    target %in% colnames(data),
    msg = "Target doesn't exist in the dataset"
  )

  assertthat::assert_that(
    length(target) == 1,
    msg = "Only one target is permitted"
  )

  assertthat::assert_that(
    length(unique(data[[target]])) > 1,
    msg = "The target contains only one value"
  )

  # Check select_vars
  assertthat::assert_that(
    inherits(select_vars, "character") | is.null(select_vars),
    msg = "select_vars must be a character vector or NULL"
  )

  # Check exclude_vars
  assertthat::assert_that(
    inherits(exclude_vars, "character") | is.null(exclude_vars),
    msg = "exclude_vars must be a character vector or NULL"
  )

  # Check other parameters
  assertthat::assert_that(
    inherits(analysis_name, "character") | is.null(analysis_name),
    msg = "analysis_name must be a character or NULL"
  )

  assertthat::assert_that(
    inherits(description_target, "character") | is.null(description_target),
    msg = "description_target must be a character or NULL"
  )

  assertthat::assert_that(
    inherits(description_data, "character") | is.null(description_data),
    msg = "description_data must be a character or NULL"
  )

  assertthat::assert_that(
    inherits(useNA, "character"),
    msg = "The parameter useNA must be a character"
  )

  assertthat::assert_that(
    inherits(order_label, "character"),
    msg = "The parameter order_label must be a character"
  )

  assertthat::assert_that(
    is.numeric(nbins) | is.integer(nbins),
    msg = "The parameter nbins must be numeric"
  )

  # Return TRUE if all validations pass
  invisible(TRUE)
}

#' Process and match function parameters
#'
#' This function validates and processes key parameters by matching them against
#' their allowed values.
#'
#' @param binning_method character - method for binning numeric variables
#' @param order_label character - order method for labels in output
#' @param target_type character - type of the target variable
#' @param woe_alternate_version character - when to use alternate WOE definition
#' @param useNA character - how to handle NA values
#'
#' @return list of processed parameters
process_parameters <- function(
  binning_method,
  order_label,
  target_type,
  woe_alternate_version,
  useNA
) {
  # Match arguments with possible values
  binning_method <- match.arg(
    binning_method,
    c("quantile", "clustering", "smart"),
    several.ok = FALSE
  )

  order_label <- match.arg(
    order_label,
    c("auto", "alpha", "count", "props", "means"),
    several.ok = FALSE
  )

  target_type <- match.arg(
    target_type,
    c("autoguess", "binary", "categorical", "numeric"),
    several.ok = FALSE
  )

  woe_alternate_version <- match.arg(
    woe_alternate_version,
    c("if_continuous", "always"),
    several.ok = FALSE
  )

  useNA <- match.arg(
    useNA,
    c("ifany", "no"),
    several.ok = FALSE
  )

  # Return processed parameters
  list(
    binning_method = binning_method,
    order_label = order_label,
    target_type = target_type,
    woe_alternate_version = woe_alternate_version,
    useNA = useNA
  )
}


analysis_description_default <- function(
  target,
  target_type,
  description_target,
  description_data,
  analysis_name
) {
  if (is.null(description_data)) {
    description_data <- "Data"
  }
  if (is.null(analysis_name)) {
    analysis_name <- paste("Analysis of ", target, "on data:", description_data)
  }
  if (is.null(description_target)) {
    description_target <- paste0("Target", target, " [", target_type, "]")
  }
  return(list(
    description_data = description_data,
    description_target = description_target,
    analysis_name = analysis_name
  ))
}


#' Calculate statistics for variable crossing with target
#'
#' This function computes appropriate statistics based on the target type and variable type
#' for analyzing relationships between explanatory variables and target variables.
#'
#' @param data data.table with the binned data
#' @param variable character - name of the variable to analyze
#' @param target character - name of the target variable
#' @param target_type character - type of the target variable (binary, categorical, numeric)
#' @param useNA character - how to handle NA values ("ifany" or "no")
#' @param bxp_factor numeric - factor for boxplot whiskers calculation
#' @param variable_type character - type of the variable ("numeric" or "character")
#' @param order_label character - method for ordering labels
#' @param cutpoints_list list - cutpoints for numeric variables (for creating intervals)
#' @param dec integer - number of decimal places for numeric formatting
#'
#' @return list with statistics and metadata
#'
#' @importFrom data.table data.table setnames dcast
#' @importFrom stats sd quantile
calculate_statistics <- function(
  data,
  variable,
  target,
  target_type,
  useNA = "ifany",
  bxp_factor = 1.5,
  variable_type = "character",
  order_label = "auto",
  cutpoints_list = NULL,
  dec = 2
) {
  # Initialize result list
  cross <- list()
  cross$varname <- variable
  cross$targetname <- target

  # Process based on target type
  if (target_type %in% c("binary", "categorical")) {
    # Compute counts by variable and target
    txt <- paste0("data[,.(N=.N),by=.(", variable, ",", target, ")]")
    x <- eval(parse(text = txt))

    # Rename columns
    colnames(x)[1:2] <- c(variable, target)

    # Reshape to have target values as columns
    tab <- data.table::dcast(x, get(variable) ~ get(target), value.var = "N")
  } else if (target_type == "numeric") {
    # Compute statistics for numeric target
    txt <- paste0(
      "data[,.(
          count = .N,
          varsum = sum(get(target), na.rm=TRUE),
          avg = mean(get(target),na.rm=TRUE),
          std = sd(get(target), na.rm=TRUE),
          q25 = quantile(get(target), prob=c(0.25), na.rm=TRUE),
          median = quantile(get(target), prob=c(0.5), na.rm=TRUE),
          q75 = quantile(get(target), prob=c(0.75), na.rm=TRUE)),
          by=.(",
      variable,
      ")]"
    )
    tab <- eval(parse(text = txt))
    setnames(tab, variable, "variable")
  }

  # Handle NA based on user preference
  if (useNA == "no") {
    tab <- tab[!is.na(tab$variable), ]
  }

  # Additional calculations for numeric target
  if (target_type == "numeric") {
    # Calculate interquartile range
    tab[, `:=`(qrange = q75 - q25)]

    # Calculate boxplot min and max
    tab[, `:=`(
      bxp_min = q25 - bxp_factor * qrange,
      bxp_max = q75 + bxp_factor * qrange
    )]
  }

  # Format table labels
  tab <- treat_tab_labels(
    tab,
    variable,
    is_numeric = (variable_type == "numeric"),
    cutpoints_list,
    dec = dec
  )
  setDT
  tab <- tab[, which(!colnames(tab) %in% "variable"), drop = FALSE]
  # Set variable type in result
  cross$variable_type <- variable_type

  # Process according to target type
  if (target_type %in% c("binary", "categorical")) {
    # Ensure no NAs in the table
    tab[is.na(tab)] <- 0

    # Calculate proportions by row
    p <- prop.table(as.table(as.matrix(tab)), margin = 1)

    # Calculate proportions by column
    p_col <- prop.table(as.table(as.matrix(tab)), margin = 2)

    # Calculate index (proportion relative to overall proportion)
    ind <- matrix(nrow = length(p[, 1]), ncol = length(colnames(p)))
    for (i in seq_along(colnames(p))) {
      avgpeni <- sum(tab[, i]) / sum(tab)
      ind[, i] <- p[, i] / avgpeni
    }
    colnames(ind) <- colnames(p)
    rownames(ind) <- rownames(p)

    # Store results
    cross$counts <- tab
    cross$props <- p
    cross$index <- ind
    cross$pcol <- p_col
  } else {
    # For numeric target, just store the statistics table
    cross$stats <- tab
  }

  # Determine ordering of labels based on specified method
  order_label_var <- order_label
  if (order_label == "auto") {
    if (variable_type == "numeric") {
      order_label_var <- "alpha"
    } else {
      order_label_var <- "props"
    }
  }

  # Create ordered labels
  if (target_type %in% c("binary", "categorical")) {
    if (order_label_var == "alpha") {
      orderlabel <- rownames(tab)[order(rownames(tab))]
    } else if (order_label_var == "count") {
      tab_total <- cbind(tab, rowSums(tab))
      colnames(tab_total)[ncol(tab_total)] <- "total"
      orderlabel <- rownames(tab)[order(-tab_total[, "total"])]
    } else if (order_label_var == "props") {
      # For binary/categorical, order by first column proportion
      t1 <- p
      orderlabel <- rownames(t1)[order(-t1[, 1])]
    } else if (
      order_label_var == "ordered" &
        !is.null(attributes(data[[variable]])$levels)
    ) {
      orderlabel <- attributes(data[[variable]])$levels
    }
  } else {
    # For numeric target
    if (order_label_var == "alpha") {
      orderlabel <- rownames(tab)[order(rownames(tab))]
    } else if (order_label_var == "count") {
      orderlabel <- rownames(tab)[order(-tab[, "count"])]
    } else if (order_label_var %in% c("props", "means")) {
      orderlabel <- rownames(tab)[order(-tab[, "avg"])]
    } else if (
      order_label_var == "ordered" &
        !is.null(attributes(data[[variable]])$levels)
    ) {
      orderlabel <- attributes(data[[variable]])$levels
    }
  }

  # Store ordering and levels
  cross$orderlabel <- orderlabel
  cross$levels <- rownames(tab)

  return(cross)
}

#' Process table labels for display
#'
#' Helper function to format variable labels for display
#'
#' @param tab data.table - table to process
#' @param variable character - variable name
#' @param is_numeric logical - whether variable is numeric
#' @param cutpoints_list list - cutpoints for numeric variables
#' @param dec integer - decimals to show
#'
#' @return processed table with formatted row names
#'
#' @importFrom data.table data.table rbindlist setnames
treat_tab_labels <- function(
  tab,
  variable,
  is_numeric,
  cutpoints_list,
  dec = 2
) {
  if (
    is_numeric &&
      !is.null(cutpoints_list) &&
      variable %in% names(cutpoints_list)
  ) {
    cp <- prettyNum(cutpoints_list[[variable]], digits = dec)
    nbi <- length(cp) - 1 # number of intervals

    if (nbi > 0) {
      labels <- data.table::data.table(
        val = 1:nbi,
        ...label = paste0(
          '[',
          letters[1:nbi],
          '] from ',
          cp[1:(length(cp) - 1)],
          ' to ',
          cp[2:length(cp)]
        )
      )
    } else {
      labels <- data.table::data.table(
        val = 0,
        ...label = paste0('===', cp[1])
      )
    }

    # Add NA label
    label_NA <- data.table::data.table(val = NA, ...label = '[Missing]')
    labels <- data.table::rbindlist(list(label_NA, labels))

    # Merge with original table
    tab <- merge(tab, labels, by.x = 'variable', by.y = 'val', all.x = TRUE)
    tab <- as.data.frame(tab)
    rownames(tab) <- tab$...label
    tab <- tab[, -ncol(tab)] # remove ...label
  } else {
    # Handle character variables
    tab <- as.data.frame(tab)
    tab[[1]] <- as.character(tab[[1]])
    tab[[1]][is.na(tab[[1]])] <- "[Missing]"
    rownames(tab) <- tab[[1]]

    # Drop the variable column
    tab <- tab[, -1, drop = FALSE]
  }

  # Handle empty row names
  rownames(tab)[rownames(tab) == ''] <- '[empty]'

  return(tab)
}

process_crossings <- function(
  data,
  data_original,
  all_vars,
  numeric_vars,
  ordinal_vars,
  categorical_vars,
  target,
  target_type,
  target_stats,
  target_reference_level,
  useNA,
  order_label,
  cutpoints_list,
  center_list,
  dec,
  verbose,
  cont_target_trim,
  bxp_factor,
  woe_alternate_version,
  woe_shift,
  woe_post_cluster,
  woe_post_cluster_n
) {
  crossvars <- vector(mode = "list", length = length(all_vars))
  names(crossvars) <- all_vars
  for (variable in all_vars) {
    if (verbose) cat("\nProcessing variable:", variable)
    if (variable %in% numeric_vars) {
      variable_type <- "numeric"
    } else if (variable %in% ordinal_vars) {
      variable_type <- "ordinal"
    } else {
      variable_type <- "categorical"
    }
    cross <- calculate_statistics(
      data = data,
      variable = variable,
      target = target,
      target_type = target_type,
      useNA = useNA,
      bxp_factor = bxp_factor,
      variable_type = variable_type,
      order_label = order_label,
      cutpoints_list = cutpoints_list,
      dec = dec
    )
    cross$target_type <- target_type
    cross$target_stats <- target_stats
    if (target_type %in% c("binary", "numeric")) {
      alternate_version <- ifelse(
        woe_alternate_version == "always",
        TRUE,
        (target_type == "numeric")
      )
      analysis_data <- data
      if (cont_target_trim > 0 & target_type == "numeric") {
        min_max <- quantile(
          data_original[[target]],
          probs = c(cont_target_trim, 1 - cont_target_trim)
        )
        analysis_data <- analysis_data[
          get(target) >= min_max[1] & get(target) <= min_max[2],
        ]
      }
      woe_results <- compute_woe_iv(
        data = analysis_data,
        variable = variable,
        target = target,
        target_type = target_type,
        target_reference_level = target_reference_level,
        alternate_version = alternate_version,
        useNA = useNA,
        woe_shift = woe_shift,
        woe_post_cluster = woe_post_cluster,
        woe_post_cluster_n = woe_post_cluster_n,
        variable_type = variable_type,
        cutpoints_list = cutpoints_list
      )
      cross$woe <- woe_results$WOE
      cross$IV <- woe_results$IV
      cross$target_reference_level <- target_reference_level
      cross$woe_cluster <- woe_results$has_clusters
    }
    if (variable %in% numeric_vars) {
      cross$cutpoints <- cutpoints_list[[variable]]
      cross$numcenters <- cutcenter_list[[variable]]
    }
    class(cross) <- c(
      "crossvar",
      paste("crossvar", target_type, sep = "_"),
      class(cross)
    )
    crossvars[[variable]] <- cross
  }
  return(crossvars)
}


compute_woe_iv <- function(
  data,
  variable,
  target,
  target_type,
  target_reference_level,
  alternate_version = FALSE,
  useNA = "ifany",
  woe_shift = 0.01,
  woe_post_cluster = FALSE,
  woe_post_cluster_n = 6,
  variable_type = "character",
  cutpoints_list = NULL
) {
  if (!(target_type %in% c("binary", "numeric"))) {
    return(list(WOE = NULL, IV = NULL))
  }
  woe_iv <- dt_WOE_IV(
    data,
    var_interest = target,
    var_cross = variable,
    alternate_version = alternate_version,
    useNA = useNA,
    woe_shift = woe_shift,
    target_reference_level = target_reference_level
  )
  WOE <- woe_iv$WOE[, c("variable", "WOE"), with = FALSE]
  WOE <- treat_tab_labels(
    WOE,
    variable,
    is_numeric = (variable_type == "numeric"),
    cutpoints_list
  )
  WOE <- WOE[ , which(!colnames(WOE) %in% "variable"), drop = FALSE]
  IV <- woe_iv$IV
  if (woe_post_cluster) {
    var_nvalues <- data[!is.na(get(variable)), uniqueN(get(variable))]
    if (woe_post_cluster_n < var_nvalues) {
      X <- WOE[rownames(WOE) != "[Missing]", , drop = FALSE]
      if (variable_type == "numeric") {
        cl <- clustering.sc.dp(as.matrix(X), k = woe_post_cluster_n)$cluster
      } else {
        cl <- Ckmeans.1d.dp(X$WOE, k = woe_post_cluster_n)$cluster
      }
      WOE[rownames(WOE) != "[Missing]", "cluster"] <- cl
      WOE[rownames(WOE) == "[Missing]", "cluster"] <- 0
    } else {
      WOE$cluster <- 1
    }
  } else {
    WOE$cluster <- 1
  }
  return(list(
    WOE = WOE,
    IV = IV,
    has_clusters = (length(unique(WOE$cluster)) > 1)
  ))
}


# dt_vartype_autoguess_onevar <- function (data, var, num_as_nominal_nval = 5, ...)
# {
#     assertthat::assert_that(inherits(data, "data.table"), msg = "data is not a data table")
#     if (inherits(data[, get(var)], c("logical")))
#         return("binary")
#     if (inherits(data[, get(var)], c("ordered")))
#         return("ordinal")
#     ncat <- data[, uniqueN(get(var))]
#     if (inherits(data[, get(var)], c("factor", "character"))) {
#         if (ncat == 2)
#             return("binary")
#         else if (ncat == 1)
#             return("unimode")
#         else return("categorical")
#     }
#     if (inherits(data[, get(var)], c("numeric", "integer", "Date",
#         "POSIXct", "POSIXlt", "IDate"))) {
#         if (ncat == 2)
#             return("binary")
#         else if (ncat == 1)
#             return("unimode")
#         else if (ncat <= num_as_nominal_nval)
#             return("categorical")
#         else return("numeric")
#     }
#     return("unknown")
# }

# dt_vartype_autoguess <- function (data, num_as_nominal_nval = 5, ...)
# {
#     assertthat::assert_that(inherits(data, "data.table"), msg = "data is not a data table")
#     sapply(names(data), function(var) dt_vartype_autoguess_onevar(data,
#         var, num_as_nominal_nval = num_as_nominal_nval, ...))
# }

#' Target Variable Profiling
#'
#' @description
#' For each explanatory variable, this function analyzes its relationship with a target
#' variable by calculating statistics, WOE, IV, and other metrics.
#'
#' The analysis includes:
#' \itemize{
#'   \item Automatic or custom binning of numeric variables
#'   \item Contingency tables with counts of each variable class vs. target values
#'   \item Proportions (count per class/target divided by sum of class counts)
#'   \item Index values showing over/under-representation
#'   \item Weight of Evidence and Information Value for binary and numeric targets
#' }
#'
#' @param data data.frame or data.table - Data to analyze
#' @param target character - Name of the target variable to explain
#' @param description_data character - Description of the dataset (optional)
#' @param target_type character - Type of target: "autoguess" (default), "binary", "categorical", or "numeric"
#' @param target_reference_level any - Reference level for binary/categorical targets (if NULL, will be inferred)
#' @param description_target character - Description of the target variable (optional)
#' @param analysis_name character - Name for the analysis (optional)
#' @param select_vars character vector - Variables to include (if NULL, all columns are considered)
#' @param exclude_vars character vector - Variables to exclude from analysis
#' @param nbins integer - Number of bins for numeric variables (default: 12)
#' @param binning_method character - Method for binning: "quantile" (default), "clustering", or "smart"
#' @param naming_conventions logical - Whether to enforce naming conventions (default: FALSE)
#' @param useNA character - How to handle NAs: "ifany" (default) or "no"
#' @param verbose logical - Whether to print detailed progress information (default: FALSE)
#' @param dec integer - Number of decimals for numeric display (default: 2)
#' @param order_label character - Method for ordering labels in output (default: "auto")
#' @param cont_target_trim numeric - Trimming factor for continuous targets, as percentage between 0 and 1 (default: 0.01)
#' @param bxp_factor numeric - Factor for boxplot whiskers calculation (default: 1.5)
#' @param num_as_categorical_nval integer - Threshold for treating numeric as categorical (default: 5)
#' @param autoguess_nrows integer - Rows to use for variable type detection (default: 1000, 0 means all rows)
#' @param woe_alternate_version character - When to use alternate WOE definition: "if_continuous" (default) or "always"
#' @param woe_shift numeric - Shift value for WOE calculation to prevent issues with 0% or 100% classes (default: 0.01)
#' @param woe_post_cluster logical - Whether to cluster WOE values (default: FALSE)
#' @param woe_post_cluster_n integer - Number of clusters for WOE clustering (default: 6)
#' @param smart_quantile_by numeric - Quantile step for smart binning (default: 0.01)
#' @param by_nvars integer - Number of variables to process in each batch (default: 200)
#' @param ... Additional parameters passed to targeter_internal
#'
#' @return An object of class "targeter" with detailed profiling information
#'
#' @importFrom data.table setDT
#' @importFrom assertthat assert_that
#' @export
targeter <- function(
  data,
  target,
  description_data = NULL,
  target_type = c("autoguess", "binary", "categorical", "numeric"),
  target_reference_level = NULL,
  description_target = NULL,
  analysis_name = NULL,
  select_vars = NULL,
  exclude_vars = NULL,
  nbins = 12,
  binning_method = c("quantile", "clustering", "smart"),
  # TODO fix alternate binning method not working
  naming_conventions = getOption(
    "targeter.use_naming_conventions",
    default = FALSE
  ),
  useNA = getOption("targeter.useNA", default = "ifany"),
  verbose = FALSE,
  dec = 2,
  order_label = c("auto", "alpha", "count", "props", "means"),
  cont_target_trim = 0.01,
  bxp_factor = 1.5,
  num_as_categorical_nval = 5,
  autoguess_nrows = 1000,
  woe_alternate_version = c("if_continuous", "always"),
  woe_shift = 0.01,
  woe_post_cluster = FALSE,
  woe_post_cluster_n = 6,
  smart_quantile_by = 0.01,
  by_nvars = 200,
  ...
) {

  dataname <- deparse(substitute(data))
  if (verbose) {
    cat("\nStarting targeter analysis")
    tstart <- Sys.time()
  }
  # Validate core inputs
  assertthat::assert_that(
    inherits(data, "data.frame") || inherits(data, "data.table"),
    msg = "data must be a data.frame or data.table"
  )

  assertthat::assert_that(
    is.character(target) && length(target) == 1,
    msg = "target must be a single character string"
  )

  assertthat::assert_that(
    target %in% colnames(data),
    msg = "target variable not found in data"
  )

  assertthat::assert_that(
    length(unique(data[[target]])) > 1,
    msg = "target variable must have more than one unique value"
  )

  # Validate batch processing parameter
  assertthat::assert_that(
    is.numeric(by_nvars) && by_nvars > 0 && by_nvars %% 1 == 0,
    msg = "by_nvars must be a positive integer"
  )

  # Convert to data.table for efficient processing
  if (!inherits(data, "data.table")) {
    data <- data.table::setDT(copy(data))
  }
  # Step 1: Validate inputs
  if (verbose) cat("\nValidating inputs...")
  validate_inputs(
    data = data,
    target = target,
    select_vars = select_vars,
    exclude_vars = exclude_vars,
    description_data = description_data,
    description_target = description_target,
    analysis_name = analysis_name,
    useNA = useNA,
    order_label = order_label,
    nbins = nbins
  )

  # Step 2: Process parameters
  if (verbose) cat("\nProcessing parameters...")
  params <- process_parameters(
    binning_method = binning_method,
    order_label = order_label,
    target_type = target_type,
    woe_alternate_version = woe_alternate_version,
    useNA = useNA
  )

  binning_method <- params$binning_method
  order_label <- params$order_label
  target_type <- params$target_type
  woe_alternate_version <- params$woe_alternate_version
  useNA <- params$useNA

  # Step 3: Check dependencies based on selected methods
  if (verbose) cat("\nChecking dependencies...")
  check_dependencies(binning_method, woe_post_cluster)

  # Step 4: Prepare data
  if (verbose) cat("\nPreparing data...")
  
  data <- data.table::setDT(data)

  # Step 5: Analyze target variable
  if (verbose) cat("\nAnalyzing target variable...")
  target_analysis <- analyze_target(
    data = data,
    target = target,
    target_type = target_type,
    target_reference_level = target_reference_level,
    num_as_categorical_nval = num_as_categorical_nval,
    autoguess_nrows = autoguess_nrows
  )

  # Check for errors in target analysis
  if (target_analysis$status == "error") {
    cat(paste(
      names(target_analysis$messages),
      target_analysis$messages,
      sep = ":",
      collapse = "\n"
    ))
    stop(target_analysis$messages$ERROR)
  }

  # Extract target properties
  target_type <- target_analysis$type
  target_reference_level <- target_analysis$reference_level
  target_messages <<- target_analysis$messages

  
  analysis_description <- analysis_description_default(
    target,
    target_type,
    description_target,
    description_data = ifelse(is.null(description_data), dataname, description_data),
    analysis_name
  )
  description_data <- analysis_description$description_data
  description_target <- analysis_description$description_target
  analysis_name <- analysis_description$analysis_name


  if (verbose) {
    cat("\nInput validated.")
  }
  # Setup batch processing
  vars <- names(data)
  by_groups <- (1 + seq_along(vars) %/% by_nvars)
  groups <- unique(by_groups)

  # Initialize progress tracking
  if (verbose) {
    cat(sprintf(
      "\nProcessing %d variables in %d groups (batch size: %d)\n",
      length(vars),
      length(groups),
      by_nvars
    ))
  }

  # Prepare result container
  out_tar <- vector(mode = 'list', length(groups))
  success_count <- 0
  error_count <- 0

  # Process each batch
  for (igroup in groups) {
    if (verbose) {
      cat(sprintf('\nProcessing group: %d of %d', igroup, length(groups)))
    }

    # Get variables for this batch
    ivars <- vars[by_groups == igroup]
    ivars <- unique(c(target, ivars))
    data_igroup <- data[, ..ivars]

    # Process batch
    if (verbose) {
      cat(sprintf('\n  Variables in batch: %d', length(ivars)))
    }

    itar <- tryCatch(
      {
        targeter_internal(
          data = data_igroup,
          dataname = dataname,
          target = target,
          description_data = description_data,
          target_type = target_type,
          target_reference_level = target_reference_level,
          description_target = description_target,
          analysis_name = analysis_name,
          select_vars = select_vars,
          exclude_vars = exclude_vars,
          nbins = nbins,
          binning_method = binning_method,
          naming_conventions = naming_conventions,
          useNA = useNA,
          verbose = verbose,
          dec = dec,
          order_label = order_label,
          cont_target_trim = cont_target_trim,
          bxp_factor = bxp_factor,
          num_as_categorical_nval = num_as_categorical_nval,
          autoguess_nrows = autoguess_nrows,
          woe_alternate_version = woe_alternate_version,
          woe_shift = woe_shift,
          woe_post_cluster = woe_post_cluster,
          woe_post_cluster_n = woe_post_cluster_n,
          smart_quantile_by = smart_quantile_by,
          ...
        )
      },
      error = function(e) {
        if (verbose) {
          cat(sprintf("\n  Error in group %d: %s", igroup, e$message))
        }
        return(NULL)
      }
    )

    if (!is.null(itar)) {
      out_tar[[igroup]] <- itar
      success_count <- success_count + 1
      if (verbose) cat(' - Done')
    } else {
      error_count <- error_count + 1
      out_tar[[igroup]] <- NULL
      if (verbose) cat(' - Failed')
    }
  }

  # Filter out NULL results
  out_tar <- out_tar[!sapply(out_tar, is.null)]

  # Combine results if we have any
  if (length(out_tar) > 0) {
    if (verbose) {
      cat(sprintf(
        "\nCombining results from %d successful groups (errors: %d)",
        success_count,
        error_count
      ))
    }
    tar <- targeter:::tbind(out_tar)

    if (verbose) {
      cat(sprintf(
        "\nProfiling complete: %d variables analyzed",
        length(tar$profiles)
      ))

      ## https://stackoverflow.com/questions/6262203/measuring-function-execution-time-in-r
      tstop <- Sys.time()
      cat(
        sprintf(
          "\nTime took %.2f %s",
          tstop - tstart,
          units(difftime(tstop, tstart))
        )
      )
    }

    return(tar)
  } else {
    stop("All processing groups failed - check input data and parameters")
  }
}
