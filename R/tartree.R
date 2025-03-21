# to prevent checks of data.table used variables
# see:  ?globalVariables

if (getRversion() >= "3.1.0")
  utils::globalVariables(
    c(
      "..vars_model",
      "Z_TARGET_PREDICTION",
      "Z_TARGET_PREDICTION_CP",
      "Z_TARGET_PREDICTION_CP_QUANTILE",
      "N_TARGET_PROB",
      "importance",
      "in_tree",
      "roc",
      "rpart",
      "rpart.control",
      "slice_sample",
      "weight_target"
    )
  )


#' @title tartree
#' @description Build a decision tree model following/based on a targeter analysis
#' @param data data.frame or data.table
#' @param tar_object targeter object
#' @param tarsum_object targeter summary object
#' @param target character, target column name  - default: NULL and if tar_object is provided, target is taken from it
#' @param decision_tree_sample numeric, proportion of data to be used for training -  to be betwwen 0 (not included) and 1 (not recommended) default: 0.8
#' @param seed integer, seed for random number generation - default: 42
#' @param predict_prob_cutpoint cutpoint to be used for binary decision - default 0.5
#' @param predict_prob_cutpoint_quantile quantile of probabilities to be used for
#' further additional preduction. Default 0.5. Could be used to see what if we
#' want to create a group of x\% records.
#' @param rpart.control list, control parameters for rpart function 
#' @param ... other parameters to be passed to targeter
#' @details
#' tartree is a function that builds a decision tree model based on a targeter analysis. It is recommended to have pre-computed targeter object and targeter summary object. If not, the function will compute them. The targeter object is used to define the target column and the target type. The targeter summary object is used to define the variables to be used in the decision tree model. The function will split the data into training and validation sets, build the decision tree model, and return it. The decision tree model is a rpart object with additional attributes: tar_object, tarsum_object, and target.
#' @return a targeter decision tree model on top of which a report can be generated with report function
#' @export tartree
#' @examples
#' \dontrun{
#' data(adult)
#' tar_object <- targeter(adult, target = "ABOVE50K")
#' tarsum_object <- summary(tar_object)
#' tar_tree <- tartree(adult, tar_object = tar_object, tarsum_object = tarsum_object)
#' plot(tar_tree)
#' tar_report(tar_tree)
#' }
#' @importFrom data.table setDT
#' @importFrom assertthat assert_that
#' @importFrom pacman p_load
#' @importFrom stats predict

tartree <- function(
  data,
  tar_object = NULL,
  tarsum_object = NULL,
  target = NULL,
  decision_tree_sample = 0.8,
  seed = 42,
  predict_prob_cutpoint = 0.5,
  predict_prob_cutpoint_quantile = 0.5,
  rpart.control = list(
    minsplit = 20,
    minbucket = 8,
    cp = 0.01,
    maxcompete = 4,
    maxsurrogate = 5,
    usesurrogate = 2,
    xval = 10,
    surrogatestyle = 0,
    maxdepth = 3L
  ),
  ...
) {
  assertthat::assert_that(
    inherits(data, "data.frame") | inherits(data, "data.table"),
    msg = "Data must to be a data.frame or a data.table"
  )

  data <- data.table::setDT(data)
  if (is.null(tar_object)) {
    cat(
      "\nNOTE: it is recommanded to have pre-computed targeter object."
    )
    tar_object <- targeter(data, target = target, ...)
  }
  if (is.null(target)) {
    assertthat::assert_that(
      inherits(tar_object, "targeter"),
      msg = "tar_object must to be a targeter object when you don't provide a target"
    )
  } else {
    assertthat::assert_that(
      is.character(target),
      msg = "target must to be a character"
    )
    if (!is.null(tar_object)) {
      assertthat::assert_that(
        target %in% tar_object$target,
        msg = "target not compatible with targeter object target"
      )
    } else {
      assertthat::assert_that(
        target %in% names(data),
        msg = "target must to be a column name of data"
      )
    }
  }
  assertthat::assert_that(
    is.integer(decision_tree_maxdepth) && (decision_tree_maxdepth > 0),
    msg = "decision_tree_maxdepth must to be a positive integer"
  )
  assertthat::assert_that(
    is.numeric(decision_tree_cp),
    msg = "decision_tree_cp must to be a numeric"
  )

  assertthat::assert_that(
    is.numeric(decision_tree_sample),
    msg = "decision_tree_sample must to be a numeric"
  )
  assertthat::assert_that(
    (0 < decision_tree_sample) && (decision_tree_sample <= 1),
    msg = "decision_tree_sample must to be between 0 (excluded) and 1"
  )

  if (is.null(tarsum_object)) {
    cat(
      "\nNOTE: it is recommanded to have pre-computed targeter summary object.\n"
    )
    tarsum_object <- summary(tar_object)
  }
  if (is.null(target)) {
    target <- tar_object$target
  }
  dt_vars_exp <- tarsum_object$varname
  # TODO: add a warning if some of the variables are not in data
  dt_vars_exp <- dt_vars_exp[which(dt_vars_exp %in% names(data))]

  vars_model <- c(dt_vars_exp, target)
  vars_model <- vars_model[vars_model %in% names(data)]

  assertthat::assert_that(
    all(vars_model %in% names(data)),
    msg = "some of required  variables are not in data"
  )

  data_model <- data[, ..vars_model]
  deps <- c("rpart", "dplyr", "pROC")
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
  dataname <- tar_object$dataname

  assertthat::assert_that(
    ncol(data_model) > 1,
    msg = "data_model must to have more than one column"
  )
  assertthat::assert_that(
    nrow(data_model) > 1,
    msg = "data_model must to have more than one row"
  )

  # split train/valid
  set.seed(seed)
  data_model$ID <- 1:nrow(data_model)

  formula_txt <- as.formula(paste(
    "Z_TARGET",
    "~ ",
    paste(dt_vars_exp, collapse = " + ")
  )) # Z_TARGET built after

  if (tar_object$target_type %in% c('binary')) {
    data_model[,
      Z_TARGET := ifelse(get(target) == tar_object$target_reference_level, 1, 0)
    ]
    data_model_train <- slice_sample(
      data_model,
      prop = decision_tree_sample,
      by = "Z_TARGET" # binary target: stratified sampling
    )
    data_model_valid <- data_model[!data_model$ID %in% data_model_train$ID]

    # look in function for categorical targets
    # we will use weight
    weights <- weight_target(
      data_model_train[, unique(c("Z_TARGET", dt_vars_exp)), with = FALSE],
      "Z_TARGET"
    )
    #minsplit <- sum(weights) / 10

    n <- table(data_model[["Z_TARGET"]])
    prior <- n / sum(n)
    #  decision_tree_cp <- 0 # to be put as parametre
    if (all(weights == 1)) {
      mod <- rpart(
        formula_txt,
        data = data_model_train[,
          unique(c("Z_TARGET", dt_vars_exp)),
          with = FALSE
        ],
        method = "class",
        model = TRUE,
        parms = list(prior = prior),
        control = rpart.control
      )
    } else {
      mod <- rpart(
        formula_txt,
        data = data_model_train[,
          unique(c("Z_TARGET", dt_vars_exp)),
          with = FALSE
        ],
        method = "class",
        weights = weights,
        model = TRUE,
        control = rpart.control
      )
    }
  } else {
    #numeric target

    data[, Z_TARGET := get(target)]
    data_model_train <- slice_sample(
      data_model,
      prop = decision_tree_sample
    )

    data_model_valid <- data_model[!data_model$ID %in% data_model_train$ID]

    # minsplit <- 30

    mod <- rpart(
      formula_txt,
      model = TRUE,
      data = data_model_train[,
        unique(c("Z_TARGET", dt_vars_exp)),
        with = FALSE
      ],
      method = "anova",
      control = rpart.control
    )
  }

  # assess model performance and store prediction ----
  if (nrow(data_model_valid) == 0) {
    data_model_valid <- data_model_train
    model_assessed_on <- "train"
  } else {
    model_assessed_on <- "valid"
  }
  predictions <- data.table(
    ID = data_model_valid$ID,
    Z_TARGET = data_model_valid$Z_TARGET,
    Z_TARGET_PREDICTION = stats::predict(mod, data_model_valid, type = "class")
  )

  predictions[,
    N_TARGET_PROB := stats::predict(mod, data_model_valid, type = "prob")[, 2]
  ]
  predictions[,
    Z_TARGET_PREDICTION_CP := ifelse(
      N_TARGET_PROB > predict_prob_cutpoint,
      1,
      0
    )
  ]
  predictions[,
    Z_TARGET_PREDICTION_CP_QUANTILE := ifelse(
      N_TARGET_PROB >= quantile(N_TARGET_PROB, predict_prob_cutpoint_quantile),
      1,
      0
    )
  ]

  predictions[, .N, by = list(Z_TARGET, Z_TARGET_PREDICTION)]
  predictions[, .N, by = list(Z_TARGET, Z_TARGET_PREDICTION_CP_QUANTILE)]

  pROC <- roc(
    predictions$Z_TARGET,
    predictions$N_TARGET_PROB,
    levels = c(0, 1),
    direction = "<"
  )

  ## subset targeter object and targeter summary object

  dt_vars <- mod$frame["var"]
  dt_vars <- unique(dt_vars[dt_vars != "<leaf>"])
  vars_imp <- mod$variable.importance
  vars_imp_df <- data.frame(var = names(vars_imp), importance = round(vars_imp))
  vars_imp_df[["in_tree"]] <- ifelse(vars_imp_df[["var"]] %in% dt_vars, "*", "")
  setDT(vars_imp_df)
  vars_imp_df <- setorder(vars_imp_df, -in_tree, -importance)
  # at min vars: vars_imp_df
  var_nmin <- length(dt_vars)
  n_vars <- max(var_nmin, min(10, nrow(vars_imp_df)))
  vars_imp_df <- vars_imp_df[1:n_vars, ]

  attr(mod, "model_assessed_on") <- model_assessed_on
  attr(mod, "model_predictions") <- predictions

  attr(mod, "pROC") <- pROC
  attr(mod, "model_varimp") <- setDT(vars_imp_df)
  attr(mod, "decision_tree_params") <- list(
    decision_tree_maxdepth = decision_tree_maxdepth,
    decision_tree_cp = decision_tree_cp,
    decision_tree_sample = decision_tree_sample,
    seed = seed
  )

  ## subset targeter object and targeter summary object
  tar_object[["profiles"]] <- tar_object[["profiles"]][which(
    names(tar_object[["profiles"]]) %in% dt_vars
  )]
  # tar_object_model <- tar_object

  # tar_object_model$profiles <- tar_object_model$profiles[dt_vars]
  attr(mod, "tar_object") <- tar_object

  tarsum_model <- tarsum_object[varname %in% dt_vars]

  attr(mod, "tarsum_object") <- tarsum_model

  attr(mod, "target") <- target
  class(mod) <- c("tartree", class(mod))
  return(mod)
}
