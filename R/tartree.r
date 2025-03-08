# to prevent checks of data.table used variables
# see:  ?globalVariables

if (getRversion() >= "3.1.0")
  utils::globalVariables(
    c(
      "..vars_model",
      "L_TARGET_PREDICTION",
      "L_TARGET_PREDICTION_CP",
      "L_TARGET_PREDICTION_CP_QUANTILE",
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
#' @param decision_tree_maxdepth integer, maximum depth of the tree - default: 3
#' @param decision_tree_cp numeric, complexity parameter - default: 0
#' @param decision_tree_sample numeric, proportion of data to be used for training -  to be betwwen 0 (not included) and 1 (not recommended) default: 0.8
#' @param seed integer, seed for random number generation - default: 42
#' @param predict_prob_cutpoint cutpoint to be used for binary decision - default 0.5
#' @param predict_prob_cutpoint_quantile quantile of probabilities to be used for 
#' further additional preduction. Default 0.5. Could be used to see what if we
#' want to create a group of x\% records. 
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
  decision_tree_maxdepth = 3L,
  decision_tree_cp = 0,
  decision_tree_sample = 0.8,
  predict_prob_cutpoint = 0.5,
  predict_prob_cutpoint_quantile = 0.5,
  seed = 42,
  ...
) {
  assertthat::assert_that(
    inherits(data, "data.frame") | inherits(data, "data.table"),
    msg = "Data must to be a data.frame or a data.table"
  )

  data <- data.table::setDT(data)
  if (is.null(tar_object)) {
    cat(
      "\n- WARNING: it is recommanded to have pre-computed targeter object.\n"
    )
    tar_object <- targeter(data, target = target, ...)
  }
  if (is.null(target))
    assertthat::assert_that(
      inherits(tar_object, "targeter"),
      msg = "tar_object must to be a targeter object when you don't provide a target"
    ) else {
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
      "\n- WARNING: it is recommanded to have pre-computed targeter summart object.\n"
    )
    tarsum_object <- summary(tar_object)
  }
  if (is.null(target)) {
    target <- tar_object$target
  }
  dt_vars_exp <- tarsum_object$varname
  vars_targeter <- c(dt_vars_exp, target)
  vars_model <- c(dt_vars_exp, target)
  assertthat::assert_that(
    all(c(vars_targeter %in% names(data))),
    msg = "some of required  variables are not in data"
  )

  data_model <- data[, ..vars_model]
  deps <- c("explore", "rpart", "dplyr", "pROC")
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
    "L_TARGET",
    "~ ",
    paste(dt_vars_exp, collapse = " + ")
  )) # L_TARGET built after

  if (tar_object$target_type %in% c('binary')) {
    # look in function for categorical targets
    # we will use weight
    data_model[,
      L_TARGET := ifelse(get(target) == tar_object$target_reference_level, 1, 0)
    ]
    data_model_train <- slice_sample(
      data_model,
      prop = decision_tree_sample,
      by = "L_TARGET"
    )
    data_model_valid <- data_model[!data_model$ID %in% data_model_train$ID]

    weights <- weight_target(
      data_model[, unique(c("L_TARGET", dt_vars_exp)), with = FALSE],
      L_TARGET
    )
    minsplit <- sum(weights) / 10

    n <- table(data_model[["L_TARGET"]])
    prior <- n / sum(n)
    #  decision_tree_cp <- 0 # to be put as parametre
    if (all(weights == 1)) {
      mod <- rpart(
        formula_txt,
        data = data_model[, unique(c("L_TARGET", dt_vars_exp)), with = FALSE],
        method = "class",
        model = TRUE,
        parms = list(prior = prior),
        control = rpart.control(
          maxdepth = decision_tree_maxdepth,
          minsplit = minsplit,
          cp = decision_tree_cp
        )
      )
    } else {
      mod <- rpart(
        formula_txt,
        data = data_model[, unique(c("L_TARGET", dt_vars_exp)), with = FALSE],
        method = "class",
        weights = weights,
        model = TRUE,
        control = rpart.control(
          maxdepth = decision_tree_maxdepth,
          minsplit = minsplit,
          cp = decision_tree_cp
        )
      )
    }
  } else {
    #numeric target
    minsplit <- 30
    data[, L_TARGET := get(target)]
    mod <- rpart(
      formula_txt,
      model = TRUE,
      data = data_model[, unique(c("L_TARGET", dt_vars_exp)), with = FALSE],
      method = "anova",
      control = rpart.control(
        maxdepth = decision_tree_maxdepth,
        minsplit = minsplit,
        cp = decision_tree_cp
      )
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
    L_TARGET = data_model_valid$L_TARGET,
    L_TARGET_PREDICTION = stats::predict(mod, data_model_valid, type = "class")
  )

  predictions[,
    N_TARGET_PROB := stats::predict(mod, data_model_valid, type = "prob")[, 2]
  ]
  predictions[,
    L_TARGET_PREDICTION_CP := ifelse(
      N_TARGET_PROB > predict_prob_cutpoint,
      1,
      0
    )
  ]
  predictions[,
    L_TARGET_PREDICTION_CP_QUANTILE := ifelse(
      N_TARGET_PROB >= quantile(N_TARGET_PROB, predict_prob_cutpoint_quantile),
      1,
      0
    )
  ]

  predictions[, .N, by = list(L_TARGET, L_TARGET_PREDICTION)]
  predictions[, .N, by = list(L_TARGET, L_TARGET_PREDICTION_CP_QUANTILE)]

  pROC <- roc(predictions$L_TARGET, predictions$N_TARGET_PROB)

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
  tar_object[["profiles"]] <- tar_object[["profiles"]][which(names(tar_object[["profiles"]]) %in% dt_vars)]
  # tar_object_model <- tar_object
  
  # tar_object_model$profiles <- tar_object_model$profiles[dt_vars]
  attr(mod, "tar_object") <- tar_object

  tarsum_model <- tarsum_object[varname %in% dt_vars]

  attr(mod, "tarsum_object") <- tarsum_model

  attr(mod, "target") <- target
  class(mod) <- c("tartree", class(mod))
  return(mod)
}
