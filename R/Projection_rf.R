#' Projection_rf
#'
#' @description
#' **Kim and Rao (2012)**, the synthetic data obtained through the model-assisted projection method can provide a useful tool for
#' efficient domain estimation when the size of the sample in survey 2 is much larger than the size of sample in survey 1.
#'
#' This function projects estimated values from a small survey onto an independent large survey using the random forest algorithm.
#' Although the two surveys are statistically independent, the projection relies on shared auxiliary variables.
#' The process includes data preprocessing, feature selection, model training, and domain-specific estimation based on survey design principles.
#'
#' @param data_model The training dataset, consisting of auxiliary variables and the target variable.
#' @param target_column The name of the target column in the \code{data_model}.
#' @param data_proj The data for projection (prediction), which needs to be projected using the trained model. It must contain the same auxiliary variables as the \code{data_model}
#' @param domain1 Domain variables for survey estimation (e.g., "province")
#' @param domain2 Domain variables for survey estimation (e.g., "regency")
#' @param psu Primary sampling units, representing the structure of the sampling frame from \code{data_proj}.
#' @param ssu Secondary sampling units, representing the structure of the sampling frame from \code{data_proj}.
#' @param strata Stratification variable in the \code{data_proj}, ensuring that specific subgroups are represented.
#' @param weights Weights used in the \code{data_proj} for indirect estimation.
#' @param split_ratio Proportion of data used for training (default is 0.8, meaning 80% for training and 20% for validation).
#' @param metric The metric used for model evaluation (default is Accuracy, other options include "AUC", "F1", etc.).
#'
#' @return
#' A list containing the following elements:
#' \itemize{
#'    \item \code{model} The trained Random Forest model.
#'    \item \code{importance} Feature importance showing which features contributed most to the model’s predictions.
#'    \item \code{train_accuracy} Accuracy of the model on the training set.
#'    \item \code{validation_accuracy} Accuracy of the model on the validation set.
#'    \item \code{validation_performance} Confusion matrix for the validation set, showing performance metrics like accuracy, precision, recall, etc.
#'    \item \code{data_proj} The projection data with predicted values.
#'    \item \code{Domain1} Estimations for Domain 1, including estimated values, variance, and relative standard error.
#'    \item \code{Domain2} Estimations for Domain 2, including estimated values, variance, and relative standard error.
#' }
#'
#'
#' @references
#' \enumerate{
#'    \item Kim, J. K., & Rao, J. N. (2012). Combining data from two independent surveys: a model-assisted approach. Biometrika, 99(1), 85-100.
#' }
#'
#' @export
#' @import themis
#' @import caret
#' @importFrom randomForest combine
#' @importFrom ranger importance
#'
#' @examples
#' \donttest{
#' library(survey)
#' library(caret)
#' library(dplyr)
#' library(themis)
#' library(randomForest)
#'
#' df_sep20 <- df_susenas_sep2020 %>% select(-c(1:6))
#' df_mar20 <- df_susenas_mar2020
#'
#' proj_rf <- Projection_rf(data_model = df_sep20,
#'                           target_column = "uses_public_transport",
#'                           data_proj = df_mar20,
#'                           domain1 = "province",
#'                           domain2 = "regency",
#'                           psu = "psu",
#'                           ssu = "ssu",
#'                           strata = "strata",
#'                           weights = "weight",
#'                           metric = "Accuracy")
#' }
#'
#' @md
Projection_rf <- function(data_model, target_column, data_proj,
                          domain1, domain2, psu, ssu, strata, weights,
                          split_ratio = 0.8, metric = 'Accuracy') {

  Est_Y <- Estimation <- CI_Lower <- CI_Upper <- Variance <- RSE <- NA
  print("Starting preprocessing...")

  # Convert target column to factor
  data_model[[target_column]] <- factor(data_model[[target_column]],
                                        levels = c("0", "1"), labels = c("No", "Yes"))

  for (col in names(data_model)) {
    if (is.factor(data_model[[col]]) && col != target_column) {
      data_proj[[col]] <- factor(data_proj[[col]], levels = levels(data_model[[col]]))
    }
  }

  print("Preprocessing completed. Starting data split...")

  # Split dataset (Training & Validation)
  set.seed(123)
  train_index <- caret::createDataPartition(data_model[[target_column]], p = split_ratio, list = FALSE)
  train_set <- data_model[train_index, ]
  validation_set <- data_model[-train_index, ]

  print("Data split completed. Starting RFE...")

  # Recursive Feature Elimination (RFE)
  control_rfe <- caret::rfeControl(
    functions = caret::rfFuncs,
    method = "cv",
    number = 5,
    verbose = TRUE
  )

  set.seed(123)
  rfe_results <- caret::rfe(
    x = train_set[, -which(names(train_set) == target_column)],
    y = train_set[[target_column]],
    sizes = c(1:10, 15, 20),
    rfeControl = control_rfe
  )

  print("RFE completed. Selecting features...")

  # Best features based on RFE
  selected_features <- caret::predictors(rfe_results)
  if (length(selected_features) == 0) {
    # stop("⚠ ERROR: RFE did not select any features. Please check the dataset.")
    cli::cli_abort('RFE did not select any features. Please check the dataset')
  }

  train_set_selected <- train_set[, c(selected_features, target_column)]
  validation_set_selected <- validation_set[, c(selected_features, target_column)]

  print("Features selected. Starting hyperparameter tuning...")

  # Hyperparameter tuning
  control_final <- caret::trainControl(
    method = "cv",
    number = 5,
    classProbs = TRUE,
    sampling = "smote"
  )

  print("Hyperparameter tuning completed. Training model...")

  # Training model with tuneLength
  set.seed(123)
  rf_model <- caret::train(
    stats::as.formula(paste(target_column, "~ .")),
    data = train_set_selected,
    method = "ranger",
    trControl = control_final,
    tuneLength = 10,
    num.trees = 500,
    importance = "impurity",
    replace = FALSE,
    metric = metric
  )

  print("Model trained. Starting model evaluation...")

  # Model evaluation
  train_pred <- stats::predict(rf_model, newdata = train_set_selected)
  train_conf_matrix <- caret::confusionMatrix(train_pred, train_set_selected[[target_column]])
  train_accuracy <- train_conf_matrix$overall["Accuracy"]

  validation_pred <- stats::predict(rf_model, newdata = validation_set_selected)
  validation_conf_matrix <- caret::confusionMatrix(validation_pred, validation_set_selected[[target_column]])
  validation_accuracy <- validation_conf_matrix$overall["Accuracy"]

  print("Evaluation completed. Starting predictions on new data...")

  # Predictions on new data
  for (col in selected_features) {
    if (is.factor(data_model[[col]])) {
      data_proj[[col]] <- factor(data_proj[[col]], levels = levels(data_model[[col]]))
    }
  }
  data_proj$Est_Y <- stats::predict(rf_model, newdata = data_proj, na.action = stats::na.pass)
  # View(data_proj)

  print("Predictions completed. Starting indirect estimation on domain...")

  data_proj <- dplyr::mutate(data_proj, Est_Y = factor(Est_Y, levels = c("No", "Yes")))

  # Survey design
  susenas_design <- survey::svydesign(
    ids = stats::as.formula(paste("~", psu, "+", ssu)),
    strata = stats::as.formula(paste("~", strata)),
    weights = stats::as.formula(paste("~", weights)),
    data = data_proj,
    nest = TRUE
  )

  # Estimation for domain1
  result_domain1 <- survey::svyby(
    formula = stats::as.formula("~ Est_Y"),
    by = stats::as.formula(paste("~", domain1)),
    vartype = c('cvpct', 'var', 'ci'),
    FUN = survey::svymean,
    design = susenas_design
  ) %>%
    dplyr::rename(
      Estimation = "Est_YYes",
      CI_Lower = "ci_l.Est_YYes",
      CI_Upper = "ci_u.Est_YYes",
      Variance = "var.Est_YYes",
      RSE = "cv%.Est_YYes"
    ) %>%
    dplyr::mutate(
      Estimation = round(Estimation * 100, 2),
      CI_Lower = round(CI_Lower * 100, 2),
      CI_Upper = round(CI_Upper * 100, 3),
      Variance = round(Variance, 6),
      RSE = round(RSE, 2)
    ) %>%
    dplyr::select(
      Estimation,
      CI_Lower,
      CI_Upper,
      Variance,
      RSE
    )

  # Estimation for domain2
  result_domain2 <- survey::svyby(
    formula = stats::as.formula("~ Est_Y"),
    by = stats::as.formula(paste("~", "interaction(", domain1, ",", domain2, ")")),
    vartype = c('cvpct', 'var', 'ci'),
    FUN = survey::svymean,
    design = susenas_design
  ) %>%
    dplyr::rename(
      Estimation = "Est_YYes",
      CI_Lower = "ci_l.Est_YYes",
      CI_Upper = "ci_u.Est_YYes",
      Variance = "var.Est_YYes",
      RSE = "cv%.Est_YYes"
    ) %>%
    dplyr::mutate(
      Estimation = round(Estimation * 100, 2),
      CI_Lower = round(CI_Lower * 100, 2),
      CI_Upper = round(CI_Upper * 100, 2),
      Variance = round(Variance, 6),
      RSE = round(RSE, 2)
    ) %>%
    dplyr::select(
      Estimation,
      CI_Lower,
      CI_Upper,
      Variance,
      RSE
    )

  print("Estimation completed. Returning results...")

  # Return results
  return(list(
    model = rf_model,
    importance = caret::varImp(rf_model),
    train_accuracy = train_accuracy,
    validation_accuracy = validation_accuracy,
    validation_performance = validation_conf_matrix,
    data_proj = data_proj,
    Domain1 = result_domain1,
    Domain2 = result_domain2
  ))
}
