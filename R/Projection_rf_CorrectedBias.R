#' Projection_rf_CorrectedBias
#'
#' @description
#' **Kim and Rao (2012)**, the synthetic data obtained through the model-assisted projection method can provide a useful tool for
#' efficient domain estimation when the size of the sample in survey 2 is much larger than the size of sample in survey 1.
#'
#' This function projects estimated values from a small survey onto an independent large survey using the random forest algorithm.
#' Although the two surveys are statistically independent, the projection relies on shared auxiliary variables.
#' The process includes data preprocessing, feature selection, model training, and domain-specific estimation based on survey design principles.
#' Additionally, the estimation results incorporate bias correction techniques.
#'
#' @param metadata The metadata for the dataset, it must contain psu, ssu, strata from small survey dataset.
#' @param data_model The training dataset, consisting of auxiliary variables and the target variable.
#' @param target_column The name of the target column in the \code{data_model}.
#' @param data_proj The data for projection (prediction), which needs to be projected using the trained model. It must contain the same auxiliary variables as the \code{data_model}
#' @param domain1 Domain variables for survey estimation (e.g., "province")
#' @param domain2 Domain variables for survey estimation (e.g., "regency")
#' @param psu Primary sampling units, representing the structure of the sampling frame in both the small and large survey datasets.
#' @param ssu Secondary sampling units, representing the structure of the sampling frame in both the small and large survey datasets.
#' @param strata Stratification variable in both the small and large survey datasets, ensuring that specific subgroups are represented.
#' @param weight_proj Weights used in the \code{data_proj} for indirect estimation.
#' @param weight_model Weights used in the \code{data_model} for direct estimation and bias correction.
#' @param split_ratio Proportion of data used for training (default is 0.8, meaning 80% for training and 20% for validation).
#' @param metric The metric used for model evaluation (default is Accuracy, other options include "AUC", "F1", etc.).
#'
#' @return A list containing the following elements:
#' \itemize{
#'    \item \code{model} The trained Random Forest model.
#'    \item \code{importance} Feature importance showing which features contributed most to the model’s predictions.
#'    \item \code{train_accuracy} Accuracy of the model on the training set.
#'    \item \code{validation_accuracy} Accuracy of the model on the validation set.
#'    \item \code{validation_performance} Confusion matrix for the validation set, showing performance metrics like accuracy, precision, recall, etc.
#'    \item \code{data_proj} The projection data with predicted values.
#'    \item \code{Direct} Direct estimations for Domain 1, including estimated values, variance, and relative standard error.
#'    \item \code{Domain1_corrected_bias} Bias-corrected estimations for Domain 1, including estimated values, variance, and relative standard error.
#'    \item \code{Domain2_corrected_bias} Bias-corrected estimations for Domain 2, including estimated values, variance, and relative standard error.
#' }

#' @references
#' \enumerate{
#'    \item Kim, J. K., & Rao, J. N. (2012). Combining data from two independent surveys: a model-assisted approach. Biometrika, 99(1), 85-100.
#' }
#'
#' @export
#' @import caret
#' @import themis
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
#' df_susenas_sep2020 <- df_susenas_sep2020 %>%
#' left_join(df_susenas_mar2020 %>% select(psu, ssu, strata, no_sample, no_household),
#'        by = c('no_sample', 'no_household'),
#'        multiple = 'any'
#')
#'
#' metadata <- df_susenas_sep2020 %>% select(1:6, 34:ncol(.))
#' df_sep20 <- df_susenas_sep2020 %>% select(7:33)
#' df_mar20 <- df_susenas_mar2020
#'
#'
#' # Example usage of the function:
#' proj_rf_nonbiased <- Projection_rf_CorrectedBias(metadata = metadata,
#'                                        data_model = df_sep20,
#'                                        target_column = "uses_public_transport",
#'                                        data_proj = df_mar20,
#'                                        domain1 = "province",
#'                                        domain2 = "regency",
#'                                        psu = "psu",
#'                                        ssu = "ssu",
#'                                        strata = "strata",
#'                                        weight_proj = "weight",
#'                                        weight_model = "weight_pnl",
#'                                        metric = "Accuracy")
#'}
#'
#'@md
Projection_rf_CorrectedBias <- function(metadata, data_model, target_column, data_proj,
                                        domain1, domain2, psu, ssu, strata, weight_proj, weight_model,
                                        split_ratio = 0.8, metric = 'Accuracy') {

  Est_Y <- Est_corrected <-  Estimation_Direct <- Estimation_Domain1 <-  Estimation_Domain2 <- Estimation_Pred <- RSE <- RSE_corrected <- Var_corrected <- Variance <- penimbang_domain1 <- penimbang_domain2 <- NA
  print("Starting preprocessing...")

  # Convert target column to factor
  data_model[[target_column]] <- factor(data_model[[target_column]], levels = c(0, 1), labels = c("No", "Yes"))

  for (col in names(data_model)) {
    if (is.factor(data_model[[col]]) && col != target_column) {
      data_proj[[col]] <- factor(data_proj[[col]], levels = levels(data_model[[col]]))
    }
  }

  print("Preprocessing completed. Starting data split...")

  # Split data model
  set.seed(123)
  train_index <- caret::createDataPartition(data_model[[target_column]], p = split_ratio, list = FALSE)
  train_set <- data_model[train_index, ]
  validation_set <- data_model[-train_index, ]

  print("Data split completed. Starting RFE...")

  # Recursive Feature Elimination (RFE)
  control_rfe <- caret::rfeControl(functions = caret::rfFuncs, method = "cv", number = 5, verbose = TRUE)
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
    cli::cli_abort('RFE did not select any features. Please check the dataset')
    # stop("⚠ ERROR: RFE did not select any features. Please check the dataset.")
  }
  train_set_selected <- train_set[, c(selected_features, target_column)]
  validation_set_selected <- validation_set[, c(selected_features, target_column)]

  print("Features selected. Starting hyperparameter tuning...")

  # Hyperparameter tuning
  control_final <- caret::trainControl(
    method = "cv",
    number = 5,
    classProbs = TRUE,
    sampling = "smote")

  # Training model with tuneLength
  set.seed(123)
  rf_model <- caret::train(
    stats::as.formula(paste(target_column, "~ .")),
    data = train_set_selected,
    method = "ranger",
    trControl = control_final,
    tuneLength = 10,   # Let caret automatically select 10 candidate values
    num.trees = 500,
    importance = "impurity",
    replace = FALSE,
    metric = metric
  )

  print("Model training completed. Starting model evaluation...")

  # Evaluate the model
  data_model$prediction <- stats::predict(rf_model, newdata = data_model)
  data_model$prediction <- factor(data_model$prediction, levels = c("No", "Yes"))

  # Model evaluation
  train_pred <- stats::predict(rf_model, newdata = train_set)
  train_conf_matrix <- caret::confusionMatrix(train_pred, train_set[[target_column]])
  train_accuracy <- train_conf_matrix$overall["Accuracy"]

  validation_pred <- stats::predict(rf_model, newdata = validation_set)
  validation_conf_matrix <- caret::confusionMatrix(validation_pred, validation_set[[target_column]])
  validation_accuracy <- validation_conf_matrix$overall["Accuracy"]

  print("Evaluation completed. Starting prediction on new data...")

  # Predictions on new data
  for (col in selected_features) {
    if (is.factor(data_model[[col]])) {
      data_proj[[col]] <- factor(data_proj[[col]], levels = levels(data_model[[col]]))
    }
  }
  data_proj$Est_Y <- stats::predict(rf_model, newdata = data_proj, na.action = stats::na.pass)
  # View(data_proj)

  data_proj <- data_proj %>%
    dplyr::mutate(Est_Y = factor(Est_Y, levels = c("No", "Yes")))

  print("Prediction completed. Starting indirect estimation for domain...")

  susenas_design <- survey::svydesign(
    ids = stats::as.formula(paste("~", psu, "+", ssu)),
    strata = stats::as.formula(paste("~", strata)),
    weights = stats::as.formula(paste("~", weight_proj)),
    data = data_proj,
    nest = TRUE
  )

  # Estimation for domain1
  result_domain1 <- survey::svyby(
    formula = stats::as.formula("~ Est_Y"),
    by = stats::as.formula(paste("~", domain1)),
    vartype = c('cvpct', 'var'),
    FUN = survey::svymean,
    design = susenas_design
  ) %>%
    dplyr::rename(
      Estimation_Domain1 = "Est_YYes",
      Variance = "var.Est_YYes",
      RSE = "cv%.Est_YYes"
    ) %>%
    dplyr::mutate(
      Estimation_Domain1 = round(Estimation_Domain1 * 100, 2),
      Variance = round(Variance, 6),
      RSE = round(RSE, 2)
    ) %>%
    dplyr::select(
      Estimation_Domain1,
      Variance,
      RSE
    )

  # Estimation for domain2
  result_domain2 <- survey::svyby(
    formula = stats::as.formula("~ Est_Y"),
    by = stats::as.formula(paste("~", "interaction(", domain1, ",", domain2, ")")),
    vartype = c('cvpct', 'var'),
    FUN = survey::svymean,
    design = susenas_design
  ) %>%
    dplyr::rename(
      Estimation_Domain2 = "Est_YYes",
      Variance = "var.Est_YYes",
      RSE = "cv%.Est_YYes"
    ) %>%
    dplyr::mutate(
      Estimation_Domain2 = round(Estimation_Domain2 * 100, 2),
      RSE = round(RSE, 2)
    ) %>%
    dplyr::select(
      Estimation_Domain2,
      Variance,
      RSE
    )

  print("Indirect estimation completed. Starting direct estimation...")

  data_direct <- as.data.frame(cbind(metadata, data_model))
  options(survey.adjust.domain.lonely = TRUE, survey.lonely.psu = 'adjust')

  ssn_design_dir <- survey::svydesign(
    ids = stats::as.formula(paste0("~ ", psu, " + ", ssu)),
    strata = stats::as.formula(paste0("~", strata)),
    weights = stats::as.formula(paste0("~ ", weight_model)),
    data = data_direct,
    nest = TRUE
  )

  result_direct <- survey::svyby(
    formula = stats::as.formula(paste("~", target_column)),
    by = stats::as.formula(paste0("~ ", domain1)),
    vartype = c('cvpct', 'var'),
    FUN = survey::svymean,
    design = ssn_design_dir,
    bys = stats::as.formula(paste0("~ ", domain1))
  ) %>%
    dplyr::rename(
      Estimation_Direct = paste(target_column, "Yes", sep = ""),
      Variance = paste("var.", target_column, "Yes", sep = ""),
      RSE = paste("cv%.", target_column, "Yes", sep = "")
    ) %>%
    dplyr::mutate(
      Estimation_Direct = round(Estimation_Direct * 100, 2),
      Variance = round(Variance, 6),
      RSE = round(RSE, 2)
    ) %>%
    dplyr::select(
      Estimation_Direct,
      Variance,
      RSE
    )

  print("Direct estimation completed. Starting bias correction estimation...")

  ssn_design_pred <- survey::svydesign(
    ids = stats::as.formula(paste0("~ ", psu, " + ", ssu)),
    strata = stats::as.formula(paste0("~", strata)),
    weights = stats::as.formula(paste0("~ ", weight_model)),
    data = data_direct,
    nest = TRUE
  )

  result_pred <- survey::svyby(
    formula = stats::as.formula("~ prediction"),
    by = stats::as.formula(paste0("~ ", domain1)),
    vartype = c('cvpct', 'var'),
    FUN = survey::svymean,
    design = ssn_design_pred,
    bys = stats::as.formula(paste0("~ ", domain1))
  ) %>%
    dplyr::rename(
      Estimation_Pred = paste("predictionYes"),
      Variance = paste("var.predictionYes"),
      RSE = paste("cv%.predictionYes")
    ) %>%
    dplyr::mutate(
      Estimation_Pred = round(Estimation_Pred * 100, 2),
      Variance = round(Variance, 6),
      RSE = round(RSE, 2)
    ) %>%
    dplyr::select(
      Estimation_Pred,
      Variance,
      RSE
    )

  # Calculating weight for projected data
  weight_domain1 <- data_proj %>%
    dplyr::group_by(!!dplyr::sym(domain1)) %>%
    dplyr::summarise(penimbang_domain1 = sum(!!dplyr::sym(weight_proj), na.rm = TRUE))

  weight_domain2 <- data_proj %>%
    dplyr::group_by(!!dplyr::sym(domain1), !!dplyr::sym(domain2)) %>%
    dplyr::summarise(penimbang_domain2 = sum(!!dplyr::sym(weight_proj), na.rm = TRUE), .groups = "drop")

  result_wi <- weight_domain2 %>%
    dplyr::mutate(penimbang_domain1 = weight_domain1$penimbang_domain1[1]) %>%
    dplyr::mutate(wi = penimbang_domain2 / penimbang_domain1)

  # Bias correction calculation for data model
  bias_est <- result_pred$Estimation_Pred - result_direct$Estimation_Direct
  var_bias_est <- result_pred$Variance + result_direct$Variance

  result_corrected_domain2 <- result_domain2 %>%
    dplyr::mutate(
      Est_corrected = result_domain2$Estimation_Domain2 + bias_est,
      Est_corrected = round(Est_corrected, 2),
      Var_corrected = result_domain2$Variance + var_bias_est,
      Var_corrected = round(Var_corrected, 6),
      RSE_corrected = (sqrt(Var_corrected) / (Est_corrected/100)) * 100,
      RSE_corrected = round(RSE_corrected, 2)
    )

  result_corrected_domain1 <- result_domain1 %>%
    dplyr::mutate(
      Est_corrected = sum(result_corrected_domain2$Est_corrected*result_wi$wi),
      Est_corrected = round(Est_corrected, 2),
      Var_corrected = sum((result_wi$wi)^2 * result_corrected_domain2$Var_corrected),
      Var_corrected = round(Var_corrected, 6),
      RSE_corrected = (sqrt(Var_corrected) / (Est_corrected/100)) * 100,
      RSE_corrected = round(RSE_corrected, 2)
    )

  print("Bias-corrected estimation completed. Returning results...")

  # Return results
  return(list(
    model = rf_model,
    importance = caret::varImp(rf_model),
    train_accuracy = train_accuracy,
    validation_accuracy = validation_accuracy,
    validation_performance = validation_conf_matrix,
    data_proj = data_proj,
    Direct = result_direct,
    Domain1_corrected_bias = result_corrected_domain1,
    Domain2_corrected_bias = result_corrected_domain2
  ))
}
