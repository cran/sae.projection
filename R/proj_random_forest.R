#' projection_rf Function
#' @description
#' This function trains a random forest model and performs domain-level estimation **without bias correction**.
#'
#' @param data_model The training dataset, consisting of auxiliary variables and the target variable.
#' @param target_column The name of the target column in the \code{data_model}.
#' @param predictor_cols A vector of predictor column names.
#' @param data_proj The data for projection (prediction), which needs to be projected using the trained model. It must contain the same auxiliary variables as the \code{data_model}
#' @param domain1 Domain variables for survey estimation (e.g., "province")
#' @param domain2 Domain variables for survey estimation (e.g., "regency")
#' @param psu Primary sampling units, representing the structure of the sampling frame.
#' @param ssu Secondary sampling units, representing the structure of the sampling frame.
#' @param strata Stratification variable, ensuring that specific subgroups are represented.
#' @param weights Weights used in the for direct estimation from \code{data_model} and indirect estimation from \code{data_proj}.
#' @param split_ratio Proportion of data used for training (default is 0.8, meaning 80 percent for training and 20 percent for validation).
#' @param metric The metric used for model evaluation (default is Accuracy, other options include "AUC", "F1", etc.).
#'
#' @keywords internal
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
#' @import themis
#' @import caret
#' @importFrom randomForest combine
#' @importFrom ranger importance
#'
projection_rf <- function(data_model, target_column, predictor_cols, data_proj,
                          domain1, domain2, psu, ssu, strata, weights,
                          split_ratio = 0.8, metric = 'Accuracy') {
  Est_Y <- Estimation <- CI_Lower <- CI_Upper <- Variance <- RSE <- NA

  print("Starting preprocessing...")

  # Membuat formula dari target_column dan predictor_cols
  formula <- stats::reformulate(predictor_cols, response = target_column)

  # Menggunakan model.frame untuk mendapatkan data sesuai formula
  modelling <- stats::model.frame(formula, data = data_model)

  # Identifikasi variabel target & prediktor
  predictor_columns <- setdiff(names(modelling), target_column)  # Ambil hanya kolom prediktor

  # Pastikan target adalah faktor
  modelling[[target_column]] <- factor(modelling[[target_column]], levels = c("0", "1"), labels = c("No", "Yes"))

  print("Preprocessing completed. Starting data split...")

  # Split dataset (Training & Validation)
  set.seed(123)
  train_index <- caret::createDataPartition(modelling[[target_column]], p = split_ratio, list = FALSE)
  train_set <- modelling[train_index, ]
  validation_set <- modelling[-train_index, ]

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
    x = train_set[, predictor_columns, drop = FALSE],  # Gunakan drop = FALSE untuk menghindari error jika hanya ada 1 kolom
    y = train_set[[target_column]],
    sizes = c(1:10, 15, 20),
    rfeControl = control_rfe
  )

  print("RFE completed. Selecting features...")

  # Best features based on RFE
  selected_features <- caret::predictors(rfe_results)
  if (length(selected_features) == 0) {
    cli::cli_abort('RFE did not select any features. Please check the dataset', class = "error")
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
    stats::reformulate(selected_features, response = target_column),
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

  # Pastikan data_proj memiliki faktor yang sesuai dengan model
  for (col in selected_features) {
    if (is.factor(modelling[[col]])) {
      data_proj[[col]] <- factor(data_proj[[col]], levels = levels(modelling[[col]]))
    }
  }

  # Prediksi pada data baru
  data_proj$Est_Y <- stats::predict(rf_model, newdata = data_proj, na.action = stats::na.pass)

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

#' projection_rf_CorrectedBias
#'
#' @description
#' This function extends \code{projection_rf} by incorporating **bias correction** for better domain-level estimation.
#'
#' @param data_model The training dataset, consisting of auxiliary variables and the target variable.
#' @param target_column The name of the target column in the \code{data_model}.
#' @param predictor_cols A vector of predictor column names.
#' @param data_proj The data for projection (prediction), which needs to be projected using the trained model. It must contain the same auxiliary variables as the \code{data_model}
#' @param domain1 Domain variables for survey estimation (e.g., "province")
#' @param domain2 Domain variables for survey estimation (e.g., "regency")
#' @param psu Primary sampling units, representing the structure of the sampling frame.
#' @param ssu Secondary sampling units, representing the structure of the sampling frame.
#' @param strata Stratification variable, ensuring that specific subgroups are represented.
#' @param weights Weights used in the for direct estimation from \code{data_model} and indirect estimation from \code{data_proj}.
#' @param split_ratio Proportion of data used for training (default is 0.8, meaning 80 percent for training and 20 percent for validation).
#' @param metric The metric used for model evaluation (default is Accuracy, other options include "AUC", "F1", etc.).
#' @keywords internal
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
#'
projection_rf_CorrectedBias <- function(data_model, target_column, predictor_cols, data_proj,
                                        domain1, domain2, psu, ssu, strata, weights,
                                        split_ratio = 0.8, metric = 'Accuracy') {
  Est_Y <- Est_corrected <-  Estimation_Direct <- Estimation_Domain1 <-  Estimation_Domain2 <- Estimation_Pred <- RSE <- RSE_corrected <- Var_corrected <- Variance <- weight_domain1 <- weight_domain2 <- NA

  print("Starting preprocessing...")

  # Membuat formula menggunakan reformulate
  formula <- stats::reformulate(predictor_cols, response = target_column)

  # Mengonversi target_column menjadi faktor
  data_model[[target_column]] <- factor(data_model[[target_column]], levels = c(0, 1), labels = c("No", "Yes"))

  # Menyamakan faktor pada data proyek agar sesuai dengan data model
  for (col in predictor_cols) {
    if (is.factor(data_model[[col]])) {
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
    x = train_set[, predictor_cols, drop = FALSE],
    y = train_set[[target_column]],
    sizes = c(1:10, 15, 20),
    rfeControl = control_rfe
  )

  print("RFE completed. Selecting features...")

  # Best features based on RFE
  selected_features <- caret::predictors(rfe_results)
  if (length(selected_features) == 0) {
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

  # Training model with selected features
  set.seed(123)
  rf_model <- caret::train(
    stats::reformulate(selected_features, response = target_column),
    data = train_set_selected,
    method = "ranger",
    trControl = control_final,
    tuneLength = 10,
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
  train_pred <- stats::predict(rf_model, newdata = train_set_selected)
  train_conf_matrix <- caret::confusionMatrix(train_pred, train_set_selected[[target_column]])
  train_accuracy <- train_conf_matrix$overall["Accuracy"]

  validation_pred <- stats::predict(rf_model, newdata = validation_set_selected)
  validation_conf_matrix <- caret::confusionMatrix(validation_pred, validation_set_selected[[target_column]])
  validation_accuracy <- validation_conf_matrix$overall["Accuracy"]

  print("Evaluation completed. Starting prediction on new data...")

  # Predictions on new data
  for (col in selected_features) {
    if (is.factor(data_model[[col]])) {
      data_proj[[col]] <- factor(data_proj[[col]], levels = levels(data_model[[col]]))
    }
  }
  data_proj$Est_Y <- stats::predict(rf_model, newdata = data_proj, na.action = stats::na.pass)

  data_proj <- dplyr::mutate(data_proj, Est_Y = factor(Est_Y, levels = c("No", "Yes")))

  print("Prediction completed. Starting indirect estimation for domain...")

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
      Variance = round(Variance, 6),
      RSE = round(RSE, 2)
    ) %>%
    dplyr::select(
      Estimation_Domain2,
      Variance,
      RSE
    )

  print("Indirect estimation completed. Starting direct estimation...")

  options(survey.adjust.domain.lonely = TRUE, survey.lonely.psu = 'adjust')

  ssn_design_dir <- survey::svydesign(
    ids = stats::as.formula(paste0("~ ", psu, " + ", ssu)),
    strata = stats::as.formula(paste0("~", strata)),
    weights = stats::as.formula(paste0("~ ", weights)),
    data = data_model,
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
    weights = stats::as.formula(paste0("~ ", weights)),
    data = data_model,
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
    dplyr::summarise(weight_domain1 = sum(!!dplyr::sym(weights), na.rm = TRUE))

  weight_domain2 <- data_proj %>%
    dplyr::group_by(!!dplyr::sym(domain1), !!dplyr::sym(domain2)) %>%
    dplyr::summarise(weight_domain2 = sum(!!dplyr::sym(weights), na.rm = TRUE), .groups = "drop")

  result_wi <- weight_domain2 %>%
    dplyr::mutate(weight_domain1 = weight_domain1$weight_domain1[1]) %>%
    dplyr::mutate(wi = weight_domain2 / weight_domain1)

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


#' proj_random_forest
#'
#' @description
#' **Kim and Rao (2012)**, the synthetic data obtained through the model-assisted projection method can provide a useful tool for
#' efficient domain estimation when the size of the sample in survey 2 is much larger than the size of sample in survey 1.
#'
#' This function projects estimated values from a small survey onto an independent large survey using the random forest algorithm.
#' The two surveys are statistically independent, but the projection relies on shared auxiliary variables.
#' The process includes data preprocessing, feature selection, model training, and domain-specific estimation based on survey design principles.
#' The function automatically selects standard estimation or bias-corrected estimation based on the parameter \code{bias_correction}.
#'
#' @references
#' \enumerate{
#'    \item Kim, J. K., & Rao, J. N. (2012). Combining data from two independent surveys: a model-assisted approach. Biometrika, 99(1), 85-100.
#' }
#'
#' @inheritParams projection_rf
#' @param bias_correction Logical; if \code{TRUE}, applies bias correction using \code{projection_rf_CorrectedBias}. Default is \code{FALSE}.
#'
#' @return The output of either \code{projection_rf} or \code{projection_rf_CorrectedBias}, depending on \code{bias_correction}.
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(survey)
#' library(caret)
#' library(dplyr)
#'
#' df_susenas_sep2020 <- df_susenas_sep2020 %>%
#' left_join(df_susenas_mar2020 %>% select(psu, ssu, strata, no_sample, no_household),
#'           by = c('no_sample', 'no_household'),
#'           multiple = 'any'
#' )
#'
#' df_sep20 <- df_susenas_sep2020
#' df_mar20 <- df_susenas_mar2020
#' x_predictors <- df_sep20 %>% select(7:32) %>% names()
#'
#' # Run projection_random_forest without bias correction
#' result_standard <- proj_random_forest(
#'                           data_model = df_sep20,
#'                           target_column = "uses_public_transport",
#'                           predictor_cols = x_predictors,
#'                           data_proj = df_mar20,
#'                           domain1 = "province",
#'                           domain2 = "regency",
#'                           psu = "psu",
#'                           ssu = "ssu",
#'                           strata = "strata",
#'                           weights = "weight",
#'                           metric = "Accuracy",
#'                           bias_correction = FALSE)
#' print(result_standard)
#'
#' # Run projection_random_forest with bias correction
#' result_bias_corrected <- proj_random_forest(
#'                           data_model = df_sep20,
#'                           target_column = "uses_public_transport",
#'                           predictor_cols = x_predictors,
#'                           data_proj = df_mar20,
#'                           domain1 = "province",
#'                           domain2 = "regency",
#'                           psu = "psu",
#'                           ssu = "ssu",
#'                           strata = "strata",
#'                           weights = "weight",
#'                           metric = "Accuracy",
#'                           bias_correction = TRUE)
#' print(result_bias_corrected)
#' }
#'@md
proj_random_forest <- function(data_model, target_column, predictor_cols, data_proj,
                               domain1, domain2, psu, ssu, strata, weights,
                               split_ratio = 0.8, metric = 'Accuracy', bias_correction = FALSE) {

  # Check if PSU, SSU, and strata exist in data_model when bias correction is enabled
  if (bias_correction) {
    if (!all(c(psu, ssu, strata) %in% names(data_model))) {
      cli::cli_warn("PSU, SSU, or strata not found in data_model. Calculating indirect estimation without bias correction.", class = "custom_warning")
      bias_correction <- FALSE  # Disable bias correction if PSU, SSU, or strata are missing
    }
  }

  # Select the appropriate function based on bias_correction
  if (bias_correction) {
    print("Bias correction is enabled. Calculating indirect estimation with bias correction.")
    return(projection_rf_CorrectedBias(data_model, target_column, predictor_cols, data_proj,
                                       domain1, domain2, psu, ssu, strata, weights,
                                       split_ratio, metric))
  } else {
    print("Bias correction is disabled. Calculating indirect estimation without bias correction.")
    return(projection_rf(data_model, target_column, predictor_cols, data_proj,
                         domain1, domain2, psu, ssu, strata, weights,
                         split_ratio, metric))
  }
}


