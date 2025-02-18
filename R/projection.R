#' Projection Estimator
#'
#' @description The function addresses the problem of combining information from two or more independent surveys, a common challenge in survey sampling. It focuses on cases where: \cr
#' \itemize{
#'    \item **Survey 1:** A large sample collects only auxiliary information.
#'    \item **Survey 2:** A much smaller sample collects both the variables of interest and the auxiliary variables.
#' }
#' The function implements a model-assisted projection estimation method based on a working model. The working models that can be used include several machine learning models that can be seen in the details section
#'
#' @references
#' \enumerate{
#'  \item Kim, J. K., & Rao, J. N. (2012). Combining data from two independent surveys: a model-assisted approach. Biometrika, 99(1), 85-100.
#' }
#'
#' @param formula  An object of class formula that contains a description of the model to be fitted. The variables included in the formula must be contained in the \code{data_model} dan \code{data_proj}.
#' @param id Column name specifying cluster ids from the largest level to the smallest level, where ~0 or ~1 represents a formula indicating the absence of clusters.
#' @param weight Column name in data_proj representing the survey weight.
#' @param strata 	Column name specifying strata, use NULL for no strata
#' @param domain Column names in data_model and data_proj representing specific domains for which disaggregated data needs to be produced.
#' @param fun A function taking a formula and survey design object as its first two arguments (default = "mean", "total", "varians").
#' @param model The working model to be used in the projection estimator. Refer to the details for the available working models.
#' @param data_model A data frame or a data frame extension (e.g., a tibble) representing the second survey, characterized by a much smaller sample, provides information on both the variable of interest and the auxiliary variables.
#' @param data_proj A data frame or a data frame extension (e.g., a tibble) representing the first survey, characterized by a large sample that collects only auxiliary information or general-purpose variables.
#' @param model_metric A yardstick::metric_set(), or NULL to compute a standard set of metrics (rmse for regression and f1-score for classification).
#' @param kfold The number of partitions of the data set (k-fold cross validation).
#' @param grid A data frame of tuning combinations or a positive integer. The data frame should have columns for each parameter being tuned and rows for tuning parameter candidates. An integer denotes the number of candidate parameter sets to be created automatically.
#' @param parallel_over A single string containing either "resamples" or "everything" describing how to use parallel processing. Alternatively, NULL is allowed, which chooses between "resamples" and "everything" automatically.
#' If "resamples", then tuning will be performed in parallel over resamples alone. Within each resample, the preprocessor (i.e. recipe or formula) is processed once, and is then reused across all models that need to be fit.
#' If "everything", then tuning will be performed in parallel at two levels. An outer parallel loop will iterate over resamples. Additionally, an inner parallel loop will iterate over all unique combinations of preprocessor and model tuning parameters for that specific resample. This will result in the preprocessor being re-processed multiple times, but can be faster if that processing is extremely fast.
#' @param seed A single value, interpreted as an integer
#' @param est_y A logical value indicating whether to return the estimation of \code{y} in \code{data_model}. If \code{TRUE}, the estimation is returned; otherwise, it is not.
#' @param ... Further argument to the \code{\link[survey]{svydesign}}.
#'
#' @return The function returns a list with the following objects (\code{model}, \code{prediction} and \code{df_result}):
#' \code{model} The working model used in the projection.
#' \code{prediction} A vector containing the prediction results from the working model.
#' \code{df_result} A data frame with the following columns:
#'    * \code{domain} The name of the domain.
#'    * \code{ypr} The estimation results of the projection for each domain.
#'    * \code{var_ypr} The sample variance of the projection estimator for each domain.
#'    * \code{rse_ypr} The Relative Standard Error (RSE) in percentage (%).
#'
#' @export
#' @import bonsai
#' @import tidymodels
#' @import lightgbm
#' @import ranger
#'
#'
#' @details
#' The available working models include:
#' \itemize{
#'    \item Linear Regression \code{linear_reg()}
#'    \item Logistic Regression \code{logistic_reg()}
#'    \item Poisson Regression \code{poisson_reg()}
#'    \item Decision Tree \code{decision_tree()}
#'    \item KNN \code{nearest_neighbor()}
#'    \item Naive Bayes \code{naive_bayes()}
#'    \item Multi Layer Perceptron \code{mlp()}
#'    \item Random Forest \code{rand_forest()}
#'    \item Accelerated Oblique Random Forests (Jaeger et al. 2022, Jaeger et al. 2024) \code{rand_forest(engine = 'aorsf')}
#'    \item XGBoost \code{boost_tree(engine = 'xgboost')}
#'    \item LightGBM \code{boost_tree(engine = 'lightgbm')}
#' }
#' A complete list of models can be seen at the following link \href{https://www.tmwr.org/pre-proc-table}{Tidy Modeling With R}
#'
#' @examples
#' \dontrun{
#' library(sae.projection)
#' library(dplyr)
#' library(bonsai)
#'
#' df_svy22_income <- df_svy22 %>% filter(!is.na(income))
#' df_svy23_income <- df_svy23 %>% filter(!is.na(income))
#'
#' # Linear regression
#' lm_proj <- projection(
#'   income ~ age + sex + edu + disability,
#'   id = "PSU", weight = "WEIGHT", strata = "STRATA",
#'   domain = c("PROV", "REGENCY"),
#'   model = linear_reg(),
#'   data_model = df_svy22_income,
#'   data_proj = df_svy23_income,
#'   nest = TRUE
#' )
#'
#' # Random forest regression with hyperparameter tunning
#' rf_proj <- projection(
#'   income ~ age + sex + edu + disability,
#'   id = "PSU", weight = "WEIGHT", strata = "STRATA",
#'   domain = c("PROV", "REGENCY"),
#'   model = rand_forest(mtry = tune(), trees = tune(), min_n = tune()),
#'   data_model = df_svy22_income,
#'   data_proj = df_svy23_income,
#'   kfold = 3,
#'   grid = 10,
#'   nest = TRUE
#' )
#'
#' df_svy22_neet <- df_svy22 %>% filter(between(age, 15, 24))
#' df_svy23_neet <- df_svy23 %>% filter(between(age, 15, 24))
#'
#' # Logistic regression
#' lr_proj <- projection(
#'   formula = neet ~ sex + edu + disability,
#'   id = ~ PSU,
#'   weight = ~ WEIGHT,
#'   strata = ~ STRATA,
#'   domain = ~ PROV + REGENCY,
#'   model = logistic_reg(),
#'   data_model = df_svy22_neet,
#'   data_proj = df_svy23_neet,
#'   nest = TRUE
#' )
#'
#' # LightGBM regression with hyperparameter tunning
#' show_engines("boost_tree")
#' lgbm_model <- boost_tree(
#'   mtry = tune(), trees = tune(), min_n = tune(),
#'   tree_depth = tune(), learn_rate = tune(),
#'   engine = "lightgbm"
#' )
#'
#' lgbm_proj <- projection(
#'   formula = neet ~ sex + edu + disability,
#'   id = "PSU",
#'   weight = "WEIGHT",
#'   strata = "STRATA",
#'   domain = c("PROV", "REGENCY"),
#'   model = lgbm_model,
#'   data_model = df_svy22_neet,
#'   data_proj = df_svy23_neet,
#'   kfold = 3,
#'   grid = 10,
#'   nest = TRUE
#' )
#' }
#' @md
projection <- function(
    formula, id, weight, strata = NULL, domain, fun = "mean", model,
    data_model, data_proj, model_metric, kfold = 3, grid = 10, parallel_over = "resamples", seed = 1, est_y = FALSE, ...) {

  domain_chr <- domain
  id <- .check_variable(id, data_model, data_proj)
  weight <- .check_variable(weight, data_model, data_proj)
  strata <- .check_variable(strata, data_model, data_proj)
  domain <- .check_variable(domain, data_model, data_proj)

  if ((methods::is(domain_chr, 'formula'))) {
    domain_chr <- colnames(stats::model.frame(domain, data_model, na.action = NULL))
  }

  y_name <- as.character(rlang::f_lhs(formula))
  y <- data_model[[y_name]]

  fun <- match.arg(fun, c("mean", "total", "varians"))
  if (fun == "mean") {
    FUN <- survey::svymean
  } else if (fun == "total") {
    FUN <- survey::svytotal
  } else if (fun == "varians") {
    FUN <- survey::svyvar
  } else {
    cli::cli_abort('fun must be "mean", "total", or "varians" not {fun}')
  }

  # spesifikasi model -------------------------------------
  if (class(y) %in% c("integer", "numeric")) {
    type <- "regression"
    if (missing(model_metric)) model_metric <- yardstick::metric_set(yardstick::rmse)
  } else {
    type <- "classification"
    if (missing(model_metric)) model_metric <- yardstick::metric_set(yardstick::f_meas)
  }

  model_spec <- parsnip::set_mode(model, mode = type)
  model_rec <- recipes::recipe(formula, data = data_model) %>%
    recipes::step_dummy(recipes::all_nominal_predictors()) %>%
    recipes::step_zv(recipes::all_predictors())

  model_wf <- workflows::workflow() %>%
    workflows::add_model(model_spec) %>%
    workflows::add_recipe(model_rec)

  # tunning model -------------------------------------
  all_tune <- grepl("tune", as.character(sapply(model[["args"]], rlang::quo_get_expr)))
  no_tune <- all(all_tune == FALSE)

  if (no_tune) {
    final_fit <- parsnip::fit(model_wf, data_model)
  } else {
    # set k-fold cv
    set.seed(seed)
    if (type == "regression") {
      model_fold <- rsample::vfold_cv(data_model, v = kfold)
    } else if (type == "classification") {
      model_fold <- rsample::vfold_cv(data_model, v = kfold, strata = y_name)
    }

    all_cores <- parallel::detectCores(logical = FALSE)
    cl <- parallel::makePSOCKcluster(all_cores)
    doParallel::registerDoParallel(cl)

    set.seed(seed)
    tune_results <- model_wf %>%
      tune::tune_grid(
        resamples = model_fold,
        grid = grid,
        metrics = model_metric,
        control = tune::control_grid(parallel_over = parallel_over, verbose = TRUE, save_pred = TRUE, save_workflow = TRUE)
      )

    parallel::stopCluster(cl)

    best_param <- tune::select_best(tune_results)
    final_fit <- tune::finalize_workflow(model_wf, best_param)
    final_fit <- parsnip::fit(final_fit, data_model)
  }

  final_model <- tune::extract_fit_engine(final_fit)

  # prediksi y pada data survei 1 -------------------------------------
  data_proj$ypr <- stats::predict(final_fit, new_data = data_proj)[[1]]
  prediction <- stats::predict(final_fit, new_data = data_model)[[1]]


  # bias correction from survey 2 -------------------------------------
  if (type == "regression") {
    data_model$bias <- y - prediction
  } else if (type == "classification") {
    data_model$bias <- ifelse(y == prediction, 0, 1)
    data_proj$ypr <- as.numeric(levels(data_proj$ypr))[data_proj$ypr]

    data_model[[y_name]] <- as.numeric(levels(y))[y]
  }

  svy2_design <- survey::svydesign(ids = id, weights = weight, strata = strata, data = data_model, ...)
  est_bias <- survey::svyby(
    formula = ~bias,
    by = domain,
    design = svy2_design,
    FUN = FUN,
    vartype = c("var")
  )
  colnames(est_bias)[colnames(est_bias) == "var"] <- "var_bias"

  # estimasi yhat pada data survei 1 ----------------------------------
  svy1_design <- survey::svydesign(ids = id, weights = weight, strata = strata, data = data_proj, ...)
  est_ypr <- survey::svyby(
    formula = ~ypr,
    by = domain,
    design = svy1_design,
    FUN = FUN,
    vartype = c("var")
  )
  colnames(est_ypr)[colnames(est_ypr) == "var"] <- "var_ypr"

  # hitung yhat pr, var yhatpr dan rse yhatpr -------------------------
  df_result <- dplyr::left_join(est_ypr, est_bias, by = domain_chr)
  ypr <- df_result$ypr + df_result$bias
  var_ypr <- df_result$var_ypr + df_result$var_bias
  rse_ypr <- sqrt(var_ypr) * 100 / ypr

  df_result$ypr <- ypr
  df_result$var_ypr <- var_ypr
  df_result$rse_ypr <- rse_ypr
  df_result[, c("bias", "var_bias")] <- NULL

  all_result <- list(
    model = final_model,
    prediction = data_proj$ypr
  )

  if (est_y) {
    est_y <- survey::svyby(
      formula = stats::as.formula(paste0("~", y_name)),
      by = domain,
      design = svy2_design,
      FUN = FUN,
      vartype = c("var", "cvpct")
    )
    colnames(est_y)[colnames(est_y) == "cv%"] <- "rse"
    all_result$est_y <- est_y
  }

  all_result$projection <- df_result
  return(all_result)
}








.check_variable <- function(variable, data_model, data_proj) {
  if (is.null(variable)) {
    return(variable)
  }

  if (methods::is(variable, "formula")) {
    if (variable == ~1 | variable == ~0) return(variable)

    tryCatch({
      dat <- stats::model.frame(variable, data_model, na.action = NULL)
      return(variable)
    },
    error = function(x){
      cli::cli_abort('variable "{variable}" is not found in the data_model')
    })

    tryCatch({
      dat <- stats::model.frame(variable, data_proj, na.action = NULL)
      return(variable)
    },
    error = function(x){
      cli::cli_abort('variable "{variable}" is not found in the data_proj')
    })

  }else if (methods::is(variable, "character")) {
    if (length(variable) > 1) {
      if (mean(variable %in% colnames(data_model)) == 1) {
        if (mean(variable %in% colnames(data_proj)) == 1) {
          return(stats::as.formula(paste0("~", paste0(variable, collapse = " + "))))
        } else {
          cli::cli_abort('variable "{setdiff(variable, colnames(data_proj))}" is not found in the data_proj')
        }
      } else {
        cli::cli_abort('variable "{setdiff(variable, colnames(data_model))}" is not found in the data_model')
      }
    } else if (variable %in% colnames(data_model)) {
      if (variable %in% colnames(data_proj)) {
        return(stats::as.formula(paste0("~", variable)))
      } else {
        cli::cli_abort('variable "{variable}" is not found in the data_proj')
      }
    } else {
      cli::cli_abort('variable "{variable}" is not found in the data_model')
    }
  } else {
    cli::cli_abort('variable "{variable}" must be character or formula')
  }
}
