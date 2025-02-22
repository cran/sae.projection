% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/proj_random_forest.R
\name{proj_random_forest}
\alias{proj_random_forest}
\title{proj_random_forest}
\usage{
proj_random_forest(
  data_model,
  target_column,
  predictor_cols,
  data_proj,
  domain1,
  domain2,
  psu,
  ssu,
  strata,
  weights,
  split_ratio = 0.8,
  metric = "Accuracy",
  bias_correction = FALSE
)
}
\arguments{
\item{data_model}{The training dataset, consisting of auxiliary variables and the target variable.}

\item{target_column}{The name of the target column in the \code{data_model}.}

\item{predictor_cols}{A vector of predictor column names.}

\item{data_proj}{The data for projection (prediction), which needs to be projected using the trained model. It must contain the same auxiliary variables as the \code{data_model}}

\item{domain1}{Domain variables for survey estimation (e.g., "province")}

\item{domain2}{Domain variables for survey estimation (e.g., "regency")}

\item{psu}{Primary sampling units, representing the structure of the sampling frame.}

\item{ssu}{Secondary sampling units, representing the structure of the sampling frame.}

\item{strata}{Stratification variable, ensuring that specific subgroups are represented.}

\item{weights}{Weights used in the for direct estimation from \code{data_model} and indirect estimation from \code{data_proj}.}

\item{split_ratio}{Proportion of data used for training (default is 0.8, meaning 80 percent for training and 20 percent for validation).}

\item{metric}{The metric used for model evaluation (default is Accuracy, other options include "AUC", "F1", etc.).}

\item{bias_correction}{Logical; if \code{TRUE}, applies bias correction using \code{projection_rf_CorrectedBias}. Default is \code{FALSE}.}
}
\value{
The output of either \code{projection_rf} or \code{projection_rf_CorrectedBias}, depending on \code{bias_correction}.
}
\description{
\strong{Kim and Rao (2012)}, the synthetic data obtained through the model-assisted projection method can provide a useful tool for
efficient domain estimation when the size of the sample in survey 2 is much larger than the size of sample in survey 1.

This function projects estimated values from a small survey onto an independent large survey using the random forest algorithm.
The two surveys are statistically independent, but the projection relies on shared auxiliary variables.
The process includes data preprocessing, feature selection, model training, and domain-specific estimation based on survey design principles.
The function automatically selects standard estimation or bias-corrected estimation based on the parameter \code{bias_correction}.
}
\examples{
\donttest{
library(survey)
library(caret)
library(dplyr)

df_susenas_sep2020 <- df_susenas_sep2020 \%>\%
left_join(df_susenas_mar2020 \%>\% select(psu, ssu, strata, no_sample, no_household),
          by = c('no_sample', 'no_household'),
          multiple = 'any'
)

df_sep20 <- df_susenas_sep2020
df_mar20 <- df_susenas_mar2020
x_predictors <- df_sep20 \%>\% select(7:32) \%>\% names()

# Run projection_random_forest without bias correction
result_standard <- proj_random_forest(
                          data_model = df_sep20,
                          target_column = "uses_public_transport",
                          predictor_cols = x_predictors,
                          data_proj = df_mar20,
                          domain1 = "province",
                          domain2 = "regency",
                          psu = "psu",
                          ssu = "ssu",
                          strata = "strata",
                          weights = "weight",
                          metric = "Accuracy",
                          bias_correction = FALSE)
print(result_standard)

# Run projection_random_forest with bias correction
result_bias_corrected <- proj_random_forest(
                          data_model = df_sep20,
                          target_column = "uses_public_transport",
                          predictor_cols = x_predictors,
                          data_proj = df_mar20,
                          domain1 = "province",
                          domain2 = "regency",
                          psu = "psu",
                          ssu = "ssu",
                          strata = "strata",
                          weights = "weight",
                          metric = "Accuracy",
                          bias_correction = TRUE)
print(result_bias_corrected)
}
}
\references{
\enumerate{
\item Kim, J. K., & Rao, J. N. (2012). Combining data from two independent surveys: a model-assisted approach. Biometrika, 99(1), 85-100.
}
}
