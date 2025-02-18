
# sae.projection

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/sae.projection)](https://CRAN.R-project.org/package=sae.projection)
[![R-CMD-check](https://github.com/Alfrzlp/sae.projection/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Alfrzlp/sae.projection/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

## Author

Azka Ubaidillah, Ridson Al Farizal P

## Maintainer

Ridson Al Farizal P <ridsonap@bps.go.id>

## Description

The **sae.projection** package provides a robust tool for *small area
estimation using a projection-based approach*. This method is
particularly beneficial in scenarios involving two surveys, the first
survey collects data solely on auxiliary variables, while the second,
typically smaller survey, collects both the variables of interest and
the auxiliary variables. The package constructs a working model to
predict the variables of interest for each sample in the first survey.
These predictions are then used to estimate relevant indicators for the
desired domains. This condition overcomes the problem of estimation in a
small area when only using the second survey data.

## Installation

You can install the development version of sae.projection from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Alfrzlp/sae.projection")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(sae.projection)
#> Loading required package: tidymodels
#> ── Attaching packages ────────────────────────────────────── tidymodels 1.2.0 ──
#> ✔ broom        1.0.7     ✔ recipes      1.1.0
#> ✔ dials        1.3.0     ✔ rsample      1.2.1
#> ✔ dplyr        1.1.4     ✔ tibble       3.2.1
#> ✔ ggplot2      3.5.1     ✔ tidyr        1.3.1
#> ✔ infer        1.0.7     ✔ tune         1.2.1
#> ✔ modeldata    1.4.0     ✔ workflows    1.1.4
#> ✔ parsnip      1.2.1     ✔ workflowsets 1.1.0
#> ✔ purrr        1.0.2     ✔ yardstick    1.3.1
#> ── Conflicts ───────────────────────────────────────── tidymodels_conflicts() ──
#> ✖ purrr::discard() masks scales::discard()
#> ✖ dplyr::filter()  masks stats::filter()
#> ✖ dplyr::lag()     masks stats::lag()
#> ✖ recipes::step()  masks stats::step()
#> • Use suppressPackageStartupMessages() to eliminate package startup messages
library(dplyr)
```

## Regression

### Data

``` r
df_svy22_income <- df_svy22 %>% filter(!is.na(income))
df_svy23_income <- df_svy23 %>% filter(!is.na(income))
```

### Linear Regression Model

``` r
lm_proj <- projection(
   income ~ age + sex + edu + disability,
   id = 'PSU', weight = 'WEIGHT', strata = 'STRATA',
   domain = c('PROV', 'REGENCY'),
   model = linear_reg(),
   data_model = df_svy22_income,
   data_proj = df_svy23_income,
)

lm_proj$df_result
#>    PROV REGENCY     ypr      var_ypr   rse_ypr
#> 1    35       1 1301174  10618881790  7.919617
#> 2    35       2 1575921   8089890094  5.707383
#> 3    35       3 1515173   8851908817  6.209496
#> 4    35       4 2016174  13493709449  5.761527
#> 5    35       5 1630143   6715811111  5.027170
#> 6    35       6 1737991   7666303467  5.037853
#> 7    35       7 2214592  20119175179  6.404887
#> 8    35       8 1903982  18080432329  7.062225
#> 9    35       9 1773319   4428235969  3.752567
#> 10   35      10 2034861   4602423996  3.333946
#> 11   35      11 1500750  26703134922 10.888621
#> 12   35      12 1722463  10352480692  5.907074
#> 13   35      13 2053568  24669804362  7.648456
#> 14   35      14 2105472  10160099561  4.787397
#> 15   35      15 3737909 144213198696 10.159535
#> 16   35      16 2412471   8240112740  3.762743
#> 17   35      17 2187732   7891875275  4.060655
#> 18   35      18 2117781  21478193260  6.920187
#> 19   35      19 1797575   7877467222  4.937490
#> 20   35      20 2053399   7851002103  4.315084
#> 21   35      21 1691834   4192575324  3.827214
#> 22   35      22 1905703   7757174634  4.621646
#> 23   35      23 2090962   6458363757  3.843395
#> 24   35      24 2223585   9689490994  4.426869
#> 25   35      25 3172802  11755846028  3.417307
#> 26   35      26 1824850  15006851832  6.713016
#> 27   35      27 1558080  25660267931 10.281133
#> 28   35      28 1525220   9180681192  6.282101
#> 29   35      29 1834459  19430440166  7.598595
#> 30   35      71 2541951  43342967547  8.190156
#> 31   35      72 2284458  10360590216  4.455630
#> 32   35      73 2835409  20677060812  5.071410
#> 33   35      74 2602627  56026780853  9.094647
#> 34   35      75 2585689  22048185343  5.742620
#> 35   35      76 2888925  14002070555  4.095999
#> 36   35      77 2554860  21995898018  5.805021
#> 37   35      78 4594493 300406206504 11.929351
#> 38   35      79 2562641  10071273136  3.916106
```

### Random Forest Regression with Hyperparameter Tunning

``` r
rf_proj <- projection(
  income ~ age + sex + edu + disability,
  id = 'PSU', weight = 'WEIGHT', strata = 'STRATA',
  domain = c('PROV', 'REGENCY'),
  model = rand_forest(mtry = tune(), trees = tune(), min_n = tune()),
  data_model = df_svy22_income,
  data_proj = df_svy23_income,
  kfold = 3,
  grid = 20
)

rf_proj$df_result
```

## Classification

### Data

``` r
df_svy22_neet <- df_svy22 %>% 
  filter(between(age, 15, 24))
df_svy23_neet <- df_svy23 %>% 
  filter(between(age, 15, 24))
```

### Logistic Regression

``` r
lr_proj <- projection(
  formula = neet ~ sex + edu + disability,
  id = 'PSU',
  weight = 'WEIGHT',
  strata = 'STRATA',
  domain = c('PROV', 'REGENCY'),
  model = logistic_reg(),
  data_model = df_svy22_neet,
  data_proj = df_svy23_neet
)

lr_proj$df_result
#>    PROV REGENCY       ypr      var_ypr   rse_ypr
#> 1    35       1 0.1883741 0.0009101754 16.015535
#> 2    35       2 0.1761453 0.0005962890 13.863006
#> 3    35       3 0.2627529 0.0008963487 11.394387
#> 4    35       4 0.1501824 0.0005408503 15.485294
#> 5    35       5 0.1970353 0.0007401344 13.807382
#> 6    35       6 0.2236771 0.0004727475  9.720599
#> 7    35       7 0.2339609 0.0004360528  8.925373
#> 8    35       8 0.3226574 0.0008149866  8.847767
#> 9    35       9 0.2947512 0.0005243011  7.768458
#> 10   35      10 0.2439578 0.0006033091 10.068281
#> 11   35      11 0.2767459 0.0010785238 11.866799
#> 12   35      12 0.2453357 0.0006228644 10.172695
#> 13   35      13 0.3123293 0.0005482030  7.496490
#> 14   35      14 0.3277912 0.0008762413  9.030557
#> 15   35      15 0.1347073 0.0002041638 10.607138
#> 16   35      16 0.2359613 0.0006320838 10.654830
#> 17   35      17 0.2293048 0.0003759636  8.455897
#> 18   35      18 0.1947305 0.0006598068 13.190899
#> 19   35      19 0.2014381 0.0008690814 14.634864
#> 20   35      20 0.1471380 0.0006121382 16.815121
#> 21   35      21 0.1766580 0.0010066641 17.960110
#> 22   35      22 0.2105672 0.0006411531 12.025142
#> 23   35      23 0.2568354 0.0007179900 10.432882
#> 24   35      24 0.2224894 0.0004518400  9.553950
#> 25   35      25 0.2033393 0.0006942281 12.957752
#> 26   35      26 0.2489649 0.0011293225 13.498045
#> 27   35      27 0.3110092 0.0018619150 13.874160
#> 28   35      28 0.1703990 0.0004611499 12.602423
#> 29   35      29 0.2939050 0.0012088401 11.829802
#> 30   35      71 0.1395280 0.0006020211 17.585090
#> 31   35      72 0.1424188 0.0005480416 16.437644
#> 32   35      73 0.1783830 0.0004321919 11.654264
#> 33   35      74 0.2186362 0.0006895557 12.010541
#> 34   35      75 0.1700568 0.0005773959 14.130020
#> 35   35      76 0.1880439 0.0006208718 13.250790
#> 36   35      77 0.1415133 0.0005442635 16.485696
#> 37   35      78 0.1759323 0.0002766692  9.454418
#> 38   35      79 0.1786988 0.0004931984 12.427652
```

### LightGBM with Hyperparameter Tunning

``` r
library(bonsai)
show_engines('boost_tree')
#> # A tibble: 7 × 2
#>   engine   mode          
#>   <chr>    <chr>         
#> 1 xgboost  classification
#> 2 xgboost  regression    
#> 3 C5.0     classification
#> 4 spark    classification
#> 5 spark    regression    
#> 6 lightgbm regression    
#> 7 lightgbm classification
lgbm_model <- boost_tree(
  mtry = tune(), trees = tune(), min_n = tune(), tree_depth = tune(), learn_rate = tune(),
  engine = 'lightgbm'
)
```

``` r
lgbm_proj <- projection(
  formula = neet ~ sex + edu + disability,
  id = 'PSU',
  weight = 'WEIGHT',
  strata = 'STRATA',
  domain = c('PROV', 'REGENCY'),
  model = lgbm_model,
  data_model = df_svy22_neet,
  data_proj = df_svy23_neet,
  kfold = 3, 
  grid = 20
)

lgbm_proj$df_result
```
