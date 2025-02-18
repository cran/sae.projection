#' @title df_svy22: August 2022 National Labor Force Survey Dataset for East Java, Indonesia.
#' @description A dataset from the August 2022 National Labor Force Survey (Sakernas) conducted in East Java, Indonesia.
#' @format A data frame with 74.070 rows and 11 variables with 38 domains.
#'
#' \describe{
#'   \item{PSU}{Primary Sampling Unit}
#'   \item{WEIGHT}{Weight from survey}
#'   \item{PROV}{province code}
#'   \item{REGENCY}{regency/municipality code}
#'   \item{STRATA}{Strata}
#'   \item{income}{Income}
#'   \item{neet}{Not in education employment or training status}
#'   \item{sex}{sex (1: male, 2: female)}
#'   \item{age}{age}
#'   \item{disability}{disability status (0: False, 1: True)}
#'   \item{edu}{last completed education}
#' }
#' @source \url{https://www.bps.go.id}
"df_svy22"

#' @title df_svy23: August 2023 National Labor Force Survey Dataset for East Java, Indonesia.
#' @description A dataset from the August 2023 National Labor Force Survey (Sakernas) conducted in East Java, Indonesia.
#' @format A data frame with 66.245 rows and 11 variables with 38 domains.
#'
#' \describe{
#'   \item{PSU}{Primary Sampling Unit}
#'   \item{WEIGHT}{Weight from survey}
#'   \item{PROV}{province code}
#'   \item{REGENCY}{regency/municipality code}
#'   \item{STRATA}{Strata}
#'   \item{income}{Income}
#'   \item{neet}{Not in education employment or training status}
#'   \item{sex}{sex (1: male, 2: female)}
#'   \item{age}{age}
#'   \item{disability}{disability status (0: False, 1: True)}
#'   \item{edu}{last completed education}
#' }
#' @source \url{https://www.bps.go.id}
"df_svy23"

#' @title df_susenas_sep2020: September 2020 National Socio-Economic Survey (Susenas) Dataset for DKI Jakarta, Indonesia
#' @description A dataset from the September 2020 National Socio-Economic Survey (Susenas) Social Resilience Module, conducted in DKI Jakarta, Indonesia, which is held every three years, presented at the provincial level.
#' @format A data frame with 3655 rows and 33 variables with 6 domains.
#'
#' \describe{
#'   \item{ID}{Unique identifier for each respondent}
#'   \item{no_sample}{Sample number}
#'   \item{no_household}{Household number}
#'   \item{no_member}{Household member number}
#'   \item{weight}{Weight from survey}
#'   \item{province}{Province code}
#'   \item{urban_rural}{Urban or rural classification (1: Urban, 2: Rural)}
#'   \item{marital_status}{Marital status (1: Married, 0: Other)}
#'   \item{sex}{Sex (1: Male, 2: Female)}
#'   \item{age}{Age of the respondent}
#'   \item{attending_school}{Currently attending school (0: No, 1: Yes)}
#'   \item{highest_edu}{Highest education completed (0: Did not complete elementary school, 1: Elementary school, 2: Junior high school, 3: Senior high school, 4: University/College)}
#'   \item{job_status}{Employment status (1: Employed, 0: Not employed)}
#'   \item{sector_type}{Type of employment sector (1: Agriculture, 0: Non-agriculture)}
#'   \item{job_position}{Job position or role}
#'   \item{building_ownership}{Ownership status of residence (1: Owned, 0: Other)}
#'   \item{floor_area}{Floor area of residence (in square meters)}
#'   \item{pension_ins}{Has pension insurance (0: No, 1: Yes)}
#'   \item{old_age_ins}{Has old-age insurance (0: No, 1: Yes)}
#'   \item{work_ins}{Has work insurance (0: No, 1: Yes)}
#'   \item{life_ins}{Has life insurance (0: No, 1: Yes)}
#'   \item{severance_pay}{Receives severance pay (0: No, 1: Yes)}
#'   \item{kks_card}{Has a KKS (Kartu Keluarga Sejahtera) card (0: No, 1: Yes)}
#'   \item{pkh_recipient}{Is the respondent a recipient of PKH (Program Keluarga Harapan) assistance? (0: No, 1: Yes)}
#'   \item{pkh_disbursement}{Location where PKH funds are disbursed}
#'   \item{pkh_food}{PKH funds used for food assistance (0: No, 1: Yes)}
#'   \item{pkh_housing}{PKH funds used for housing assistance (0: No, 1: Yes)}
#'   \item{pkh_healthcare}{PKH funds used for healthcare assistance (0: No, 1: Yes)}
#'   \item{pkh_maternity}{PKH funds used for maternity assistance (0: No, 1: Yes)}
#'   \item{pkh_school}{PKH funds used for school assistance (0: No, 1: Yes)}
#'   \item{pkh_other}{PKH funds used for other types of assistance (0: No, 1: Yes)}
#'   \item{bpnt_program}{Receives BPNT (Bantuan Pangan Non-Tunai) program assistance (0: No, 1: Yes)}
#'   \item{uses_public_transport}{Using public transportation (0: No, 1: Yes), which includes motorized vehicles with specific routes}
#' }
#' @source \url{https://www.bps.go.id}
"df_susenas_sep2020"

#' @title df_susenas_mar2020: Maret 2020 National Socio-Economic Survey (Susenas) Dataset for DKI Jakarta, Indonesia
#' @description A dataset from the March 2020 National Socio-Economic Survey (Susenas) KOR Module, conducted in DKI Jakarta, Indonesia, which is held annually, presented at the regency level.
#' @format A data frame with 18842 rows and 38 variables with 6 domains.
#'
#' \describe{
#'   \item{year}{Year the survey was conducted}
#'   \item{psu}{Primary Sampling Unit (PSU)}
#'   \item{ssu}{Secondary Sampling Unit (SSU)}
#'   \item{strata}{Strata used for sampling}
#'   \item{ID}{Unique identifier for each respondent}
#'   \item{no_sample}{Sample number}
#'   \item{no_household}{Household number}
#'   \item{no_member}{Household member number}
#'   \item{weight}{Weight from survey}
#'   \item{province}{Province code}
#'   \item{regency}{Regency or municipality code}
#'   \item{urban_rural}{Urban or rural classification (1: Urban, 2: Rural)}
#'   \item{marital_status}{Marital status (1: Married, 0: Other)}
#'   \item{sex}{Sex (1: Male, 2: Female)}
#'   \item{age}{Age of the respondent}
#'   \item{attending_school}{Currently attending school (0: No, 1: Yes)}
#'   \item{highest_edu}{Highest education completed (0: Did not complete elementary school, 1: Elementary school, 2: Junior high school, 3: Senior high school, 4: University/College)}
#'   \item{job_status}{Employment status (1: Employed, 0: Not employed)}
#'   \item{sector_type}{Type of employment sector (1: Agriculture, 0: Non-agriculture)}
#'   \item{job_position}{Job position or role}
#'   \item{building_ownership}{Ownership status of residence (1: Owned, 0: Other)}
#'   \item{floor_area}{Floor area of residence (in square meters)}
#'   \item{pension_ins}{Has pension insurance (0: No, 1: Yes)}
#'   \item{old_age_ins}{Has old-age insurance (0: No, 1: Yes)}
#'   \item{work_ins}{Has work insurance (0: No, 1: Yes)}
#'   \item{life_ins}{Has life insurance (0: No, 1: Yes)}
#'   \item{severance_pay}{Receives severance pay (0: No, 1: Yes)}
#'   \item{kks_card}{Has a KKS (Kartu Keluarga Sejahtera) card (0: No, 1: Yes)}
#'   \item{pkh_recipient}{Is the respondent a recipient of PKH (Program Keluarga Harapan) assistance? (0: No, 1: Yes)}
#'   \item{pkh_disbursement}{Location where PKH funds are disbursed}
#'   \item{pkh_food}{PKH funds used for food assistance (0: No, 1: Yes)}
#'   \item{pkh_housing}{PKH funds used for housing assistance (0: No, 1: Yes)}
#'   \item{pkh_healthcare}{PKH funds used for healthcare assistance (0: No, 1: Yes)}
#'   \item{pkh_maternity}{PKH funds used for maternity assistance (0: No, 1: Yes)}
#'   \item{pkh_school}{PKH funds used for school assistance (0: No, 1: Yes)}
#'   \item{pkh_other}{PKH funds used for other types of assistance (0: No, 1: Yes)}
#'   \item{bpnt_program}{Receives BPNT (Bantuan Pangan Non-Tunai) program assistance (0: No, 1: Yes)}
#' }
#' @source \url{https://www.bps.go.id}
"df_susenas_mar2020"


