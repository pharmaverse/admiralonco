#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Create Test Data ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(magrittr)

adsl <- tibble::tribble(
  ~USUBJID, ~TRTSDTC,
  "1",      "2020-01-01",
  "2",      "2019-12-12",
  "3",      "2019-11-11",
  "4",      "2019-12-30",
  "5",      "2020-01-01",
  "6",      "2020-02-02",
  "7",      "2020-02-02",
  "8",      "2020-04-01"
) %>%
  dplyr::mutate(
    TRTSDT = lubridate::ymd(TRTSDTC),
    STUDYID = "XX1234"
  )

adrs <- tibble::tribble(
  ~USUBJID, ~ADTC,        ~AVALC,
  "1",      "2020-01-01", "PR",
  "1",      "2020-02-01", "CR",
  "1",      "2020-02-16", "NE",
  "1",      "2020-03-01", "CR",
  "1",      "2020-04-01", "SD",
  "2",      "2020-01-01", "SD",
  "2",      "2020-02-01", "PR",
  "2",      "2020-03-01", "SD",
  "2",      "2020-03-13", "CR",
  "3",      "2019-11-12", "CR",
  "3",      "2019-12-02", "CR",
  "3",      "2020-01-01", "SD",
  "4",      "2020-01-01", "PR",
  "4",      "2020-03-01", "SD",
  "4",      "2020-04-01", "SD",
  "4",      "2020-05-01", "PR",
  "4",      "2020-05-15", "NON-CR/NON-PD",
  "5",      "2020-01-01", "PR",
  "5",      "2020-01-10", "SD",
  "5",      "2020-01-20", "PR",
  "5",      "2020-05-15", "NON-CR/NON-PD",
  "6",      "2020-02-06", "PR",
  "6",      "2020-02-16", "CR",
  "6",      "2020-03-30", "PR",
  "7",      "2020-02-06", "PR",
  "7",      "2020-02-16", "CR",
  "7",      "2020-04-01", "NE"
) %>%
  dplyr::mutate(PARAMCD = "OVR") %>%
  dplyr::bind_rows(tibble::tribble(
    ~USUBJID, ~ADTC,        ~AVALC,
    "9",      "2020-02-16", "Y"
  ) %>%
    dplyr::mutate(PARAMCD = "PD")) %>%
  dplyr::mutate(
    ADT = lubridate::ymd(ADTC),
    STUDYID = "XX1234"
  ) %>%
  dplyr::select(-ADTC) %>%
  admiral::derive_vars_merged(
    dataset_add = adsl,
    by_vars = dplyr::vars(STUDYID, USUBJID),
    new_vars = dplyr::vars(TRTSDT)
  )

pd_date <- admiral::date_source(
  dataset_name = "adrs",
  date = ADT,
  filter = PARAMCD == "PD"
)

# derive_param_bor, Test 1 ----
testthat::test_that("derive_param_bor Test 1: No source_pd", {
  
  aval_fun_pass <- function(arg) {
    dplyr::case_when(
      arg == "CR" ~ 1,
      arg == "PR" ~ 2,
      arg == "SD" ~ 3,
      arg == "NON-CR/NON-PD" ~ 4,
      arg == "PD" ~ 5,
      arg == "NE" ~ 6,
      arg == "MISSING" ~ 7,
      TRUE ~ NA_real_
    ) }
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # default  BOR, All Subjects have a record after reference date ----
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  actual_01 <- derive_param_bor(adrs,
                                dataset_adsl     = adsl,
                                filter_source    = PARAMCD == "OVR",
                                source_pd        = NULL,
                                source_datasets  = NULL,
                                reference_date   = TRTSDT,
                                ref_start_window = 28,
                                aval_fun         = aval_fun_pass,
                                set_values_to    = admiral::vars(PARAMCD = "BOR",
                                                                 PARAM   = "Best Overall Response"))
  
  expected_01 <- dplyr::bind_rows(
    adrs,
    tibble::tribble(
      ~USUBJID, ~ADTC,         ~AVALC,    ~AVAL,  ~TRTSDTC,
      "1",      "2020-02-01", "CR",      1,       "2020-01-01",
      "2",      "2020-03-13", "CR",      1,       "2019-12-12",
      "3",      "2019-11-12", "CR",      1,       "2019-11-11",
      "4",      "2020-01-01", "PR",      2,       "2019-12-30",
      "5",      "2020-01-01", "PR",      2,       "2020-01-01",
      "6",      "2020-02-16", "CR",      1,       "2020-02-02",
      "7",      "2020-02-16", "CR",      1,       "2020-02-02",
      "8",      "",           "MISSING", 7,       "2020-04-01"
    ) %>%
      dplyr::mutate(ADT     = lubridate::ymd(ADTC),
                    TRTSDT  = lubridate::ymd(TRTSDTC),
                    STUDYID = "XX1234",
                    PARAMCD = "BOR",
                    PARAM   = "Best Overall Response") %>%
      dplyr::select(-ADTC, -TRTSDTC))
  
  admiral::expect_dfs_equal(base    = expected_01,
                            compare = actual_01,
                            keys    = c("USUBJID", "PARAMCD", "ADT"))
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Subjects only have records less than reference date ----
  # REsponse is PR and CR so will be included
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  # set subject 7 to only have records before ADT, expected will be the same as prevously
  adrs_01                             <- adrs
  adrs_01$ADT[adrs_01$USUBJID == "7"][1:3] <- c(lubridate::ymd("2020-01-01"),
                                                lubridate::ymd("2020-01-02"),
                                                lubridate::ymd("2020-01-03"))
  
  actual_02 <- derive_param_bor(adrs_01,
                                dataset_adsl     = adsl,
                                filter_source    = PARAMCD == "OVR",
                                source_pd        = NULL,
                                source_datasets  = NULL,
                                reference_date   = TRTSDT,
                                ref_start_window = 28,
                                aval_fun         = aval_fun_pass,
                                set_values_to    = admiral::vars(PARAMCD = "BOR",
                                                                 PARAM   = "Best Overall Response"))
  
  #expected will be the same as previous test excetp ADT for subject 7
  expected_02 <- expected_01
  expected_02$ADT[expected_02$USUBJID == "7"][1:4] <- c(lubridate::ymd("2020-01-01"),
                                                        lubridate::ymd("2020-01-02"),
                                                        lubridate::ymd("2020-01-03"),
                                                        lubridate::ymd("2020-01-02"))
  
  admiral::expect_dfs_equal(base    = expected_02,
                            compare = actual_02,
                            keys    = c("USUBJID", "PARAMCD", "ADT"))
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Subjects only have records less than reference date ----
  # Response is SD will not be included and response will be NE
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  # set subject 7 to have response of SD
  adrs_02                             <- adrs_01
  adrs_02$AVALC[adrs_02$USUBJID == "7"] <- "SD"
  
  actual_03 <- derive_param_bor(adrs_02,
                                dataset_adsl     = adsl,
                                filter_source    = PARAMCD == "OVR",
                                source_pd        = NULL,
                                source_datasets  = NULL,
                                reference_date   = TRTSDT,
                                ref_start_window = 28,
                                aval_fun         = aval_fun_pass,
                                set_values_to    = admiral::vars(PARAMCD = "BOR",
                                                                 PARAM   = "Best Overall Response"))
  
  # Expected Updated for Subject 7, response is now NE and 6 with a earlier ADT (first)
  expected_03 <- expected_02
  expected_03$AVALC[expected_03$USUBJID == "7"]    <- "SD"
  expected_03$ADT[expected_02$USUBJID == "7"][4]   <- c(lubridate::ymd("2020-01-01"))
  expected_03$AVALC[expected_02$USUBJID == "7"][4] <- "NE"
  expected_03$AVAL[expected_02$USUBJID == "7"][4]  <- 6
  
  admiral::expect_dfs_equal(base    = expected_03,
                            compare = actual_03,
                            keys    = c("USUBJID", "PARAMCD", "ADT"))
})

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Amgen Data and non package source TO DELETE ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

source("~/admiralonco_22_derive_param_bor/R/date_source.R")
source("~/admiralonco_22_derive_param_bor/R/filter_confirmation.R")
source("~/admiralonco_22_derive_param_bor/R/derive_param_bor.R")

adsl_amgen <- haven::read_sas("/userdata/stat/amg160/onc/20180101/analysis/final/statdata/adam/adsl.sas7bdat")

adrs_amgen <- haven::read_sas("/userdata/stat/amg160/onc/20180101/analysis/final/statdata/adam/adrs.sas7bdat") %>%
  dplyr::select(USUBJID, STUDYID, TRTSDT, PARAM, PARAMCD, 
                ADT, ASEQ, AVAL, AVALC, dplyr::starts_with("ANL"))

pd_date <-  admiral::date_source(
  dataset_name = "adrs_amgen",
  date         = ADT,
  filter       = PARAMCD == "CLINRESP" & AVALC == "PD" # check with Catherine
)
