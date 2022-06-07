
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Amgen Data TO DELETE ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

source("~/admiralonco_22_derive_param_bor/R/date_source.R")
source("~/admiralonco_22_derive_param_bor/R/filter_confirmation.R")
source("~/admiralonco_22_derive_param_bor/R/derive_param_bor.R")

adsl_amgen <- haven::read_sas("/userdata/stat/amg160/onc/20180101/analysis/final/statdata/adam/adsl.sas7bdat")

adrs_amgen <- haven::read_sas("/userdata/stat/amg160/onc/20180101/analysis/final/statdata/adam/adrs.sas7bdat") %>%
  dplyr::select(USUBJID, PARAM, PARAMCD, ADT, ASEQ, AVAL, AVALC, dplyr::starts_with("ANL"))

test_pd <-  date_source(
  dataset_name = "adrs_amgen",
  date         = ADT,
  filter       = PARAMCD == "CLINRESP" & AVALC == "PD" # check with Catherine
)

LSTAC <- derive_param_lasta(
  dataset      = adrs_amgen,
  order           = admiral::vars(USUBJID, ADT, ASEQ),
  filter_source = PARAMCD == "OVRLRESP", # & ANL01FL == "Y",
  source_pd = test_pd,
  source_datasets = list(adrs_amgen = adrs_amgen),
  set_values_to = vars(
    PARAMCD = "LSTAC",
    PARAM = " Last Disease Assessment Censored at First PD by Investigator",
    PARCAT1 = "Tumor Response",
    PARCAT2 = "Investigator",
    PARCAT3 = "Recist 1.1",
    ANL01FL = "Y")
)

View(LSTAC %>% dplyr::filter(PARAMCD == "LSTAC"))


LSTA <- derive_param_lasta(
  dataset      = adrs_amgen,
  order           = admiral::vars(USUBJID, ADT, ASEQ),
  filter_source = PARAMCD == "OVRLRESP", # & ANL01FL == "Y",
  source_pd = NULL,
  source_datasets = NULL,
  set_values_to = vars(
    PARAMCD = "LSTA",
    PARAM = " Last Disease Assessment by Investigator",
    PARCAT1 = "Tumor Response",
    PARCAT2 = "Investigator",
    PARCAT3 = "Recist 1.1",
    ANL01FL = "Y")
)

View(LSTA %>% dplyr::filter(PARAMCD == "LSTA"))


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Create Test Data ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(magrittr)
library(testthat)

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
    "2",      "2020-03-01", "Y",
    "4",      "2020-02-01", "Y"
  ) %>%
    dplyr::mutate(PARAMCD = "PD")) %>%
  dplyr::mutate(
    ADT = lubridate::ymd(ADTC),
    STUDYID = "XX1234"
  ) %>%
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

# derive_param_confirmed_bor ----
## derive_param_confirmed_bor Test 1: default confirmed BOR ----
test_that("derive_param_confirmed_bor Test 1: default confirmed BOR", {
  actual <-
    derive_param_confirmed_bor(
      adrs,
      dataset_adsl = adsl,
      filter_source = PARAMCD == "OVR",
      source_pd = pd_date,
      source_datasets = list(adrs = adrs),
      reference_date = TRTSDT,
      ref_start_window = 28,
      ref_confirm = 28,
      set_values_to = vars(
        PARAMCD = "CBOR",
        PARAM = "Best Confirmed Overall Response by Investigator"
      )
    )
  
  expected <- dplyr::bind_rows(
    adrs,
    tibble::tribble(
      ~USUBJID, ~ADTC,        ~AVALC,          ~AVAL,
      "1",      "2020-02-01", "CR",            1,
      "2",      "2020-02-01", "SD",            3,
      "3",      "2020-01-01", "SD",            3,
      "4",      "2020-03-01", "SD",            3,
      "5",      "2020-05-15", "NON-CR/NON-PD", 4,
      "6",      "2020-03-30", "SD",            3,
      "7",      "2020-02-06", "NE",            6,
      "8",      "",           "MISSING",       7
    ) %>%
      dplyr::mutate(
        ADT = lubridate::ymd(ADTC),
        STUDYID = "XX1234",
        PARAMCD = "CBOR",
        PARAM = "Best Confirmed Overall Response by Investigator"
      ) %>%
      select(-ADTC)
  )
  
  expect_dfs_equal(
    base = expected,
    compare = actual,
    keys = c("USUBJID", "PARAMCD", "ADT")
  )
})

## derive_param_confirmed_bor Test 2: accept SD, ND handling, missing as NE ----
test_that("derive_param_confirmed_bor Test 2: accept SD, ND handling, missing as NE", {
  adrs_ext <- dplyr::bind_rows(
    filter(adrs, USUBJID != "7"),
    tibble::tribble(
      ~USUBJID, ~ADTC,        ~AVALC,
      "7",      "2020-04-02", "ND"
    ) %>%
      dplyr::mutate(
        PARAMCD = "OVR",
        ADT = lubridate::ymd(ADTC),
        STUDYID = "XX1234"
      ) %>%
      admiral::derive_vars_merged(
        dataset_add = adsl,
        by_vars = dplyr::vars(STUDYID, USUBJID),
        new_vars = dplyr::vars(TRTSDT)
      )
  )
  
  actual <-
    derive_param_confirmed_bor(
      adrs_ext,
      dataset_adsl = adsl,
      filter_source = PARAMCD == "OVR",
      source_pd = pd_date,
      source_datasets = list(adrs = adrs),
      reference_date = TRTSDT,
      ref_start_window = 28,
      ref_confirm = 14,
      max_nr_ne = 0,
      accept_sd = TRUE,
      missing_as_ne = TRUE,
      set_values_to = vars(
        PARAMCD = "CBOR",
        PARAM = "Best Confirmed Overall Response by Investigator"
      )
    )
  
  expected <- dplyr::bind_rows(
    adrs_ext,
    tibble::tribble(
      ~USUBJID, ~ADTC,        ~AVALC,          ~AVAL,
      "1",      "2020-01-01", "PR",            2,
      "2",      "2020-02-01", "PR",            2,
      "3",      "2019-11-12", "CR",            1,
      "4",      "2020-03-01", "SD",            3,
      "5",      "2020-01-01", "PR",            2,
      "6",      "2020-03-30", "SD",            3,
      "7",      "2020-04-02", "ND",            NA,
      "8",      "",           "NE",            6
    ) %>%
      dplyr::mutate(
        ADT = lubridate::ymd(ADTC),
        STUDYID = "XX1234",
        PARAMCD = "CBOR",
        PARAM = "Best Confirmed Overall Response by Investigator"
      ) %>%
      select(-ADTC)
  )
  
  expect_dfs_equal(
    base = expected,
    compare = actual,
    keys = c("USUBJID", "PARAMCD", "ADT")
  )
})