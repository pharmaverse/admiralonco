library(tibble)
library(dplyr)
library(lubridate)
library(admiraldev)
library(admiral)

adsl <- tribble(
  ~USUBJID, ~TRTSDTC,
  "1",      "2020-01-01",
  "2",      "2019-12-12",
  "3",      "2019-11-11",
  "4",      "2019-12-30",
  "5",      "2020-01-01",
  "6",      "2020-02-02",
  "7",      "2020-02-02",
  "8",      "2020-04-01",
  "9",      "2020-02-01"
) %>%
  mutate(
    STUDYID = "XX1234"
  )

adrs <- tribble(
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
  "7",      "2020-04-01", "NE",
  "9",      "2020-02-16", "PD",
  "9",      "2020-03-06", "SD"
) %>%
  mutate(PARAMCD = "OVR") %>%
  bind_rows(tribble(
    ~USUBJID, ~ADTC,        ~AVALC,
    "9",      "2020-02-16", "Y"
  ) %>%
    mutate(PARAMCD = "PD")) %>%
  mutate(
    ADT = ymd(ADTC),
    STUDYID = "XX1234"
  )

pd_date <- date_source(
  dataset_name = "adrs",
  date = ADT,
  filter = PARAMCD == "PD"
)

# derive_param_confirmed_resp ----
## Test 1: default confirmed response ----
test_that("derive_param_confirmed_resp Test 1: default confirmed response", {
  suppress_warning(
    actual <-
      derive_param_confirmed_resp(
        adrs,
        dataset_adsl = adsl,
        filter_source = PARAMCD == "OVR",
        source_pd = pd_date,
        source_datasets = list(adrs = adrs),
        ref_confirm = 28,
        set_values_to = exprs(
          PARAMCD = "CRSP",
          PARAM = "Confirmed Response by Investigator"
        )
      ),
    "Dataset contains CR records followed by PR"
  )

  expected <- bind_rows(
    adrs,
    tribble(
      ~USUBJID, ~ADTC,         ~AVALC, ~AVAL,
      "1",      "2020-01-01",  "Y",    1,
      "2",      NA_character_, "N",    0,
      "3",      NA_character_, "N",    0,
      "4",      NA_character_, "N",    0,
      "5",      NA_character_, "N",    0,
      "6",      NA_character_, "N",    0,
      "7",      NA_character_, "N",    0,
      "8",      NA_character_, "N",    0,
      "9",      NA_character_, "N",    0
    ) %>%
      mutate(
        ADT = ymd(ADTC),
        STUDYID = "XX1234",
        PARAMCD = "CRSP",
        PARAM = "Confirmed Response by Investigator"
      )
  )

  expect_dfs_equal(
    base = expected,
    compare = actual,
    keys = c("USUBJID", "PARAMCD", "ADT")
  )
})

## Test 2: accept SD ----
test_that("derive_param_confirmed_resp Test 2: accept SD", {
  adrs_ext <- bind_rows(
    filter(adrs, USUBJID != "7"),
    tribble(
      ~USUBJID, ~ADTC,        ~AVALC,
      "7",      "2020-04-02", "ND"
    ) %>%
      mutate(
        PARAMCD = "OVR",
        ADT = ymd(ADTC),
        STUDYID = "XX1234"
      )
  )

  suppress_warning(
    actual <-
      derive_param_confirmed_resp(
        adrs_ext,
        dataset_adsl = adsl,
        filter_source = PARAMCD == "OVR",
        source_pd = pd_date,
        source_datasets = list(adrs = adrs),
        ref_confirm = 14,
        max_nr_ne = 0,
        accept_sd = TRUE,
        set_values_to = exprs(
          PARAMCD = "CRSP",
          PARAM = "Confirmed Response by Investigator"
        )
      ),
    "Dataset contains CR records followed by PR"
  )

  expected <- bind_rows(
    adrs_ext,
    tribble(
      ~USUBJID, ~ADTC,         ~AVALC, ~AVAL,
      "1",      "2020-01-01",  "Y",    1,
      "2",      "2020-02-01",  "Y",    1,
      "3",      "2019-11-12",  "Y",    1,
      "4",      NA_character_, "N",    0,
      "5",      "2020-01-01",  "Y",    1,
      "6",      NA_character_, "N",    0,
      "7",      NA_character_, "N",    0,
      "8",      NA_character_, "N",    0,
      "9",      NA_character_, "N",    0
    ) %>%
      mutate(
        ADT = ymd(ADTC),
        STUDYID = "XX1234",
        PARAMCD = "CRSP",
        PARAM = "Confirmed Response by Investigator"
      )
  )

  expect_dfs_equal(
    base = expected,
    compare = actual,
    keys = c("USUBJID", "PARAMCD", "ADT")
  )
})

## Test 3: error if invalid response values ----
test_that("derive_param_confirmed_resp Test 3: error if invalid response values", {
  adrs <- tribble(
    ~USUBJID, ~ADTC,        ~AVALC,
    "1",      "2020-01-01", "PR",
    "1",      "2020-02-01", "CR",
    "1",      "2020-02-16", "NE",
    "1",      "2020-03-01", "iCR",
    "1",      "2020-04-01", "SD",
  ) %>%
    mutate(
      PARAMCD = "OVR",
      ADT = ymd(ADTC),
      STUDYID = "XX1234"
    )

  expect_error(
    derive_param_confirmed_resp(
      adrs,
      dataset_adsl = adsl,
      filter_source = PARAMCD == "OVR",
      source_pd = pd_date,
      source_datasets = list(adrs = adrs),
      ref_confirm = 28,
      set_values_to = exprs(
        PARAMCD = "CRSP",
        PARAM = "Confirmed Response by Investigator"
      )
    ),
    regexp = "The following invalid values were found:\n`iCR`"
  )
})

## Test 4: No source_pd ----
test_that("derive_param_confirmed_resp Test 4: No source_pd", {
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # source_pd = NULL, so tibble with two subjects:
  #    - USUBJID == 1 will be a response as responsed after PD.
  #    - USUBJID == 2 will not be a responses as it straddles a PD.
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  adrs <- tribble(
    ~USUBJID, ~ADTC,        ~AVALC,
    "1",      "2020-01-01", "SD",
    "1",      "2020-02-01", "PD",
    "1",      "2020-02-16", "PR",
    "1",      "2020-03-21", "CR",
    "2",      "2020-01-01", "SD",
    "2",      "2020-02-01", "CR",
    "2",      "2020-02-16", "PD",
    "2",      "2020-03-21", "CR",
  ) %>%
    mutate(
      PARAMCD = "OVR",
      ADT = ymd(ADTC),
      STUDYID = "XX1234"
    )

  actual_no_source_pd <-
    derive_param_confirmed_resp(
      adrs,
      dataset_adsl = adsl,
      filter_source = PARAMCD == "OVR",
      source_pd = NULL,
      source_datasets = NULL,
      ref_confirm = 28,
      set_values_to = exprs(
        PARAMCD = "CRSP",
        PARAM = "Confirmed Response by Investigator"
      )
    )

  expected_no_source_pd <- bind_rows(
    adrs,
    tribble(
      ~USUBJID, ~ADTC,         ~AVALC, ~AVAL,
      "1",      "2020-02-16",  "Y",    1,
      "2",      NA_character_, "N",    0,
      "3",      NA_character_, "N",    0,
      "4",      NA_character_, "N",    0,
      "5",      NA_character_, "N",    0,
      "6",      NA_character_, "N",    0,
      "7",      NA_character_, "N",    0,
      "8",      NA_character_, "N",    0,
      "9",      NA_character_, "N",    0
    ) %>%
      mutate(
        ADT = ymd(ADTC),
        STUDYID = "XX1234",
        PARAMCD = "CRSP",
        PARAM = "Confirmed Response by Investigator"
      )
  )

  expect_dfs_equal(
    base = expected_no_source_pd,
    compare = actual_no_source_pd,
    keys = c("USUBJID", "PARAMCD", "ADT")
  )
})
