
adsl <- tibble::tribble(
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
  "7",      "2020-04-01", "NE",
  "9",      "2020-02-16", "PD",
  "9",      "2020-03-06", "SD"
) %>%
  mutate(PARAMCD = "OVR") %>%
  bind_rows(tibble::tribble(
    ~USUBJID, ~ADTC,        ~AVALC,
    "9",      "2020-02-16", "Y"
  ) %>%
    mutate(PARAMCD = "PD")) %>%
  mutate(
    ADT = lubridate::ymd(ADTC),
    STUDYID = "XX1234"
  )

pd_date <- admiral::date_source(
  dataset_name = "adrs",
  date = ADT,
  filter = PARAMCD == "PD"
)

# derive_param_confirmed_resp ----
## derive_param_confirmed_resp Test 1: default confirmed response ----
test_that("derive_param_confirmed_resp Test 1: default confirmed response", {
  actual <-
    derive_param_confirmed_resp(
      adrs,
      dataset_adsl = adsl,
      filter_source = PARAMCD == "OVR",
      source_pd = pd_date,
      source_datasets = list(adrs = adrs),
      ref_confirm = 28,
      set_values_to = vars(
        PARAMCD = "CRSP",
        PARAM = "Confirmed Response by Investigator"
      )
    )

  expected <- bind_rows(
    adrs,
    tibble::tribble(
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
        ADT = lubridate::ymd(ADTC),
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

## derive_param_confirmed_resp Test 2: accept SD ----
test_that("derive_param_confirmed_resp Test 2: accept SD", {
  adrs_ext <- bind_rows(
    filter(adrs, USUBJID != "7"),
    tibble::tribble(
      ~USUBJID, ~ADTC,        ~AVALC,
      "7",      "2020-04-02", "ND"
    ) %>%
      mutate(
        PARAMCD = "OVR",
        ADT = lubridate::ymd(ADTC),
        STUDYID = "XX1234"
      )
  )

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
      set_values_to = vars(
        PARAMCD = "CRSP",
        PARAM = "Confirmed Response by Investigator"
      )
    )

  expected <- bind_rows(
    adrs_ext,
    tibble::tribble(
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
        ADT = lubridate::ymd(ADTC),
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

## derive_param_confirmed_resp Test 3: error if invalid response values ----
test_that("derive_param_confirmed_resp Test 3: error if invalid response values", {
  adrs <- tibble::tribble(
    ~USUBJID, ~ADTC,        ~AVALC,
    "1",      "2020-01-01", "PR",
    "1",      "2020-02-01", "CR",
    "1",      "2020-02-16", "NE",
    "1",      "2020-03-01", "iCR",
    "1",      "2020-04-01", "SD",
  ) %>%
    mutate(
      PARAMCD = "OVR",
      ADT = lubridate::ymd(ADTC),
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
      set_values_to = vars(
        PARAMCD = "CRSP",
        PARAM = "Confirmed Response by Investigator"
      )
    ),
    regexp = "The following invalid values were found:\n`iCR`"
  )
})