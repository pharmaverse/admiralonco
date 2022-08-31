library(tibble)
library(dplyr)
library(lubridate)
library(admiraldev)
library(admiral)

adsl <- tribble(
  ~USUBJID, ~TRTSDT,      ~EOSDT,
  "01",     "2020-12-06", "2022-03-06",
  "02",     "2021-01-16", "2022-02-03",
  "03",     "2021-01-09", "2021-02-24",
  "04",     "2021-04-21", "2021-09-15",
  "05",     "2021-06-10", "2021-10-31",
  "06",     "2021-07-04", "2021-09-01"
) %>%
  mutate(
    STUDYID = "AB42",
    TRTSDT = as_date(TRTSDT),
    EOSDT = as_date(EOSDT),
  )

adrs <- tribble(
  ~USUBJID, ~PARAMCD, ~AVALC, ~ADT,
  "01",     "RSP",    "Y",    "2021-04-08",
  "02",     "RSP",    "N",    "2021-05-07",
  "03",     "RSP",    "N",    NA,
  "04",     "RSP",    "N",    NA,
  "06",     "RSP",    "N",    NA,
  "01",     "PD",     "N",    NA,
  "02",     "PD",     "Y",    "2021-05-07",
  "03",     "PD",     "N",    NA,
  "04",     "PD",     "N",    NA,
  "06",     "PD",     "Y",    "2021-08-20",
  "01",     "OVR",    "SD",   "2021-03-07",
  "01",     "OVR",    "PR",   "2021-04-08",
  "02",     "OVR",    "SD",   "2021-03-07",
  "02",     "OVR",    NA,     "2021-04-07",
  "02",     "OVR",    "PD",   "2021-05-07",
  "03",     "OVR",    "SD",   "2021-01-30",
  "04",     "OVR",    "NE",   "2021-05-21",
  "04",     "OVR",    "NA",   "2021-06-30",
  "04",     "OVR",    "NE",   "2021-07-24",
  "04",     "OVR",    "ND",   "2021-09-30",
  "06",     "OVR",    "PD",   "2021-08-20"
) %>%
  mutate(
    STUDYID = "AB42",
    ADT = as_date(ADT),
    ANL01FL = "Y"
  ) %>%
  derive_vars_merged(
    dataset_add = adsl,
    by_vars = vars(STUDYID, USUBJID),
    new_vars = vars(TRTSDT)
  )

pd <- date_source(
  dataset_name = "adrs",
  date = ADT,
  filter = PARAMCD == "PD" & AVALC == "Y" & ANL01FL == "Y"
)

resp <- date_source(
  dataset_name = "adrs",
  date = ADT,
  filter = PARAMCD == "RSP" & AVALC == "Y" & ANL01FL == "Y"
)

test_that("Clinical benefit rate parameter is derived correctly", {
  input_cbr <- tribble(
    ~USUBJID, ~PARAMCD, ~AVALC, ~AVAL, ~ADT,
    "01",     "CBR",    "Y",    1,     "2021-03-07",
    "02",     "CBR",    "Y",    1,     "2021-03-07",
    "03",     "CBR",    "N",    0,     NA,
    "04",     "CBR",    "N",    0,     NA,
    "05",     "CBR",    "N",    0,     NA,
    "06",     "CBR",    "N",    0,     NA
  ) %>%
    mutate(
      STUDYID = "AB42",
      ADT = as_date(ADT),
      ANL01FL = "Y"
    ) %>%
    left_join(adsl, by = c("STUDYID", "USUBJID")) %>%
    select(-EOSDT)

  expected_output <- bind_rows(adrs, input_cbr)

  actual_output <- derive_param_clinbenefit(
    dataset = adrs,
    dataset_adsl = adsl,
    filter_source = PARAMCD == "OVR",
    source_resp = resp,
    source_pd = pd,
    source_datasets = list(adrs = adrs),
    reference_date = TRTSDT,
    ref_start_window = 28,
    set_values_to = vars(
      PARAMCD = "CBR",
      ANL01FL = "Y"
    )
  )

  expect_dfs_equal(actual_output, expected_output,
    keys = c("USUBJID", "PARAMCD", "ADT")
  )
})
