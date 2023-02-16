########################################################################
#  Description: Unit Testing Exposed Function derive_param_response    #
########################################################################

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Load required packages ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(tibble)
library(lubridate)
library(dplyr)
library(admiraldev)
library(admiral)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Create Test Data ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

adsl <- tribble(
  ~USUBJID, ~TRTSDTC,      ~CHECKKEPTCOL, ~CHECKNOTKEPTCOL,
  "1",      "2020-01-01",  "001",         "991",
  "2",      "2019-12-12",  "002",         "992",
  "3",      "2019-11-11",  "003",         "993",
  "4",      "2019-12-30",  "004",         "994",
) %>%
  mutate(
    TRTSDT = ymd(TRTSDTC),
    STUDYID = "XX1234"
  )

adrs <- tribble(
  ~USUBJID, ~ADTC, ~PARAMCD, ~AVALC, ~CHECKKEPTCOL,
  "1", "2020-01-02", "OVR", "PR", "001",
  "1", "2020-02-01", "OVR", "CR", "001",
  "1", "2020-03-01", "OVR", "CR", "001",
  "1", "2020-04-01", "OVR", "SD", "001",
  "1", NA_character_, "PD", "N", "001",
  "2", "2021-06-15", "OVR", "SD", "002",
  "2", "2021-07-16", "OVR", "PD", "002",
  "2", "2021-09-14", "OVR", "PD", "002",
  "2", "2021-09-14", "PD", "Y", "002",
  "3", "2021-09-14", "OVR", "SD", "003",
  "3", "2021-10-30", "OVR", "PD", "003",
  "3", "2021-12-25", "OVR", "CR", "003",
  "3", "2021-10-30", "PD", "Y", "003"
) %>%
  mutate(
    STUDYID = "XX1234",
    ADT = ymd(ADTC),
  ) %>%
  select(-ADTC)

# derive_param_response, Test 1 ----
test_that("Test 1: Test that response is derived accurately, with source_pd", {
  expected_output <- bind_rows(
    adrs,
    tribble(
      ~USUBJID, ~ADTC,        ~AVALC, ~AVAL, ~CHECKKEPTCOL,
      "1",      "2020-01-02", "Y",    1,     "001",
      "2",      "",           "N",    0,     "002",
      "3",      "",           "N",    0,     "003",
      "4",      "",           "N",    0,     "004",
    ) %>%
      mutate(
        ADT = ymd(ADTC),
        STUDYID = "XX1234",
        PARAMCD = "RSP",
        PARAM = "Response by investigator"
      ) %>%
      select(-ADTC)
  )

  # Define the end of the assessment period for responses as the first PD date.
  pd <- date_source(
    dataset_name = "adrs",
    date = ADT,
    filter = PARAMCD == "PD" & AVALC == "Y"
  )

  # Derive the response parameter
  actual_output <- adrs %>%
    derive_param_response(
      dataset_adsl = adsl,
      filter_source = PARAMCD == "OVR" & AVALC %in% c("CR", "PR"),
      source_pd = pd,
      source_datasets = list(adrs = adrs),
      set_values_to = exprs(
        PARAMCD = "RSP",
        PARAM = "Response by investigator"
      ),
      subject_keys = get_admiral_option("subject_keys")
    )

  expect_dfs_equal(
    actual_output,
    expected_output,
    keys = c("USUBJID", "PARAMCD", "ADT")
  )
})

# derive_param_response, Test 2 ----
test_that("Test 2: Test that response is derived accurately, with No source_pd", {
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # source_pd = NULL, so the response on 2021-12-25 for subjid 3 is selected
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  expected_output_no_source_pd <- bind_rows(
    adrs,
    tribble(
      ~USUBJID, ~ADTC,        ~AVALC, ~AVAL, ~CHECKKEPTCOL,
      "1",      "2020-01-02", "Y",    1,     "001",
      "2",      "",           "N",    0,     "002",
      "3",      "2021-12-25", "Y",    1,     "003",
      "4",      "",           "N",    0,     "004",
    ) %>%
      mutate(
        ADT = ymd(ADTC),
        STUDYID = "XX1234",
        PARAMCD = "RSP",
        PARAM = "Response by investigator"
      ) %>%
      select(-ADTC)
  )

  # Derive the response parameter
  actual_output_no_source_pd <- adrs %>%
    derive_param_response(
      dataset_adsl = adsl,
      filter_source = PARAMCD == "OVR" & AVALC %in% c("CR", "PR"),
      source_pd = NULL,
      source_datasets = NULL,
      set_values_to = exprs(
        PARAMCD = "RSP",
        PARAM = "Response by investigator"
      ),
      subject_keys = get_admiral_option("subject_keys")
    )

  expect_dfs_equal(
    actual_output_no_source_pd,
    expected_output_no_source_pd,
    keys = c("USUBJID", "PARAMCD", "ADT")
  )
})
