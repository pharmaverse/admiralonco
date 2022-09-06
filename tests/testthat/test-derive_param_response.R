########################################################################
#  Description: Unit Testing Exposed Function derive_param_response    #
########################################################################

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Load required packages ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(tibble)
library(lubridate)
library(dplyr)
library(rlang)
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
  ~USUBJID, ~ADTC, ~PARAMCD, ~AVALC,
  "1", "2020-01-02", "OVR", "PR",
  "1", "2020-02-01", "OVR", "CR",
  "1", "2020-03-01", "OVR", "CR",
  "1", "2020-04-01", "OVR", "SD",
  "1", NA_character_, "PD", "N",
  "2", "2021-06-15", "OVR", "SD",
  "2", "2021-07-16", "OVR", "PD",
  "2", "2021-09-14", "OVR", "PD",
  "2", "2021-09-14", "PD", "Y",
  "3", "2021-09-14", "OVR", "SD",
  "3", "2021-10-30", "OVR", "PD",
  "3", "2021-12-25", "OVR", "CR",
  "3", "2021-10-30", "PD", "Y"
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
      ~USUBJID, ~ADTC,        ~AVALC, ~AVAL, ~TRTSDTC,     ~CHECKKEPTCOL,
      "1",      "2020-01-02", "Y",    1,     "2020-01-01", "001",
      "2",      "",           "N",    0,     "2019-12-12", "002",
      "3",      "",           "N",    0,     "2019-11-11", "003",
      "4",      "",           "N",    0,     "2019-12-30", "004",
    ) %>%
      mutate(
        ADT = ymd(ADTC),
        TRTSDT = ymd(TRTSDTC),
        STUDYID = "XX1234",
        PARAMCD = "RSP",
        PARAM = "Response by investigator"
      ) %>%
      select(-ADTC, -TRTSDTC)
  )

  # Dropping TRTSDT and CHECKKEPTCOL, but I believe the should be in the output dataframe
  # TBC: sgorm123
  expected_output <- expected_output %>%
    dplyr::select(-c(TRTSDT, CHECKKEPTCOL))

  # Define the end of the assessment period for responses as the first PD date.
  pd <- date_source(
    dataset_name = "adrs",
    date = ADT,
    filter = PARAMCD == "PD" & AVALC == "Y"
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
      ~USUBJID, ~ADTC,        ~AVALC, ~AVAL, ~TRTSDTC,     ~CHECKKEPTCOL,
      "1",      "2020-01-02", "Y",    1,     "2020-01-01", "001",
      "2",      "",           "N",    0,     "2019-12-12", "002",
      "3",      "2021-12-25", "Y",    0,     "2019-11-11", "003",
      "4",      "",           "N",    0,     "2019-12-30", "004",
    ) %>%
      mutate(
        ADT = ymd(ADTC),
        TRTSDT = ymd(TRTSDTC),
        STUDYID = "XX1234",
        PARAMCD = "RSP",
        PARAM = "Response by investigator"
      ) %>%
      select(-ADTC, -TRTSDTC)
  )

  # Dropping TRTSDT and CHECKKEPTCOL, but I believe the should be in the output dataframe
  # TBC: sgorm123
  expected_output_no_source_pd <- expected_output_no_source_pd %>%
    dplyr::select(-c(TRTSDT, CHECKKEPTCOL))

  # Derive the response parameter
  actual_output_no_source_pd <- adrs %>%
    derive_param_response(
      dataset_adsl = adsl,
      filter_source = PARAMCD == "OVR" & AVALC %in% c("CR", "PR"),
      source_pd = NULL,
      source_datasets = NULL,
      set_values_to = vars(
        PARAMCD = "RSP",
        PARAM = "Response by investigator"
      ),
      subject_keys = vars(STUDYID, USUBJID)
    )

  expect_dfs_equal(
    actual_output_no_source_pd,
    expected_output_no_source_pd,
    keys = c("USUBJID", "PARAMCD", "ADT")
  )
})
