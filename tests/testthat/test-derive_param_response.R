test_that("Test that response is derived properly", {
  library(tibble)
  library(lubridate)
  library(dplyr)
  library(rlang)

  adsl <- tribble(
    ~USUBJID,
    "1",
    "2",
    "3",
    "4"
  ) %>%
    mutate(STUDYID = "XX1234")

  adrs1 <- tribble(
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


  rsp <- adrs1 %>%
    filter(PARAMCD == "OVR" & AVALC %in% c("CR", "PR")) %>%
    left_join(adrs1 %>%
      filter(PARAMCD == "PD") %>%
      select(STUDYID, USUBJID, PDDT = ADT),
    by = c("STUDYID", "USUBJID")
    ) %>%
    filter(ADT <= PDDT | is.na(PDDT)) %>%
    arrange(USUBJID, PARAMCD, ADT) %>%
    group_by(USUBJID, PARAMCD) %>%
    filter(row_number() == 1) %>%
    mutate(avalc = 1)

  new_obs <- adsl %>%
    left_join(rsp, by = c("STUDYID", "USUBJID")) %>%
    mutate(
      AVALC = if_else(avalc == 1, "Y", "N", "N"),
      AVAL = if_else(avalc == 1, 1, 0, 0),
      PARAMCD = "RSP", PARAM = "Response by investigator"
    ) %>%
    select(-avalc, -PDDT)

  expected_output <- bind_rows(adrs1, new_obs)

  # Define the end of the assessment period for responses as the first PD date.
  pd <- date_source(
    dataset_name = "adrs1",
    date = ADT,
    filter = PARAMCD == "PD" & AVALC == "Y"
  )
  # Derive the response parameter
  actual_output <- adrs1 %>%
    derive_param_response(
      dataset_adsl = adsl,
      filter_source = PARAMCD == "OVR" & AVALC %in% c("CR", "PR"),
      source_pd = pd,
      source_datasets = list(adrs1 = adrs1),
      set_values_to = vars(
        PARAMCD = "RSP",
        PARAM = "Response by investigator"
      ),
      subject_keys = vars(STUDYID, USUBJID)
    ) %>%
    arrange(USUBJID, PARAMCD, ADT)


  expect_dfs_equal(
    actual_output,
    expected_output,
    keys = c("USUBJID", "PARAMCD", "ADT")
  )
})
