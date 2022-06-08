
adsl <- tibble::tribble(
  ~USUBJID, ~TRTSDT, ~EOSDT,
  "01", "2020-12-06", "2022-03-06",
  "02", "2021-01-16", "2022-02-03",
  "03", "2021-01-09", "2021-02-24",
  "04", "2021-04-21", "2021-09-15"
) %>%
  mutate(STUDYID = "AB42",
         TRTSDT = lubridate::as_date(TRTSDT),
         EOSDT = lubridate::as_date(EOSDT),
         )

adrs <- tibble::tribble(
  ~USUBJID, ~PARAMCD, ~AVALC, ~ADT,
  "01", "RSP", "Y", "2021-04-08",
  "02", "RSP", "N", "2021-05-07",
  "03", "RSP", "N", NA,
  "04", "RSP", "N", NA,
  "01", "PD", "N", NA,
  "02", "PD", "Y", "2021-05-07",
  "03", "PD", "N", NA,
  "04", "PD", "N", NA,
  "01", "OVR", "SD", "2021-03-07",
  "01", "OVR", "PR", "2021-04-08",
  "02", "OVR", "SD", "2021-03-07",
  "02", "OVR", NA, "2021-04-07",
  "02", "OVR", "PD", "2021-05-07",
  "03", "OVR", "SD", "2021-01-30",
  "04", "OVR", "NE", "2021-05-21",
  "04", "OVR", "NA", "2021-06-30",
  "04", "OVR", "NE", "2021-07-24",
  "04", "OVR", "ND", "2021-09-30",
) %>%
  mutate(STUDYID = "AB42",
         ADT = lubridate::as_date(ADT))

pd <- admiral::date_source(
  dataset_name = "adrs",
  date = ADT,
  filter = PARAMCD == "PD" & AVALC == "Y"
)

resp <- admiral::date_source(
  dataset_name = "adrs",
  date = ADT,
  filter = PARAMCD == "RSP" & AVALC == "Y"
)

test_that("Clinical benefit rate parameter is derived correctly", {
  input_cbr <- tibble::tribble(
    ~USUBJID, ~PARAMCD, ~AVALC, ~ADT,
    "01", "CBR", "Y", "2021-04-08",
    "02", "CBR", "Y", "2021-03-07",
    "03", "CBR", "N", NA,
    "04", "CBR", "N", NA
  ) %>%
    mutate(STUDYID = "AB42",
           ADT = lubridate::as_date(ADT))

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
      PARAMCD = "CBR"
    )
  )

  expect_dfs_equal(actual_output, expected_output,
    keys = c("USUBJID", "PARAMCD", "ADT")
  )
})
