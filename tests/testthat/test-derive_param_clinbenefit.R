library(lubridate)
adsl <- tibble::tribble(
  ~USUBJID, ~TRTSDT,           ~EOSDT,
  "01",     ymd("2020-12-06"), ymd("2022-03-06"),
  "02",     ymd("2021-01-16"), ymd("2022-02-03"),
  "03",     ymd("2021-01-09"), ymd("2021-02-24"),
  "04",     ymd("2021-04-21"), ymd("2021-09-15")
) %>%
  mutate(STUDYID = "AB42")

adrs <- tibble::tribble(
  ~USUBJID, ~PARAMCD, ~AVALC, ~ADT,
  "01", "RSP", "Y", ymd("2021-04-08"),
  "02", "RSP", "N", ymd("2021-05-07"),
  "03", "RSP", "N", NA,
  "04", "RSP", "N", NA,
  "01", "PD", "N", NA,
  "02", "PD", "Y", ymd("2021-05-07"),
  "03", "PD", "N", NA,
  "04", "PD", "N", NA,
  "01", "OVR", "SD", ymd("2021-03-07"),
  "01", "OVR", "PR", ymd("2021-04-08"),
  "02", "OVR", "SD", ymd("2021-03-07"),
  "02", "OVR", NA, ymd("2021-04-07"),
  "02", "OVR", "PD", ymd("2021-05-07"),
  "03", "OVR", "SD", ymd("2021-01-30"),
  "04", "OVR", "NE", ymd("2021-05-21"),
  "04", "OVR", "NA", ymd("2021-06-30"),
  "04", "OVR", "NE", ymd("2021-07-24"),
  "04", "OVR", "ND", ymd("2021-09-30"),
) %>%
  mutate(STUDYID = "AB42")

pd <-  date_source(
  dataset_name = "adrs",
  date = ADT,
  filter = PARAMCD == "PD" & AVALC == "Y"
)

resp <- date_source(
  dataset_name = "adrs",
  date = ADT,
  filter = PARAMCD == "RSP" & AVALC == "Y"
)

test_that("Clinical benefit rate parameter is derived correctly", {

  input_cbr <-  tibble::tribble(
    ~USUBJID, ~PARAMCD, ~AVALC, ~ADT,
    "01", "CBR", "Y", ymd("2021-04-08"),
    "02", "CBR", "Y", ymd("2021-03-07"),
    "03", "CBR", "N", NA,
    "04", "CBR", "N", NA)  %>%
    mutate(STUDYID = "AB42")

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
                   keys = c("USUBJID", "PARAMCD", "ADT"))

})
