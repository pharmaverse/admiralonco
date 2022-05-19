
adsl <- tibble::tribble(
  ~USUBJID, ~TRTSDTC,
  "1",      "2020-01-01",
  "2",      "2019-12-12",
  "3",      "2019-11-11",
  "4",      "2019-12-30",
  "5",      "2019-12-06"
) %>%
  mutate(TRTSDT = lubridate::ymd(TRTSDTC),
         STUDYID = "XX1234")

adrs <- tibble::tribble(
  ~USUBJID, ~ADTC,        ~AVALC,
  "1",      "2020-01-01", "PR",
  "1",      "2020-02-01", "CR",
  "1",      "2020-03-01", "CR",
  "1",      "2020-04-01", "SD",
  "1",      "2020-05-01", "NE",
  "2",      "2020-01-01", "SD",
  "2",      "2020-02-01", "PR",
  "2",      "2020-03-01", "PD",
  "3",      "2020-01-01", "SD",
  "4",      "2020-01-01", "PR",
  "4",      "2020-02-01", "PD",
  "4",      "2020-03-01", "SD",
  "4",      "2020-04-01", "SD",
  "4",      "2020-05-01", "PR"
) %>%
  mutate(PARAMCD = "OVR") %>%
  bind_rows(tibble::tribble(
    ~USUBJID, ~ADTC,        ~AVALC,
    "2",      "2020-03-01", "Y",
    "4",      "2020-02-01", "Y"
  ) %>%
    mutate(PARAMCD = "PD")
  ) %>%
  mutate(ADT = lubridate::ymd(ADTC),
         STUDYID = "XX1234") %>%
  admiral::derive_vars_merged(
    dataset_add = adsl,
    by_vars = dplyr::vars(STUDYID, USUBJID),
    new_vars = dplyr::vars(TRTSDT)
  )

pd_date <- admiral::date_source(dataset_name = "adrs",
                                date = ADT,
                                filter = PARAMCD == "PD")

# derive_param_confirmed_bor ----
## derive_param_confirmed_bor Test 1: filter without first_cond
test_that("filter_confirmation Test 1: filter without first_cond", {
  actual <-
    derive_param_confirmed_bor(
      adrs,
      dataset_adsl = adsl,
      filter_source = PARAMCD == "OVR",
      source_pd = pd_date,
      source_datasets = list(adrs = adrs),
      reference_date = TRTSDT,
      ref_start_window = 28,
      ref_confirm = 14,
      set_values_to = vars(
        PARAMCD = "CBOR",
        PARAM = "Best Confirmed Overall Response by Investigator"
      )
    )

  expected <- tibble::tribble(
    ~USUBJID, ~AVISITN, ~AVALC,
    "1",      1,        "PR",
    "4",      1,        "PR"
  )

  expect_dfs_equal(
    base = expected,
    compare = actual,
    keys = c("USUBJID", "AVISITN"))
})

## filter_confirmation Test 1:
test_that("filter_confirmation Test 1: filter with first_cond", {
  actual <-
    filter_confirmation(
      data,
      by_vars = vars(USUBJID),
      join_vars = vars(AVALC),
      first_cond = AVALC == "CR" &
        AVALC.join == "CR",
      order = vars(AVISITN),
      filter = TRUE
    )

  expected <- tibble::tribble(
    ~USUBJID, ~AVISITN, ~AVALC,
    "1",      2,        "CR"
  )

  expect_dfs_equal(base = expected,
                   compare = actual,
                   keys = c("USUBJID", "AVISITN"))
})

## filter_confirmation Test 3: filter with first_cond and summary function
test_that("filter_confirmation Test 3: filter with first_cond and summary function", {
  actual <-
    filter_confirmation(
      data,
      by_vars = vars(USUBJID),
      join_vars = vars(AVALC),
      first_cond = AVALC == "PR" &
        AVALC.join %in% c("CR", "PR"),
      order = vars(AVISITN),
      filter = count_vals(AVALC.join, "SD") <= 1
    )

  expected <- tibble::tribble(
    ~USUBJID, ~AVISITN, ~AVALC,
    "1",      1,        "PR"
  )

  expect_dfs_equal(base = expected,
                   compare = actual,
                   keys = c("USUBJID", "AVISITN"))
})

data <- tibble::tribble(
  ~USUBJID, ~AVISITN, ~AVALC,
  "1",      1,        "Y",
  "1",      2,        "N",
  "1",      3,        "Y",
  "1",      4,        "N",
  "2",      1,        "Y",
  "2",      2,        "N",
  "3",      1,        "Y",
  "4",      1,        "N",
  "4",      2,        "N",
)

filter_confirmation(
  data,
  by_vars = vars(USUBJID),
  join_vars = vars(AVALC, AVISITN),
  order = vars(AVISITN),
  filter = AVALC == "Y" & AVALC.join == "Y" & AVISIT < AVISIT.join
)

data <- tibble::tribble(
  ~USUBJID, ~AVISITN, ~AVALC,
  "1",      1,        "PR",
  "1",      2,        "CR",
  "1",      3,        "NE",
  "1",      4,        "CR",
  "1",      5,        "NE",
  "2",      1,        "CR",
  "2",      2,        "PR",
  "2",      3,        "CR",
  "3",      1,        "CR",
  "4",      1,        "CR",
  "4",      2,        "NE",
  "4",      3,        "NE",
  "4",      4,        "CR",
  "4",      5,        "PR"
)

filter_confirmation(
  data,
  by_vars = vars(USUBJID),
  join_vars = vars(AVALC),
  order = vars(AVISITN),
  first_cond = AVALC.join == "CR",
  filter = AVALC == "CR" & all(AVALC.join %in% c("CR", "NE")) & count_vals(var = AVALC.join, val = "NE") <=1
)
