
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
    ADT = lubridate::ymd(ADTC),
    STUDYID = "XX1234"
  ) %>%
  derive_vars_merged(
    dataset_add = adsl,
    by_vars = exprs(STUDYID, USUBJID),
    new_vars = exprs(TRTSDT)
  )

pd_date <- admiral::date_source(
  dataset_name = "adrs",
  date = ADT,
  filter = PARAMCD == "PD"
)

# derive_param_confirmed_bor ----
## Test 1: default confirmed BOR ----
test_that("derive_param_confirmed_bor Test 1: default confirmed BOR", {
  suppress_warning(
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
        set_values_to = exprs(
          AVAL = aval_resp(AVALC),
          PARAMCD = "CBOR",
          PARAM = "Best Confirmed Overall Response by Investigator"
        )
      ),
    "Dataset contains CR records followed by PR"
  )

  expected <- bind_rows(
    adrs,
    tribble(
      ~USUBJID, ~ADTC,         ~AVALC,          ~AVAL,
      "1",      "2020-02-01",  "CR",            1,
      "2",      "2020-02-01",  "SD",            3,
      "3",      "2020-01-01",  "SD",            3,
      "4",      "2020-03-01",  "SD",            3,
      "5",      "2020-05-15",  "NON-CR/NON-PD", 4,
      "6",      "2020-03-30",  "SD",            3,
      "7",      "2020-02-06",  "NE",            6,
      "8",      NA_character_, "MISSING",       7,
      "9",      "2020-02-16",  "PD",            5
    ) %>%
      mutate(
        ADT = lubridate::ymd(ADTC),
        STUDYID = "XX1234",
        PARAMCD = "CBOR",
        PARAM = "Best Confirmed Overall Response by Investigator"
      ) %>%
      derive_vars_merged(
        dataset_add = adsl,
        by_vars = exprs(STUDYID, USUBJID),
        new_vars = exprs(TRTSDT)
      )
  )

  expect_dfs_equal(
    base = expected,
    compare = actual,
    keys = c("USUBJID", "PARAMCD", "ADT")
  )
})

## Test 2: accept SD, ND handling, missing as NE ----
test_that("derive_param_confirmed_bor Test 2: accept SD, ND handling, missing as NE", {
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
      ) %>%
      derive_vars_merged(
        dataset_add = adsl,
        by_vars = exprs(STUDYID, USUBJID),
        new_vars = exprs(TRTSDT)
      )
  )

  suppress_warning(
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
        set_values_to = exprs(
          AVAL = aval_resp(AVALC),
          PARAMCD = "CBOR",
          PARAM = "Best Confirmed Overall Response by Investigator"
        )
      ),
    "Dataset contains CR records followed by PR"
  )

  expected <- bind_rows(
    adrs_ext,
    tibble::tribble(
      ~USUBJID, ~ADTC,         ~AVALC, ~AVAL,
      "1",      "2020-01-01",  "PR",   2,
      "2",      "2020-02-01",  "PR",   2,
      "3",      "2019-11-12",  "CR",   1,
      "4",      "2020-03-01",  "SD",   3,
      "5",      "2020-01-01",  "PR",   2,
      "6",      "2020-03-30",  "SD",   3,
      "7",      "2020-04-02",  "ND",   NA,
      "8",      NA_character_, "NE",   6,
      "9",      "2020-02-16",  "PD",   5
    ) %>%
      mutate(
        ADT = lubridate::ymd(ADTC),
        STUDYID = "XX1234",
        PARAMCD = "CBOR",
        PARAM = "Best Confirmed Overall Response by Investigator"
      ) %>%
      derive_vars_merged(
        dataset_add = adsl,
        by_vars = exprs(STUDYID, USUBJID),
        new_vars = exprs(TRTSDT)
      )
  )

  expect_dfs_equal(
    base = expected,
    compare = actual,
    keys = c("USUBJID", "PARAMCD", "ADT")
  )
})

## Test 3: error if invalid response values ----
test_that("derive_param_confirmed_bor Test 3: error if invalid response values", {
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
    ) %>%
    derive_vars_merged(
      dataset_add = adsl,
      by_vars = exprs(STUDYID, USUBJID),
      new_vars = exprs(TRTSDT)
    )

  expect_error(
    derive_param_confirmed_bor(
      adrs,
      dataset_adsl = adsl,
      filter_source = PARAMCD == "OVR",
      source_pd = pd_date,
      source_datasets = list(adrs = adrs),
      reference_date = TRTSDT,
      ref_start_window = 28,
      ref_confirm = 28,
      set_values_to = exprs(
        PARAMCD = "CBOR",
        PARAM = "Best Confirmed Overall Response by Investigator"
      )
    ),
    regexp = "The following invalid values were found:\n`iCR`"
  )
})

## Test 4: No source_pd ----
test_that("derive_param_confirmed_bor Test 4: No source_pd", {
  suppress_warning(
    actual_no_source_pd <-
      derive_param_confirmed_bor(
        adrs,
        dataset_adsl = adsl,
        filter_source = PARAMCD == "OVR",
        source_pd = NULL,
        source_datasets = NULL,
        reference_date = TRTSDT,
        ref_start_window = 28,
        ref_confirm = 28,
        set_values_to = exprs(
          AVAL = aval_resp(AVALC),
          PARAMCD = "CBOR",
          PARAM = "Best Confirmed Overall Response by Investigator"
        )
      ),
    "Dataset contains CR records followed by PR"
  )

  expected_no_source_pd <- bind_rows(
    adrs,
    tibble::tribble(
      ~USUBJID, ~ADTC,         ~AVALC,          ~AVAL,
      "1",      "2020-02-01",  "CR",            1,
      "2",      "2020-02-01",  "SD",            3,
      "3",      "2020-01-01",  "SD",            3,
      "4",      "2020-03-01",  "SD",            3,
      "5",      "2020-05-15",  "NON-CR/NON-PD", 4,
      "6",      "2020-03-30",  "SD",            3,
      "7",      "2020-02-06",  "NE",            6,
      "8",      NA_character_, "MISSING",       7,
      "9",      "2020-03-06",  "SD",            3 # expected is now SD
    ) %>%
      mutate(
        ADT = ymd(ADTC),
        STUDYID = "XX1234",
        PARAMCD = "CBOR",
        PARAM = "Best Confirmed Overall Response by Investigator"
      ) %>%
      derive_vars_merged(
        dataset_add = adsl,
        by_vars = exprs(STUDYID, USUBJID),
        new_vars = exprs(TRTSDT)
      )
  )

  expect_dfs_equal(
    base = expected_no_source_pd,
    compare = actual_no_source_pd,
    keys = c("USUBJID", "PARAMCD", "ADT")
  )
})

## Test 5: Deprecation warning for aval_fun ----
test_that("derive_param_confirmed_bor Test 5: Deprecation warning for aval_fun", {
  expect_warning(
    suppress_warning(
    actual_no_source_pd <-
      derive_param_confirmed_bor(
        adrs,
        dataset_adsl = adsl,
        filter_source = PARAMCD == "OVR",
        source_pd = NULL,
        source_datasets = NULL,
        reference_date = TRTSDT,
        ref_start_window = 28,
        ref_confirm = 28,
        aval_fun = aval_resp,
        set_values_to = exprs(
          PARAMCD = "CBOR",
          PARAM = "Best Confirmed Overall Response by Investigator"
        )
      ),
    "Dataset contains CR records followed by PR"
  ),
  class = "lifecycle_warning_deprecated"
  )

  expected_no_source_pd <- bind_rows(
    adrs,
    tibble::tribble(
      ~USUBJID, ~ADTC,         ~AVALC,          ~AVAL,
      "1",      "2020-02-01",  "CR",            1,
      "2",      "2020-02-01",  "SD",            3,
      "3",      "2020-01-01",  "SD",            3,
      "4",      "2020-03-01",  "SD",            3,
      "5",      "2020-05-15",  "NON-CR/NON-PD", 4,
      "6",      "2020-03-30",  "SD",            3,
      "7",      "2020-02-06",  "NE",            6,
      "8",      NA_character_, "MISSING",       7,
      "9",      "2020-03-06",  "SD",            3 # expected is now SD
    ) %>%
      mutate(
        ADT = ymd(ADTC),
        STUDYID = "XX1234",
        PARAMCD = "CBOR",
        PARAM = "Best Confirmed Overall Response by Investigator"
      ) %>%
      derive_vars_merged(
        dataset_add = adsl,
        by_vars = exprs(STUDYID, USUBJID),
        new_vars = exprs(TRTSDT)
      )
  )

  expect_dfs_equal(
    base = expected_no_source_pd,
    compare = actual_no_source_pd,
    keys = c("USUBJID", "PARAMCD", "ADT")
  )
})
