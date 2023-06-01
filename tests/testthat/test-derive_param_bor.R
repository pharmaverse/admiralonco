######################################################################## !
#  Description: Unit Testing Exposed Function derive_param_bor         #
#                                                                      #
#  Requirements: https://github.com/pharmaverse/admiralonco/issues/22  #
######################################################################## !

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Create Test Data ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# To test:
# 1. We keep all variables from the input dataset.
# 2. For subjects without observations in the input dataset we
#    keep all variables from ADSL which are also in the input dataset.
# Two columns added:
# 1. CHECKKEPTCOL ensure is in final dataframe
# 2. CHECKNOTKEPTCOL ensure not in final dataframe
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

adsl <- tibble::tribble(
  ~USUBJID, ~TRTSDTC,      ~CHECKKEPTCOL, ~CHECKNOTKEPTCOL,
  "1",      "2020-01-01",  "001",         "991",
  "2",      "2019-12-12",  "002",         "992",
  "3",      "2019-11-11",  "003",         "993",
  "4",      "2019-12-30",  "004",         "994",
  "5",      "2020-01-01",  "005",         "995",
  "6",      "2020-02-02",  "006",         "996",
  "7",      "2020-02-02",  "007",         "997",
  "8",      "2020-04-01",  "008",         "999"
) %>%
  mutate(
    TRTSDT = lubridate::ymd(TRTSDTC),
    STUDYID = "XX1234"
  )

adrs <- tibble::tribble(
  ~USUBJID, ~ADTC, ~AVALC, ~CHECKKEPTCOL,
  "1", "2020-01-01", "PR", "001",
  "1", "2020-02-01", "CR", "001",
  "1", "2020-02-16", "NE", "001",
  "1", "2020-03-01", "CR", "001",
  "1", "2020-04-01", "SD", "001",
  "2", "2020-01-01", "SD", "002",
  "2", "2020-02-01", "PR", "002",
  "2", "2020-03-01", "SD", "002",
  "2", "2020-03-13", "CR", "002",
  "3", "2019-11-12", "CR", "003",
  "3", "2019-12-02", "CR", "003",
  "3", "2020-01-01", "SD", "003",
  "4", "2020-01-01", "PR", "004",
  "4", "2020-03-01", "SD", "004",
  "4", "2020-04-01", "SD", "004",
  "4", "2020-05-01", "PR", "004",
  "4", "2020-05-15", "NON-CR/NON-PD", "004",
  "5", "2020-01-01", "PR", "005",
  "5", "2020-01-10", "SD", "005",
  "5", "2020-01-20", "PR", "005",
  "5", "2020-05-15", "NON-CR/NON-PD", "005",
  "6", "2020-02-06", "PR", "006",
  "6", "2020-02-16", "CR", "006",
  "6", "2020-03-30", "PR", "006",
  "7", "2020-02-06", "PR", "007",
  "7", "2020-02-16", "CR", "007",
  "7", "2020-04-01", "NE", "007"
) %>%
  mutate(
    PARAMCD = "OVR",
    ADT = lubridate::ymd(ADTC),
    STUDYID = "XX1234"
  ) %>%
  select(-ADTC) %>%
  derive_vars_merged(
    dataset_add = adsl,
    by_vars = exprs(STUDYID, USUBJID),
    new_vars = exprs(TRTSDT)
  )

# Function to create numeric AVAL from AVALC, overwrites ADMIRAL default.
aval_fun_pass <- function(arg) {
  case_when(
    arg == "CR" ~ 11,
    arg == "PR" ~ 22,
    arg == "SD" ~ 33,
    arg == "NON-CR/NON-PD" ~ 44,
    arg == "PD" ~ 55,
    arg == "NE" ~ 66,
    arg == "MISSING" ~ 77,
    TRUE ~ NA_real_
  )
}

# excepcted dataframe, deviations from this are handeled in each individual test
expected_01 <- bind_rows(
  adrs,
  tibble::tribble(
    ~USUBJID, ~ADTC, ~AVALC, ~AVAL, ~TRTSDTC, ~CHECKKEPTCOL,
    "1", "2020-02-01", "CR", 11, "2020-01-01", "001",
    "2", "2020-03-13", "CR", 11, "2019-12-12", "002",
    "3", "2019-11-12", "CR", 11, "2019-11-11", "003",
    "4", "2020-01-01", "PR", 22, "2019-12-30", "004",
    "5", "2020-01-01", "PR", 22, "2020-01-01", "005",
    "6", "2020-02-16", "CR", 11, "2020-02-02", "006",
    "7", "2020-02-16", "CR", 11, "2020-02-02", "007",
    "8", "", "MISSING", 77, "2020-04-01", "008"
  ) %>%
    mutate(
      ADT = lubridate::ymd(ADTC),
      TRTSDT = lubridate::ymd(TRTSDTC),
      STUDYID = "XX1234",
      PARAMCD = "BOR",
      PARAM = "Best Overall Response"
    ) %>%
    select(-ADTC, -TRTSDTC)
)

## Test 1: No source_pd ----
test_that("derive_param_bor Test 1: No source_pd", {
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ### default  BOR, All Subjects have a record after reference date ----
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  actual_01 <- derive_param_bor(
    dataset = adrs,
    dataset_adsl = adsl,
    filter_source = PARAMCD == "OVR",
    source_pd = NULL,
    source_datasets = NULL,
    reference_date = TRTSDT,
    ref_start_window = 28,
    set_values_to = exprs(
      AVAL = {{ aval_fun_pass }}(AVALC),
      PARAMCD = "BOR",
      PARAM = "Best Overall Response"
    )
  )

  expect_dfs_equal(
    base    = expected_01,
    compare = actual_01,
    keys    = c("USUBJID", "PARAMCD", "ADT")
  )

  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # default  BOR, All Subjects have a record after reference date ----
  # use default for aval_fun
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  actual_01_def_aval_fun <- derive_param_bor(
    dataset = adrs,
    dataset_adsl = adsl,
    filter_source = PARAMCD == "OVR",
    source_pd = NULL,
    source_datasets = NULL,
    reference_date = TRTSDT,
    ref_start_window = 28,
    set_values_to = exprs(
      AVAL = aval_resp(AVALC),
      PARAMCD = "BOR",
      PARAM = "Best Overall Response"
    )
  )

  expected_01_def_aval_fun <- expected_01

  expected_01_def_aval_fun$AVAL[28:35] <- c(1, 1, 1, 2, 2, 1, 1, 7)

  expect_dfs_equal(
    base    = expected_01_def_aval_fun,
    compare = actual_01_def_aval_fun,
    keys    = c("USUBJID", "PARAMCD", "ADT")
  )

  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ### Subjects only have records less than reference date (PR/CR) ----
  # Response is PR and CR so will be included
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  # set subject 7 to only have records before ADT
  adrs_01 <- adrs
  adrs_01$ADT[adrs_01$USUBJID == "7"][1:3] <- c(
    lubridate::ymd("2020-01-01"),
    lubridate::ymd("2020-01-02"),
    lubridate::ymd("2020-01-03")
  )

  actual_02 <- derive_param_bor(
    dataset = adrs_01,
    dataset_adsl = adsl,
    filter_source = PARAMCD == "OVR",
    source_pd = NULL,
    source_datasets = NULL,
    reference_date = TRTSDT,
    ref_start_window = 28,
    set_values_to = exprs(
      AVAL = {{ aval_fun_pass }}(AVALC),
      PARAMCD = "BOR",
      PARAM = "Best Overall Response"
    )
  )

  # expected will be the same as previous test except ADT for subject 7
  expected_02 <- expected_01
  expected_02$ADT[expected_02$USUBJID == "7"][1:4] <- c(
    lubridate::ymd("2020-01-01"),
    lubridate::ymd("2020-01-02"),
    lubridate::ymd("2020-01-03"),
    lubridate::ymd("2020-01-02")
  )

  expect_dfs_equal(
    base    = expected_02,
    compare = actual_02,
    keys    = c("USUBJID", "PARAMCD", "ADT")
  )

  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ### Subjects only have records less than reference date (SD) ----
  # Response is SD will not be included and response will be NE
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  # set subject 7 to have response of SD
  adrs_02 <- adrs_01
  adrs_02$AVALC[adrs_02$USUBJID == "7"] <- "SD"

  actual_03 <- derive_param_bor(
    dataset = adrs_02,
    dataset_adsl = adsl,
    filter_source = PARAMCD == "OVR",
    source_pd = NULL,
    source_datasets = NULL,
    reference_date = TRTSDT,
    ref_start_window = 28,
    set_values_to = exprs(
      AVAL = {{ aval_fun_pass }}(AVALC),
      PARAMCD = "BOR",
      PARAM = "Best Overall Response"
    )
  )

  # Expected Updated for Subject 7, response is now NE and 6 with a earlier ADT (first)
  expected_03 <- expected_02
  expected_03$AVALC[expected_03$USUBJID == "7"] <- "SD"
  expected_03$ADT[expected_02$USUBJID == "7"][4] <- c(lubridate::ymd("2020-01-01"))
  expected_03$AVALC[expected_02$USUBJID == "7"][4] <- "NE"
  expected_03$AVAL[expected_02$USUBJID == "7"][4] <- 66

  expect_dfs_equal(
    base    = expected_03,
    compare = actual_03,
    keys    = c("USUBJID", "PARAMCD", "ADT")
  )
})

## Test 2: With source_pd ----
test_that("derive_param_bor Test 2: With source_pd", {
  pd_date <- admiral::date_source(
    dataset_name = "adrs_pd",
    date         = ADT,
    filter       = PARAMCD == "PD"
  )

  # Add PD rows to input ADRS dataset
  pd_records <- tibble::tribble(
    ~USUBJID, ~ADTC,        ~AVALC,
    "9",      "2020-02-16", "Y",
    "2",      "2020-03-01", "Y", # This subjects best response should now be PR
    "6",      "2020-02-05", "Y" # This subjects best response should now be MISSING
  ) %>%
    mutate(
      PARAMCD = "PD",
      ADT = lubridate::ymd(ADTC),
      STUDYID = "XX1234"
    ) %>%
    select(-ADTC)

  adrs_pd <- adrs %>%
    bind_rows(pd_records)

  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ### default  BOR, All Subjects have a record after reference date ----
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  actual_pd_01 <- derive_param_bor(
    dataset = adrs_pd,
    dataset_adsl = adsl,
    filter_source = PARAMCD == "OVR",
    source_pd = pd_date,
    source_datasets = list(adrs_pd = adrs_pd),
    reference_date = TRTSDT,
    ref_start_window = 28,
    set_values_to = exprs(
      AVAL = {{ aval_fun_pass }}(AVALC),
      PARAMCD = "BOR",
      PARAM = "Best Overall Response"
    )
  )

  # add PD recodrs to expected
  expected_pd_01 <- expected_01 %>%
    bind_rows(pd_records)

  # This is now PR as PD removed CR recrods
  expected_pd_01$AVALC[expected_pd_01$USUBJID == 2 &
    expected_pd_01$PARAMCD == "BOR"] <- "PR"
  expected_pd_01$AVAL[expected_pd_01$USUBJID == 2 &
    expected_pd_01$PARAMCD == "BOR"] <- 22
  expected_pd_01$ADT[expected_pd_01$USUBJID == 2 &
    expected_pd_01$PARAMCD == "BOR"] <- lubridate::ymd("2020-02-01")


  # This is now MISSING as PD removed all records
  expected_pd_01$AVALC[expected_pd_01$USUBJID == 6 &
    expected_pd_01$PARAMCD == "BOR"] <- "MISSING"
  expected_pd_01$AVAL[expected_pd_01$USUBJID == 6 &
    expected_pd_01$PARAMCD == "BOR"] <- 77
  expected_pd_01$ADT[expected_pd_01$USUBJID == 6 &
    expected_pd_01$PARAMCD == "BOR"] <- NA

  expect_dfs_equal(
    base    = expected_pd_01,
    compare = actual_pd_01,
    keys    = c("USUBJID", "PARAMCD", "ADT")
  )
})

## Test 3: Error if missing records for filter_source ----
test_that("derive_param_bor Test 3: Error if missing records for filter_source", {
  expect_error(
    derive_param_bor(
      dataset = adrs,
      dataset_adsl = adsl,
      filter_source = PARAMCD == "MISSING RECORDS",
      source_pd = NULL,
      source_datasets = NULL,
      reference_date = TRTSDT,
      ref_start_window = 28,
      set_values_to = exprs(
        PARAMCD = "BOR",
        PARAM = "Best Overall Response"
      )
    ),
    'PARAMCD == "MISSING RECORDS" has 0 records'
  )
})

## Test 4: Deprecation warning for aval_fun ----
test_that("derive_param_bor Test 4: Deprecation warning for aval_fun", {
  pd_date <- admiral::date_source(
    dataset_name = "adrs_pd",
    date         = ADT,
    filter       = PARAMCD == "PD"
  )

  # Add PD rows to input ADRS dataset
  pd_records <- tibble::tribble(
    ~USUBJID, ~ADTC,        ~AVALC,
    "9",      "2020-02-16", "Y",
    "2",      "2020-03-01", "Y", # This subjects best response should now be PR
    "6",      "2020-02-05", "Y" # This subjects best response should now be MISSING
  ) %>%
    mutate(
      PARAMCD = "PD",
      ADT = lubridate::ymd(ADTC),
      STUDYID = "XX1234"
    ) %>%
    select(-ADTC)

  adrs_pd <- adrs %>%
    bind_rows(pd_records)

  expect_warning(
    actual_pd_01 <- derive_param_bor(
      dataset = adrs_pd,
      dataset_adsl = adsl,
      filter_source = PARAMCD == "OVR",
      source_pd = pd_date,
      source_datasets = list(adrs_pd = adrs_pd),
      reference_date = TRTSDT,
      ref_start_window = 28,
      aval_fun = aval_fun_pass,
      set_values_to = exprs(
        PARAMCD = "BOR",
        PARAM = "Best Overall Response"
      )
    ),
    class = "lifecycle_warning_deprecated"
  )

  # add PD recodrs to expected
  expected_pd_01 <- expected_01 %>%
    bind_rows(pd_records)

  # This is now PR as PD removed CR recrods
  expected_pd_01$AVALC[expected_pd_01$USUBJID == 2 &
    expected_pd_01$PARAMCD == "BOR"] <- "PR"
  expected_pd_01$AVAL[expected_pd_01$USUBJID == 2 &
    expected_pd_01$PARAMCD == "BOR"] <- 22
  expected_pd_01$ADT[expected_pd_01$USUBJID == 2 &
    expected_pd_01$PARAMCD == "BOR"] <- lubridate::ymd("2020-02-01")


  # This is now MISSING as PD removed all records
  expected_pd_01$AVALC[expected_pd_01$USUBJID == 6 &
    expected_pd_01$PARAMCD == "BOR"] <- "MISSING"
  expected_pd_01$AVAL[expected_pd_01$USUBJID == 6 &
    expected_pd_01$PARAMCD == "BOR"] <- 77
  expected_pd_01$ADT[expected_pd_01$USUBJID == 6 &
    expected_pd_01$PARAMCD == "BOR"] <- NA

  expect_dfs_equal(
    base    = expected_pd_01,
    compare = actual_pd_01,
    keys    = c("USUBJID", "PARAMCD", "ADT")
  )
})
