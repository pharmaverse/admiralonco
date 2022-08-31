######################################################################## !
#  Description: Unit Testing Exposed Function derive_param_bor         #
#                                                                      #
#  Requirements: https://github.com/pharmaverse/admiralonco/issues/25  #
######################################################################## !

library(tibble)
library(dplyr)
library(lubridate)
library(admiraldev)
library(admiral)

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

adsl <- tribble(
  ~USUBJID, ~TRTSDTC, ~EOSDTC, ~CHECKKEPTCOL, ~CHECKNOTKEPTCOL,
  "01", "2020-12-06", "2022-03-06", "001", "991",
  "02", "2021-01-16", "2022-02-03", "002", "992",
  "03", "2021-01-09", "2021-02-24", "003", "993",
  "04", "2021-04-21", "2021-09-15", "004", "994",
  "05", "2021-08-21", "2022-01-11", "005", "995"
) %>%
  mutate(
    STUDYID = "a_study_id",
    TRTSDT = ymd(TRTSDTC),
    EOSDT = ymd(EOSDTC)
  ) %>%
  select(-c(TRTSDTC, EOSDTC))

adrs <- tribble(
  ~USUBJID, ~PARAMCD, ~AVAL, ~AVALC, ~ASEQ, ~ADTC, ~CHECKKEPTCOL,
  "01", "RSP", NA, "Y", 1, "2021-04-08", "001",
  "02", "RSP", NA, "N", 1, "2021-05-07", "002",
  "03", "RSP", NA, "N", 1, NA, "003",
  "04", "RSP", NA, "N", 1, NA, "004",
  "01", "PD", NA, "N", 1, NA, "001",
  "02", "PD", NA, "Y", 1, "2021-05-06", "002",
  "03", "PD", NA, "N", 1, NA, "003",
  "04", "PD", NA, "N", 1, NA, "004",
  "01", "OVR", 3, "SD", 1, "2021-03-07", "001",
  "01", "OVR", 2, "PR", 1, "2021-04-08", "001",
  "02", "OVR", 3, "SD", 1, "2021-03-07", "002",
  "02", "OVR", NA, NA, 1, "2021-04-07", "002",
  "02", "OVR", 6, "PD", 1, "2021-05-07", "002",
  "03", "OVR", 3, "SD", 1, "2021-01-30", "003",
  "03", "OVR", 3, "SD", 2, "2021-01-30", "003",
  "04", "OVR", NA, "NE", 1, "2021-05-21", "004",
  "04", "OVR", 5, "NON-PD", 1, "2021-06-30", "004",
  "04", "OVR", NA, "NE", 1, "2021-07-24", "004",
  "04", "OVR", NA, "ND", 1, "2021-09-30", "004"
) %>%
  mutate(
    STUDYID = "a_study_id",
    ADT = ymd(ADTC)
  ) %>%
  select(-c(ADTC))

pd_test <- date_source(
  dataset_name = "adrs",
  date         = ADT,
  filter       = PARAMCD == "PD" & AVALC == "Y" # check with Catherine
)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# testthat calls ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

test_that("Last assesment derived correctly from derive_param_lasta", {

  # Test 1: No removal of NE and censored up to PD ----
  actual_01 <- derive_param_lasta(
    dataset = adrs,
    order = vars(USUBJID, ADT, ASEQ),
    filter_source = PARAMCD == "OVR", # & ANL01FL == "Y",
    source_pd = pd_test,
    source_datasets = list(adrs = adrs),
    set_values_to = vars(
      PARAMCD = "LSTAC",
      PARAM   = "Last Disease Assessment Censored at First PD by Investigator",
      PARCAT1 = "Tumor Response",
      PARCAT2 = "Investigator",
      PARCAT3 = "Recist 1.1",
      ANL01FL = "Y"
    )
  )

  # expected output
  expected_01 <- bind_rows(
    adrs,
    tribble(
      ~USUBJID, ~AVAL, ~AVALC, ~ASEQ, ~ADTC, ~CHECKKEPTCOL,
      "01", 2, "PR", 1, "2021-04-08", "001",
      "02", NA, NA, 1, "2021-04-07", "002",
      "03", 3, "SD", 2, "2021-01-30", "003",
      "04", NA, "ND", 1, "2021-09-30", "004"
    ) %>%
      mutate(
        ADT = ymd(ADTC),
        PARAMCD = "LSTAC",
        PARAM = "Last Disease Assessment Censored at First PD by Investigator",
        PARCAT1 = "Tumor Response",
        PARCAT2 = "Investigator",
        PARCAT3 = "Recist 1.1",
        ANL01FL = "Y",
        STUDYID = "a_study_id"
      ) %>%
      select(-c(ADTC))
  )

  # compare
  expect_dfs_equal(
    actual_01,
    expected_01,
    keys = c("USUBJID", "PARAMCD", "ADT", "ASEQ")
  )

  # Test 2: No removal of NE and NOT censored up to PD ----
  actual_02 <- derive_param_lasta(
    dataset = adrs,
    order = vars(USUBJID, ADT, ASEQ),
    filter_source = PARAMCD == "OVR", # & ANL01FL == "Y",
    source_pd = NULL,
    source_datasets = NULL,
    set_values_to = vars(
      PARAMCD = "LSTA",
      PARAM   = "Last Disease Assessment by Investigator",
      PARCAT1 = "Tumor Response",
      PARCAT2 = "Investigator",
      PARCAT3 = "Recist 1.1",
      ANL01FL = "Y"
    )
  )

  # create expected dataframe
  expected_02 <- expected_01

  # expected will be the same as previous test except for subject 2, which will now be later
  # as PDs not removed
  expected_02$PARAMCD[expected_02$PARAMCD == "LSTAC"] <- "LSTA"
  expected_02$PARAM[expected_02$PARAMCD == "LSTA"] <- "Last Disease Assessment by Investigator"
  expected_02$ADT[expected_02$USUBJID == "02" &
    expected_02$PARAMCD == "LSTA"] <- ymd("2021-05-07")
  expected_02$AVAL[expected_02$USUBJID == "02" &
    expected_02$PARAMCD == "LSTA"] <- 6
  expected_02$AVALC[expected_02$USUBJID == "02" &
    expected_02$PARAMCD == "LSTA"] <- "PD"

  # compare
  expect_dfs_equal(
    actual_02,
    expected_02,
    keys = c("USUBJID", "PARAMCD", "ADT", "ASEQ")
  )

  # Test 3: Remove NE and censored up to PD ----
  actual_03 <- derive_param_lasta(
    dataset = adrs,
    order = vars(USUBJID, ADT, ASEQ),
    filter_source = PARAMCD == "OVR" & !(AVALC %in% c(NA, "NE", "ND", "NA")), # & ANL01FL == "Y",
    source_pd = pd_test,
    source_datasets = list(adrs = adrs),
    set_values_to = vars(
      PARAMCD = "LSTAC",
      PARAM = "Last Disease Assessment Censored at First PD by Investigator",
      PARCAT1 = "Tumor Response",
      PARCAT2 = "Investigator",
      PARCAT3 = "Recist 1.1",
      ANL01FL = "Y"
    )
  )

  # create expected dataframe
  expected_03 <- expected_01

  # Subject 2 is now a SD as NA has been removed and records >= PD.
  expected_03$ADT[expected_03$USUBJID == "02" &
    expected_03$PARAMCD == "LSTAC"] <- ymd("2021-03-07")
  expected_03$AVAL[expected_03$USUBJID == "02" & expected_03$PARAMCD == "LSTAC"] <- 3
  expected_03$AVALC[expected_03$USUBJID == "02" & expected_03$PARAMCD == "LSTAC"] <- "SD"

  # Subject 4 is now a NON-PD as NA has been removed and records >= PD.
  expected_03$ADT[expected_03$USUBJID == "04" &
    expected_03$PARAMCD == "LSTAC"] <- ymd("2021-06-30")
  expected_03$AVAL[expected_03$USUBJID == "04" & expected_03$PARAMCD == "LSTAC"] <- 5
  expected_03$AVALC[expected_03$USUBJID == "04" & expected_03$PARAMCD == "LSTAC"] <- "NON-PD"

  # compare
  expect_dfs_equal(
    actual_03,
    expected_03,
    keys = c("USUBJID", "PARAMCD", "ADT", "ASEQ")
  )
})

test_that("Errors correctly from derive_param_lasta", {

  # Test Error 1: missing dataset argument ----
  expect_error(
    derive_param_lasta(
      dataset = "not a dataset",
      order = vars(USUBJID, ADT, ASEQ),
      filter_source = PARAMCD == "OVR" & !(AVALC %in% c(NA, "NE", "ND", "NA")),
      source_pd = pd_test,
      source_datasets = list(adrs = adrs),
      set_values_to = vars(
        PARAMCD = "LSTAC",
        PARAM = " Last Disease Assessment Censored at First PD by Investigator",
        PARCAT1 = "Tumor Response",
        PARCAT2 = "Investigator",
        PARCAT3 = "Recist 1.1",
        ANL01FL = "Y"
      )
    ),
    "`dataset` must be a data frame but is"
  )

  # Test Error 2: missing set_values_to argument ----
  expect_error(
    derive_param_lasta(
      dataset         = adrs,
      order           = vars(USUBJID, ADT, ASEQ),
      filter_source   = PARAMCD == "OVR" & !(AVALC %in% c(NA, "NE", "ND", "NA")),
      source_pd       = pd_test,
      source_datasets = list(adrs = adrs),
      set_values_to   = "not a list"
    ),
    "`set_values_to` must be a named list of quosures where each element"
  )

  # Test Error 3: No PARAMCD in dataset ----
  adrs_test_missing_paramcd <-
    adrs %>% select(-PARAMCD)

  expect_error(
    derive_param_lasta(
      dataset = adrs_test_missing_paramcd,
      order = vars(USUBJID, ADT, ASEQ),
      filter_source = PARAMCD == "OVR" & !(AVALC %in% c(NA, "NE", "ND", "NA")),
      source_pd = pd_test,
      source_datasets = list(adrs = adrs),
      set_values_to = vars(
        PARAMCD = "LSTAC",
        PARAM = " Last Disease Assessment Censored at First PD by Investigator",
        PARCAT1 = "Tumor Response",
        PARCAT2 = "Investigator",
        PARCAT3 = "Recist 1.1",
        ANL01FL = "Y"
      )
    ),
    "Required variable `PARAMCD` is missing"
  )

  # Test Error 5 ----
  expect_error(
    derive_param_lasta(
      dataset = adrs,
      order = vars(USUBJID, ADT, ASEQ),
      filter_source = PARAMCD == "MISSING",
      source_pd = pd_test,
      source_datasets = list(adrs = adrs),
      set_values_to = vars(
        PARAMCD = "LSTAC",
        PARAM = " Last Disease Assessment Censored at First PD by Investigator",
        PARCAT1 = "Tumor Response",
        PARCAT2 = "Investigator",
        PARCAT3 = "Recist 1.1",
        ANL01FL = "Y"
      )
    ),
    "dataframe passed into dataset argument with the filter PARAMCD"
  )
})
