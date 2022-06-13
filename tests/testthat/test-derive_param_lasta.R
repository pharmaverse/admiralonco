########################################################################!
#  Description: Unit Testing Exposed Function derive_param_bor         #
#                                                                      #
#  Requirements: https://github.com/pharmaverse/admiralonco/issues/25  #
########################################################################!

library(magrittr)

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

adsl_test <- tibble::tribble(
  ~USUBJID, ~TRTSDT,           ~EOSDT,                                  ~CHECKKEPTCOL, ~CHECKNOTKEPTCOL,
  "01",     lubridate::ymd("2020-12-06"), lubridate::ymd("2022-03-06"), "001",         "991",
  "02",     lubridate::ymd("2021-01-16"), lubridate::ymd("2022-02-03"), "002",         "992",
  "03",     lubridate::ymd("2021-01-09"), lubridate::ymd("2021-02-24"), "003",         "993",
  "04",     lubridate::ymd("2021-04-21"), lubridate::ymd("2021-09-15"), "004",         "994",
  "05",     lubridate::ymd("2021-08-21"), lubridate::ymd("2022-01-11"), "005",         "995"
) %>%
  dplyr::mutate(STUDYID = "a_study_id")

adrs_test <- tibble::tribble(
  ~USUBJID, ~PARAMCD, ~AVAL, ~AVALC, ~ASEQ, ~ADT, ~CHECKKEPTCOL,
  "01", "RSP", NA, "Y", 1, lubridate::ymd("2021-04-08"),"001",
  "02", "RSP", NA, "N", 1, lubridate::ymd("2021-05-07"),"002",
  "03", "RSP", NA, "N", 1, NA,"003",
  "04", "RSP", NA, "N", 1, NA,"004",
  "01", "PD", NA, "N", 1, NA,"001",
  "02", "PD", NA, "Y", 1, lubridate::ymd("2021-05-07"),"002",
  "03", "PD", NA, "N", 1, NA,"003",
  "04", "PD", NA, "N", 1, NA,"004",
  "01", "OVR", 3, "SD", 1, lubridate::ymd("2021-03-07"),"001",
  "01", "OVR", 2, "PR", 1, lubridate::ymd("2021-04-08"),"001",
  "02", "OVR", 3, "SD", 1, lubridate::ymd("2021-03-07"),"002",
  "02", "OVR", NA, NA, 1, lubridate::ymd("2021-04-07"),"002",
  "02", "OVR", 6, "PD", 1, lubridate::ymd("2021-05-07"),"002",
  "03", "OVR", 3, "SD", 1, lubridate::ymd("2021-01-30"),"003",
  "03", "OVR", 3, "SD", 2, lubridate::ymd("2021-01-30"),"003",
  "04", "OVR", NA, "NE", 1, lubridate::ymd("2021-05-21"),"004",
  "04", "OVR", 5, "NON-PD", 1, lubridate::ymd("2021-06-30"),"004",
  "04", "OVR", NA, "NE", 1, lubridate::ymd("2021-07-24"),"004",
  "04", "OVR", NA, "ND", 1, lubridate::ymd("2021-09-30"),"004",
) %>%
  dplyr::mutate(STUDYID = "a_study_id")

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# testthat calls ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

test_that("Last assesment derived correctly from derive_param_lasta", {

  pd_test <-  admiral::date_source(
    dataset_name = "adrs_test",
    date         = ADT,
    filter       = PARAMCD == "PD" & AVALC == "Y" # check with Catherine
  )

  # Test 1: No removal of NE and censored up to PD ----
  actual_output_lstac <- derive_param_lasta(
    dataset         = adrs_test,
    dataset_adsl    = adsl_test,
    order           = admiral::vars(USUBJID, ADT, ASEQ),
    filter_source   = PARAMCD == "OVR", # & ANL01FL == "Y",
    source_pd       = pd_test,
    source_datasets = list(adrs_test = adrs_test),
    set_values_to   = vars(
        PARAMCD = "LSTAC",
        PARAM = " Last Disease Assessment Censored at First PD by Investigator",
        PARCAT1 = "Tumor Response",
        PARCAT2 = "Investigator",
        PARCAT3 = "Recist 1.1",
        ANL01FL = "Y")
  )

  # expected output
  expected_output_lstac <- tibble::tribble(
    ~USUBJID, ~AVAL, ~AVALC, ~ASEQ, ~ADT,
    "01", 2, "PR", 1, lubridate::ymd("2021-04-08"),
    "02", NA, NA, 1, lubridate::ymd("2021-04-07"),
    "03", 3, "SD", 2, lubridate::ymd("2021-01-30"),
    "04", NA, "ND", 1, lubridate::ymd("2021-09-30"),) %>%
    dplyr::mutate(PARAMCD = "LSTAC",
                  PARAM   = " Last Disease Assessment Censored at First PD by Investigator",
                  PARCAT1 = "Tumor Response",
                  PARCAT2 = "Investigator",
                  PARCAT3 = "Recist 1.1",
                  ANL01FL = "Y",
                  STUDYID = "a_study_id")

  # join with original data
  expected_output_lstac_plus_source <- dplyr::bind_rows(adrs_test,
                                                        expected_output_lstac)

  # compare
  admiral::expect_dfs_equal(actual_output_lstac,
                            expected_output_lstac_plus_source,
                            keys = c("USUBJID", "PARAMCD", "ADT", "ASEQ"))

  # Test 2: No removal of NE and NOT censored up to PD ----
  actual_output_lsta <- derive_param_lasta(
    dataset         = adrs_test,
    order           = admiral::vars(USUBJID, ADT, ASEQ),
    filter_source   = PARAMCD == "OVR", # & ANL01FL == "Y",
    source_pd       = NULL,
    source_datasets = NULL,
    set_values_to = vars(
      PARAMCD = "LSTA",
      PARAM = " Last Disease Assessment by Investigator",
      PARCAT1 = "Tumor Response",
      PARCAT2 = "Investigator",
      PARCAT3 = "Recist 1.1",
      ANL01FL = "Y")
  )

  # expected output
  expected_output_lsta <- tibble::tribble(
    ~USUBJID, ~AVAL, ~AVALC, ~ASEQ, ~ADT,
    "01", 2, "PR", 1, lubridate::ymd("2021-04-08"),
    "02", 6, "PD", 1, lubridate::ymd("2021-05-07"),
    "03", 3, "SD", 2, lubridate::ymd("2021-01-30"),
    "04", NA, "ND", 1, lubridate::ymd("2021-09-30"),) %>%
    dplyr::mutate(PARAMCD = "LSTA",
                  PARAM   = " Last Disease Assessment by Investigator",
                  PARCAT1 = "Tumor Response",
                  PARCAT2 = "Investigator",
                  PARCAT3 = "Recist 1.1",
                  ANL01FL = "Y",
                  STUDYID = "a_study_id")

  # join with original data
  expected_output_lsta_plus_source <- dplyr::bind_rows(adrs_test,
                                                        expected_output_lsta)

  # compare
  admiral::expect_dfs_equal(actual_output_lsta,
                            expected_output_lsta_plus_source,
                            keys = c("USUBJID", "PARAMCD", "ADT", "ASEQ"))

  # Test 3: Remove NE and censored up to PD ----
  actual_output_lstac_ne_removed <- derive_param_lasta(
    dataset         = adrs_test,
    order           = admiral::vars(USUBJID, ADT, ASEQ),
    filter_source   = PARAMCD == "OVR" & !(AVALC %in% c(NA, "NE", "ND", "NA")), # & ANL01FL == "Y",
    source_pd       = pd_test,
    source_datasets = list(adrs_test = adrs_test),
    set_values_to   = vars(
      PARAMCD = "LSTAC",
      PARAM = " Last Disease Assessment Censored at First PD by Investigator",
      PARCAT1 = "Tumor Response",
      PARCAT2 = "Investigator",
      PARCAT3 = "Recist 1.1",
      ANL01FL = "Y")
  )

  # expected output
  expected_output_lstac_ne_removed <- tibble::tribble(
    ~USUBJID, ~AVAL, ~AVALC, ~ASEQ, ~ADT,
    "01", 2, "PR", 1, lubridate::ymd("2021-04-08"),
    "02", 3, "SD", 1, lubridate::ymd("2021-03-07"),
    "03", 3, "SD", 2, lubridate::ymd("2021-01-30"),
    "04", 5, "NON-PD", 1, lubridate::ymd("2021-06-30"),) %>%
    dplyr::mutate(PARAMCD = "LSTAC",
                  PARAM   = " Last Disease Assessment Censored at First PD by Investigator",
                  PARCAT1 = "Tumor Response",
                  PARCAT2 = "Investigator",
                  PARCAT3 = "Recist 1.1",
                  ANL01FL = "Y",
                  STUDYID = "a_study_id")

  # join with original data
  expected_output_lstac_ne_removed_plus_source <- dplyr::bind_rows(adrs_test,
                                                        expected_output_lstac_ne_removed)

  # compare
  admiral::expect_dfs_equal(actual_output_lstac_ne_removed,
                            expected_output_lstac_ne_removed_plus_source,
                            keys = c("USUBJID", "PARAMCD", "ADT", "ASEQ"))

})

test_that("Errors correctly from derive_param_lasta", {

  # Test Error 1: missing dataset argument ----
  testthat::expect_error(
        derive_param_lasta(
            dataset         = "not a dataset",
            order           = admiral::vars(USUBJID, ADT, ASEQ),
            filter_source   = PARAMCD == "OVR" & !(AVALC %in% c(NA, "NE", "ND", "NA")), # & ANL01FL == "Y",
            source_pd       = pd_test,
            source_datasets = list(adrs_test = adrs_test),
            set_values_to   = vars(
              PARAMCD = "LSTAC",
              PARAM = " Last Disease Assessment Censored at First PD by Investigator",
              PARCAT1 = "Tumor Response",
              PARCAT2 = "Investigator",
              PARCAT3 = "Recist 1.1",
              ANL01FL = "Y")
          ),
       "`dataset` must be a data frame but is")

  # Test Error 2: missing source_datasets argument ----
  testthat::expect_error(
    derive_param_lasta(
      dataset         = adrs_test,
      order           = admiral::vars(USUBJID, ADT, ASEQ),
      filter_source   = PARAMCD == "OVR" & !(AVALC %in% c(NA, "NE", "ND", "NA")), # & ANL01FL == "Y",
      source_pd       = pd_test,
      source_datasets = "not a list",
      set_values_to   = vars(
        PARAMCD = "LSTAC",
        PARAM = " Last Disease Assessment Censored at First PD by Investigator",
        PARCAT1 = "Tumor Response",
        PARCAT2 = "Investigator",
        PARCAT3 = "Recist 1.1",
        ANL01FL = "Y")
    ),
    "The dataset names must be included in the list specified for the `source_datasets` parameter.")

  # Test Error 3: missing set_values_to argument ----
  testthat::expect_error(
    derive_param_lasta(
      dataset         = adrs_test,
      order           = admiral::vars(USUBJID, ADT, ASEQ),
      filter_source   = PARAMCD == "OVR" & !(AVALC %in% c(NA, "NE", "ND", "NA")), # & ANL01FL == "Y",
      source_pd       = pd_test,
      source_datasets = list(adrs_test = adrs_test),
      set_values_to   = "not a list"
      ),
     "`set_values_to` must be a named list of quosures where each element")

  # Test Error 4: No PARAMCD in dataset ----
  adrs_test_missing_paramcd <-
    adrs_test %>% dplyr::select(-PARAMCD)

  testthat::expect_error(
    derive_param_lasta(
      dataset         = adrs_test_missing_paramcd,
      order           = admiral::vars(USUBJID, ADT, ASEQ),
      filter_source   = PARAMCD == "OVR" & !(AVALC %in% c(NA, "NE", "ND", "NA")), # & ANL01FL == "Y",
      source_pd       = pd_test,
      source_datasets = list(adrs_test = adrs_test),
      set_values_to   = vars(
        PARAMCD = "LSTAC",
        PARAM = " Last Disease Assessment Censored at First PD by Investigator",
        PARCAT1 = "Tumor Response",
        PARCAT2 = "Investigator",
        PARCAT3 = "Recist 1.1",
        ANL01FL = "Y")
    ),
    "Required variable `PARAMCD` is missing")
})

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Amgen Data TO DELETE ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

source("~/admiralonco/R/date_source.R")
source("~/admiralonco/R/derive_param_lasta.R")

adsl_amgen <- haven::read_sas("/userdata/stat/amg160/onc/20180101/analysis/final/statdata/adam/adsl.sas7bdat")

adrs_amgen <- haven::read_sas("/userdata/stat/amg160/onc/20180101/analysis/final/statdata/adam/adrs.sas7bdat") %>%
  dplyr::select(USUBJID, PARAM, PARAMCD, ADT, ASEQ, AVAL, AVALC, dplyr::starts_with("ANL"))

test_pd <-  date_source(
  dataset_name = "adrs_amgen",
  date         = ADT,
  filter       = PARAMCD == "CLINRESP" & AVALC == "PD" # check with Catherine
)

LSTAC <- derive_param_lasta(
  dataset      = adrs_amgen,
  order           = admiral::vars(USUBJID, ADT, ASEQ),
  filter_source = PARAMCD == "OVRLRESP", # & ANL01FL == "Y",
  source_pd = test_pd,
  source_datasets = list(adrs_amgen = adrs_amgen),
  set_values_to = vars(
    PARAMCD = "LSTAC",
    PARAM = " Last Disease Assessment Censored at First PD by Investigator",
    PARCAT1 = "Tumor Response",
    PARCAT2 = "Investigator",
    PARCAT3 = "Recist 1.1",
    ANL01FL = "Y")
)

View(LSTAC %>% dplyr::filter(PARAMCD == "LSTAC"))


LSTA <- derive_param_lasta(
  dataset      = adrs_amgen,
  order           = admiral::vars(USUBJID, ADT, ASEQ),
  filter_source = PARAMCD == "OVRLRESP", # & ANL01FL == "Y",
  source_pd = NULL,
  source_datasets = NULL,
  set_values_to = vars(
    PARAMCD = "LSTA",
    PARAM = " Last Disease Assessment by Investigator",
    PARCAT1 = "Tumor Response",
    PARCAT2 = "Investigator",
    PARCAT3 = "Recist 1.1",
    ANL01FL = "Y")
)

View(LSTA %>% dplyr::filter(PARAMCD == "LSTA"))
