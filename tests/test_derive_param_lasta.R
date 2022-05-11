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

library(magrittr)
library(assertthat)
library(dplyr)
library(admiral.test)
library(admiral)

adsl_amgen <- haven::read_sas("/userdata/stat/amg160/onc/20180101/analysis/final/statdata/adam/adsl.sas7bdat")

adrs_amgen <- haven::read_sas("/userdata/stat/amg160/onc/20180101/analysis/final/statdata/adam/adrs.sas7bdat") %>%
  dplyr::select(USUBJID, PARAM, PARAMCD, ADT, AVAL, AVALC, dplyr::starts_with("ANL"))

# Filter and sort as requirement
adrs_amgen_02 <- adrs_amgen %>%
  dplyr::filter(PARAMCD == "OVRLRESP")  # what about NE's and others like UNDEFIINED, NON-CR?

# flag last assessment using derive_extreme_flag
adrs_amgen_03 <- adrs_amgen_02 %>%
  derive_extreme_flag(by_vars = vars(USUBJID, PARAMCD),
                      order   = vars(ADT),
                      mode    = "last",
                      new_var = TEMP)

# rename 
param_lstac <- adrs_amgen_03 %>%
  dplyr::filter(TEMP == "Y") %>%
  dplyr::mutate(
      PARAMCD = "LSTAC",
      PARAM   = " Last Disease Assessment Censored at First PD by Investigator",
      PARCAT1 = "Tumor Response",
      PARCAT2 = "Investigator",
      PARCAT3 = "Recist 1.1",
      ANL01FL = "Y")

# bind back 
adrs_amgen_04 <- dplyr::bind_rows(adrs_amgen_03, param_lstac) 

source("~/admiralonco/R/date_source.R")
source("~/admiralonco/R/derive_param_lasta.R")

adsl_amgen <- haven::read_sas("/userdata/stat/amg160/onc/20180101/analysis/final/statdata/adam/adsl.sas7bdat")

adrs_amgen <- haven::read_sas("/userdata/stat/amg160/onc/20180101/analysis/final/statdata/adam/adrs.sas7bdat") %>%
  dplyr::select(USUBJID, PARAM, PARAMCD, ADT, AVAL, AVALC, dplyr::starts_with("ANL"))

test_pd <-  date_source(
  dataset_name = "adrs_amgen",
  date         = ADT,
  filter       = PARAMCD == "CLINRESP" & AVALC == "PD" # check with Catherine
)

check <- derive_param_lasta(
  dataset      = adrs_amgen,
  dataset_adsl = adsl_amgen,
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

pd_data <- eval(rlang::parse_expr(test_pd$dataset_name)) %>%
  admiral::filter_if(test_pd$filter) %>%
  dplyr::select(!!!subject_keys, !!!by_vars, !!test_pd$date) %>%
  dplyr::rename(temp_pd_date = !!test_pd$date)

nrow(pd_data)

dataset_filer <- adrs_amgen %>% 
  dplyr::left_join(pd_data, by = "USUBJID") %>%
  dplyr::filter(ADT >= temp_pd_date)

nrow(dataset_filer)
nrow(adrs_amgen)

adrs_amgen<- adrs_amgen %>%
  dplyr::filter(ADT >= TRTSDT)

nrow(check)
test_that("Last assesment derived correctly", {

  input_cbr <-  tibble::tribble(
    ~USUBJID, ~PARAMCD, ~AVALC, ~ADT,
    "01", "CBR", "Y", ymd("2021-04-08"),
    "02", "CBR", "Y", ymd("2021-03-07"),
    "03", "CBR", "N", NA,
    "04", "CBR", "N", NA)  %>%
    mutate(STUDYID = "AB42")

  expected_output <- bind_rows(adrs, input_cbr)


  actual_output <- derive_param_lasta(
    dataset = adrs,
    dataset_adsl = adsl,
    source_param = "OVR",
    source_resp = resp,
    source_pd = pd,
    reference_date = TRTSDT,
    ref_start_window = 28,
    set_values_to = vars(
      PARAMCD = "CBR"
    )
  )

  expect_dfs_equal(actual_output, expected_output,
                   keys = c("USUBJID", "PARAMCD", "ADT"))

})
death <- event_source(
  dataset_name = "adsl",
  filter = DTHFL == "Y",
  date = DTHDT,
  set_values_to = vars(
    EVNTDESC = "DEATH",
    SRCDOM = "ADSL",
    SRCVAR = "DTHDT"
  )
)

lstalv <- censor_source(
  dataset_name = "adsl",
  date = LSTALVDT,
  set_values_to = vars(
    EVNTDESC = "LAST KNOWN ALIVE DATE",
    SRCDOM = "ADSL",
    SRCVAR = "LSTALVDT"
  )
)

assert_list_element(
  list = list(source_pd),
  element = "dataset_name",
  condition = dataset_name %in% "source_names",
  message_text = "For events the censor values must be zero."
)


admiral::assert_list_element(
  list         = source_pd,
  element      = "dataset_name",
  condition    = dataset_name %in% source_names,
  source_names = source_names,
  message_text = paste0(
    "The dataset names must be included in the list specified for the ",
    "`source_datasets` parameter.\n",
    "Following names were provided by `source_datasets`:\n",
    admiral:::enumerate(source_names, quote_fun = sQuote)
  )
)