# Name: ADTTE
#
# Label: Time to Event Analysis Dataset
#
# Input: adsl, adrs, tte_source objects
library(admiral)
library(admiralonco)
# pharmaverseadam contains example datasets generated from the CDISC pilot
# project SDTM ran through admiral templates
library(pharmaverseadam)
library(dplyr)
library(lubridate)

# Load source datasets ----

# Use e.g. haven::read_sas to read in .sas7bdat, or other suitable functions
# as needed and assign to the variables below.
# For illustration purposes read in admiral test data

data("adsl")
data("adrs_onco")

adrs <- adrs_onco

# Derivations ----

# Add response date to ADSL for duration of response calculation
adsl <- adsl %>%
  derive_vars_merged(
    dataset_add = adrs,
    filter_add = PARAMCD == "RSP" & AVALC == "Y" & ANL01FL == "Y",
    by_vars = exprs(STUDYID, USUBJID),
    new_vars = exprs(TEMP_RESPDT = ADT)
  )

# Use pre-defined tte_source objects to derive Overall Survival, Progression
# Free Survival and Duration of Response
adtte <- derive_param_tte(
  dataset_adsl = adsl,
  start_date = RANDDT,
  event_conditions = list(death_event),
  censor_conditions = list(lastalive_censor, rand_censor),
  source_datasets = list(adsl = adsl, adrs = adrs),
  set_values_to = exprs(PARAMCD = "OS", PARAM = "Overall Survival")
) %>%
  derive_param_tte(
    dataset_adsl = adsl,
    start_date = RANDDT,
    event_conditions = list(pd_event, death_event),
    censor_conditions = list(lasta_censor, rand_censor),
    source_datasets = list(adsl = adsl, adrs = adrs),
    set_values_to = exprs(PARAMCD = "PFS", PARAM = "Progression Free Survival")
  ) %>%
  derive_param_tte(
    dataset_adsl = filter(adsl, !is.na(TEMP_RESPDT)),
    start_date = TEMP_RESPDT,
    event_conditions = list(pd_event, death_event),
    censor_conditions = list(lasta_censor),
    source_datasets = list(adsl = adsl, adrs = adrs),
    set_values_to = exprs(PARAMCD = "RSD", PARAM = "Duration of Response")
  )

# Derive analysis value and sequence
adtte <- adtte %>%
  derive_vars_duration(
    new_var = AVAL,
    start_date = STARTDT,
    end_date = ADT
  ) %>%
  derive_var_obs_number(
    by_vars = exprs(STUDYID, USUBJID),
    order = exprs(PARAMCD),
    check_type = "error"
  )

# Join any required ADSL variables
adtte <- adtte %>%
  derive_vars_merged(
    dataset_add = adsl,
    new_vars = exprs(ARMCD, ARM, ACTARMCD, ACTARM, AGE, SEX),
    by_vars = exprs(STUDYID, USUBJID)
  )

# Save output ----

dir <- tempdir() # Change to whichever directory you want to save the dataset in
saveRDS(adtte, file = file.path(dir, "adtte.rds"), compress = "bzip2")
