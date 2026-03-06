# Explore the admiralonco ADaM Templates

## Introduction

This page is a catalog of the ADaM templates available through
[admiralonco](https://pharmaverse.github.io/admiralonco/). These are
lifted from our GitHub repository. The intention is for this to be a
reference for users, should they wish to peruse the code of a specific
template without visiting the
[admiralonco](https://pharmaverse.github.io/admiralonco/) repository.

As a reminder, you can create a starter script from a template using
[`admiral::use_ad_template()`](https:/pharmaverse.github.io/admiral/v1.4.1/cran-release/reference/use_ad_template.html),
e.g.,

``` r
use_ad_template(
  adam_name = "adrs",
  save_path = "./ad_adrs.R",
  package = "admiralonco"
)
```

## ADaM templates

ad_adrs_basic.R

```
# Name: ADRS
#
# Label: Response Analysis Dataset
#
# Input: adsl, rs, tu
#
# Please note that this template implements the endpoints which were considered
# by the admiralonco team as the most common ones. The admiralonco functions
# used to derive these endpoints provide a certain flexibility, e.g., specifying
# the reference date or time windows for confirmation or stable disease. If
# different endpoints or more flexibility is required please use the ad_adrs.R
# template.

library(admiral)
library(admiralonco)
library(pharmaversesdtm) # Contains example datasets from the CDISC pilot project
# pharmaverseadam contains example datasets generated from the CDISC pilot
# project SDTM ran through admiral templates
library(pharmaverseadam)
library(dplyr)
library(lubridate)
library(stringr)

# ---- Load source datasets ----

# Use e.g. haven::read_sas to read in .sas7bdat, or other suitable functions
# as needed and assign to the variables below.
# For illustration purposes read in pharmaverse test data

data("adsl")
data("rs_onco_recist")
data("tu_onco_recist")

rs <- rs_onco_recist
tu <- tu_onco_recist

rs <- convert_blanks_to_na(rs)
tu <- convert_blanks_to_na(tu)

# ---- Derivations ----

# Get list of ADSL vars required for derivations - here we assume randomized study
adsl_vars <- exprs(RANDDT)

# Join ADSL vars to RS
adrs <- rs %>%
  derive_vars_merged(
    dataset_add = adsl,
    new_vars = adsl_vars,
    by_vars = get_admiral_option("subject_keys")
  )

# ---- Company-specific pre-processing ----

# Filtering to select Overall Response records - here we used Investigator records
# but all these steps could equally be repeated for Independent Review Facility
adrs <- adrs %>%
  filter(RSEVAL == "INVESTIGATOR" & RSTESTCD == "OVRLRESP") %>%
  mutate(
    PARAMCD = "OVR",
    PARAM = "Overall Response by Investigator",
    PARCAT1 = "Tumor Response",
    PARCAT2 = "Investigator",
    PARCAT3 = "RECIST 1.1"
  )

# Date imputations - here we impute missing day to last possible date
adrs <- adrs %>%
  derive_vars_dt(
    dtc = RSDTC,
    new_vars_prefix = "A",
    highest_imputation = "D",
    date_imputation = "last"
  ) %>%
  mutate(AVISIT = VISIT)

# Set numeric analysis value - here RECIST 1.1 response values are expected
adrs <- adrs %>%
  mutate(
    AVALC = RSSTRESC,
    AVAL = aval_resp(AVALC)
  )

# Set analysis flag to include only the records that should contribute to the
# parameter derivations - here only valid assessments and those occurring on or
# after randomization date, if there is more than one assessment per date the
# worst one is flagged
worst_resp <- function(arg) {
  case_when(
    arg == "NE" ~ 1,
    arg == "CR" ~ 2,
    arg == "PR" ~ 3,
    arg == "SD" ~ 4,
    arg == "NON-CR/NON-PD" ~ 5,
    arg == "PD" ~ 6,
    TRUE ~ 0
  )
}

adrs <- adrs %>%
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = c(get_admiral_option("subject_keys"), exprs(ADT)),
      order = exprs(worst_resp(AVALC), RSSEQ),
      new_var = ANL01FL,
      mode = "last"
    ),
    filter = !is.na(AVAL) & ADT >= RANDDT
  )

# ---- Parameter derivations ----

# Progressive disease
adrs <- adrs %>%
  derive_extreme_records(
    dataset_ref = adsl,
    dataset_add = adrs,
    by_vars = get_admiral_option("subject_keys"),
    filter_add = PARAMCD == "OVR" & AVALC == "PD" & ANL01FL == "Y",
    order = exprs(ADT, RSSEQ),
    mode = "first",
    exist_flag = AVALC,
    false_value = "N",
    set_values_to = exprs(
      PARAMCD = "PD",
      PARAM = "Disease Progression by Investigator",
      PARCAT1 = "Tumor Response",
      PARCAT2 = "Investigator",
      PARCAT3 = "RECIST 1.1",
      AVAL = yn_to_numeric(AVALC),
      ANL01FL = "Y"
    )
  )

# Define the progressive disease source location
pd <- date_source(
  dataset_name = "adrs",
  date = ADT,
  filter = PARAMCD == "PD" & AVALC == "Y"
)

# Response
adrs <- adrs %>%
  derive_param_response(
    dataset_adsl = adsl,
    filter_source = PARAMCD == "OVR" & AVALC %in% c("CR", "PR") & ANL01FL == "Y",
    source_pd = pd,
    source_datasets = list(adrs = adrs),
    set_values_to = exprs(
      PARAMCD = "RSP",
      PARAM = "Response by Investigator (confirmation not required)",
      PARCAT1 = "Tumor Response",
      PARCAT2 = "Investigator",
      PARCAT3 = "RECIST 1.1",
      AVAL = yn_to_numeric(AVALC),
      ANL01FL = "Y"
    )
  )

# Define the response source location
resp <- date_source(
  dataset_name = "adrs",
  date = ADT,
  filter = PARAMCD == "RSP" & AVALC == "Y"
)

# Clinical benefit
adrs <- adrs %>%
  derive_param_clinbenefit(
    dataset_adsl = adsl,
    filter_source = PARAMCD == "OVR" & ANL01FL == "Y",
    source_resp = resp,
    source_pd = pd,
    source_datasets = list(adrs = adrs),
    reference_date = RANDDT,
    ref_start_window = 42,
    set_values_to = exprs(
      PARAMCD = "CB",
      PARAM = "Clinical Benefit by Investigator (confirmation for response not required)",
      PARCAT1 = "Tumor Response",
      PARCAT2 = "Investigator",
      PARCAT3 = "RECIST 1.1",
      AVAL = yn_to_numeric(AVALC),
      ANL01FL = "Y"
    )
  )

# Best overall response (without confirmation)
adrs <- adrs %>%
  derive_param_bor(
    dataset_adsl = adsl,
    filter_source = PARAMCD == "OVR" & ANL01FL == "Y",
    source_pd = pd,
    source_datasets = list(adrs = adrs),
    reference_date = RANDDT,
    ref_start_window = 42,
    set_values_to = exprs(
      PARAMCD = "BOR",
      PARAM = "Best Overall Response by Investigator (confirmation not required)",
      PARCAT1 = "Tumor Response",
      PARCAT2 = "Investigator",
      PARCAT3 = "RECIST 1.1",
      AVAL = aval_resp(AVALC),
      ANL01FL = "Y"
    )
  )

# Best overall response of CR/PR
adrs <- adrs %>%
  derive_extreme_records(
    dataset_ref = adsl,
    dataset_add = adrs,
    by_vars = get_admiral_option("subject_keys"),
    filter_add = PARAMCD == "BOR" & AVALC %in% c("CR", "PR"),
    order = exprs(ADT, RSSEQ),
    mode = "first",
    exist_flag = AVALC,
    false_value = "N",
    set_values_to = exprs(
      PARAMCD = "BCP",
      PARAM = "Best Overall Response of CR/PR by Investigator (confirmation not required)",
      PARCAT1 = "Tumor Response",
      PARCAT2 = "Investigator",
      PARCAT3 = "RECIST 1.1",
      AVAL = yn_to_numeric(AVALC),
      ANL01FL = "Y"
    )
  )

# Confirmed response versions of the above parameters
adrs <- adrs %>%
  derive_param_confirmed_resp(
    dataset_adsl = adsl,
    filter_source = PARAMCD == "OVR" & ANL01FL == "Y",
    source_pd = pd,
    source_datasets = list(adrs = adrs),
    ref_confirm = 28,
    set_values_to = exprs(
      PARAMCD = "CRSP",
      PARAM = "Confirmed Response by Investigator",
      PARCAT1 = "Tumor Response",
      PARCAT2 = "Investigator",
      PARCAT3 = "RECIST 1.1",
      AVAL = yn_to_numeric(AVALC),
      ANL01FL = "Y"
    )
  )

confirmed_resp <- date_source(
  dataset_name = "adrs",
  date = ADT,
  filter = PARAMCD == "CRSP" & AVALC == "Y"
)

adrs <- adrs %>%
  derive_param_clinbenefit(
    dataset_adsl = adsl,
    filter_source = PARAMCD == "OVR" & ANL01FL == "Y",
    source_resp = confirmed_resp,
    source_pd = pd,
    source_datasets = list(adrs = adrs),
    reference_date = RANDDT,
    ref_start_window = 42,
    set_values_to = exprs(
      PARAMCD = "CCB",
      PARAM = "Confirmed Clinical Benefit by Investigator",
      PARCAT1 = "Tumor Response",
      PARCAT2 = "Investigator",
      PARCAT3 = "RECIST 1.1",
      AVAL = yn_to_numeric(AVALC),
      ANL01FL = "Y"
    )
  ) %>%
  derive_param_confirmed_bor(
    dataset_adsl = adsl,
    filter_source = PARAMCD == "OVR" & ANL01FL == "Y",
    source_pd = pd,
    source_datasets = list(adrs = adrs),
    reference_date = RANDDT,
    ref_start_window = 42,
    ref_confirm = 28,
    set_values_to = exprs(
      PARAMCD = "CBOR",
      PARAM = "Best Confirmed Overall Response by Investigator",
      PARCAT1 = "Tumor Response",
      PARCAT2 = "Investigator",
      PARCAT3 = "RECIST 1.1",
      AVAL = aval_resp(AVALC),
      ANL01FL = "Y"
    )
  ) %>%
  derive_extreme_records(
    dataset_ref = adsl,
    dataset_add = adrs,
    by_vars = get_admiral_option("subject_keys"),
    filter_add = PARAMCD == "CBOR" & AVALC %in% c("CR", "PR"),
    order = exprs(ADT, RSSEQ),
    mode = "first",
    exist_flag = AVALC,
    false_value = "N",
    set_values_to = exprs(
      PARAMCD = "CBCP",
      PARAM = "Best Confirmed Overall Response of CR/PR by Investigator",
      PARCAT1 = "Tumor Response",
      PARCAT2 = "Investigator",
      PARCAT3 = "RECIST 1.1",
      AVAL = yn_to_numeric(AVALC),
      ANL01FL = "Y"
    )
  )

# Death
adsldth <- adsl %>%
  select(!!!get_admiral_option("subject_keys"), DTHDT, !!!adsl_vars)

adrs <- adrs %>%
  derive_extreme_records(
    dataset_ref = adsldth,
    dataset_add = adsldth,
    by_vars = get_admiral_option("subject_keys"),
    filter_add = !is.na(DTHDT),
    exist_flag = AVALC,
    false_value = "N",
    set_values_to = exprs(
      PARAMCD = "DEATH",
      PARAM = "Death",
      PARCAT1 = "Reference Event",
      ANL01FL = "Y",
      AVAL = yn_to_numeric(AVALC),
      ADT = DTHDT
    )
  ) %>%
  select(-DTHDT)

# Last disease assessment
adrs <- adrs %>%
  derive_extreme_records(
    dataset_ref = adsl,
    dataset_add = adrs,
    by_vars = get_admiral_option("subject_keys"),
    filter_add = PARAMCD == "OVR" & ANL01FL == "Y",
    order = exprs(ADT, RSSEQ),
    mode = "last",
    set_values_to = exprs(
      PARAMCD = "LSTA",
      PARAM = "Last Disease Assessment by Investigator",
      PARCAT1 = "Tumor Response",
      PARCAT2 = "Investigator",
      PARCAT3 = "RECIST 1.1",
      ANL01FL = "Y"
    )
  )

# Measurable disease at baseline
adslmdis <- adsl %>%
  select(!!!get_admiral_option("subject_keys"), !!!adsl_vars)

adrs <- adrs %>%
  derive_param_exist_flag(
    dataset_ref = adslmdis,
    dataset_add = tu,
    condition = TUEVAL == "INVESTIGATOR" & TUSTRESC == "TARGET" & VISIT == "SCREENING",
    false_value = "N",
    missing_value = "N",
    set_values_to = exprs(
      PARAMCD = "MDIS",
      PARAM = "Measurable Disease at Baseline by Investigator",
      PARCAT2 = "Investigator",
      PARCAT3 = "RECIST 1.1",
      AVAL = yn_to_numeric(AVALC),
      ANL01FL = "Y"
    )
  )

# Derive analysis sequence
adrs <- adrs %>%
  derive_var_obs_number(
    by_vars = get_admiral_option("subject_keys"),
    order = exprs(PARAMCD, ADT, VISITNUM, RSSEQ),
    check_type = "error"
  )

# Join any required ADSL variables
adrs <- adrs %>%
  derive_vars_merged(
    dataset_add = select(adsl, !!!negate_vars(adsl_vars)),
    by_vars = get_admiral_option("subject_keys")
  )

# ---- Save output ----
# Change to whichever directory you want to save the dataset in
dir <- tools::R_user_dir("admiralonco_templates_data", which = "cache")
if (!file.exists(dir)) {
  # Create the folder
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
}
save(adrs, file = file.path(dir, "adrs_basic.rda"), compress = "bzip2")
```

ad_adrs.R

```
# Name: ADRS
#
# Label: Response Analysis Dataset
#
# Input: adsl, rs, tu
library(admiral)
library(admiralonco)
library(pharmaversesdtm) # Contains example datasets from the CDISC pilot project
# pharmaverseadam contains example datasets generated from the CDISC pilot
# project SDTM ran through admiral templates
library(pharmaverseadam)
library(dplyr)
library(lubridate)
library(stringr)

# Load source datasets ----

# Use e.g. haven::read_sas to read in .sas7bdat, or other suitable functions
# as needed and assign to the variables below.
# For illustration purposes read in pharmaverse test data

data("adsl")
data("rs_onco_recist")
data("tu_onco_recist")

rs <- rs_onco_recist
tu <- tu_onco_recist

rs <- convert_blanks_to_na(rs)
tu <- convert_blanks_to_na(tu)

# Derivations ----

# Get list of ADSL vars required for derivations - here we assume randomized study
adsl_vars <- exprs(RANDDT)

# Join ADSL vars to RS
adrs <- rs %>%
  derive_vars_merged(
    dataset_add = adsl,
    new_vars = adsl_vars,
    by_vars = get_admiral_option("subject_keys")
  )

# Company-specific pre-processing ----

# Filtering to select Overall Response records - here we used Investigator records
# but all these steps could equally be repeated for Independent Review Facility
adrs <- adrs %>%
  filter(RSEVAL == "INVESTIGATOR" & RSTESTCD == "OVRLRESP") %>%
  mutate(
    PARAMCD = "OVR",
    PARAM = "Overall Response by Investigator",
    PARCAT1 = "Tumor Response",
    PARCAT2 = "Investigator",
    PARCAT3 = "RECIST 1.1"
  )

# Date imputations - here we impute missing day to last possible date
adrs <- adrs %>%
  derive_vars_dt(
    dtc = RSDTC,
    new_vars_prefix = "A",
    highest_imputation = "D",
    date_imputation = "last"
  ) %>%
  mutate(AVISIT = VISIT)

# Set numeric analysis value - here RECIST 1.1 response values are expected
adrs <- adrs %>%
  mutate(
    AVALC = RSSTRESC,
    AVAL = aval_resp(AVALC)
  )

# Set analysis flag to include only the records that should contribute to the
# parameter derivations - here only valid assessments and those occurring on or
# after randomization date, if there is more than one assessment per date the
# worst one is flagged
worst_resp <- function(arg) {
  case_when(
    arg == "NE" ~ 1,
    arg == "CR" ~ 2,
    arg == "PR" ~ 3,
    arg == "SD" ~ 4,
    arg == "NON-CR/NON-PD" ~ 5,
    arg == "PD" ~ 6,
    TRUE ~ 0
  )
}

adrs <- adrs %>%
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = c(get_admiral_option("subject_keys"), exprs(ADT)),
      order = exprs(worst_resp(AVALC), RSSEQ),
      new_var = ANL01FL,
      mode = "last"
    ),
    filter = !is.na(AVAL) & ADT >= RANDDT
  ) %>%
  derive_var_relative_flag(
    by_vars = get_admiral_option("subject_keys"),
    order = exprs(ADT, RSSEQ),
    new_var = ANL02FL,
    condition = AVALC == "PD",
    mode = "first",
    selection = "before",
    inclusive = TRUE
  )

# Create dataset with overall responses to be used for deriving parameters
ovr <- filter(adrs, PARAMCD == "OVR" & ANL01FL == "Y" & ANL02FL == "Y")

# Parameter derivations ----

## Define events ----
# These events are just examples showing how to define the ADSL variables to keep.
# More may need to be added depending on the study needs, e.g., for adjusting
# confirmation period.
no_data_n <- event(
  description = "Define no response for all patients in adsl (should be used as last event)",
  dataset_name = "adsl",
  condition = TRUE,
  set_values_to = exprs(AVALC = "N"),
  keep_source_vars = adsl_vars
)

no_data_missing <- event(
  description = paste(
    "Define missing response (MISSING) for all patients in adsl (should be used",
    "as last event)"
  ),
  dataset_name = "adsl",
  condition = TRUE,
  set_values_to = exprs(AVALC = "MISSING"),
  keep_source_vars = adsl_vars
)

## Progressive disease ----
adrs <- adrs %>%
  derive_extreme_records(
    dataset_ref = adsl,
    dataset_add = adrs,
    by_vars = get_admiral_option("subject_keys"),
    filter_add = PARAMCD == "OVR" & AVALC == "PD" & ANL01FL == "Y",
    order = exprs(ADT, RSSEQ),
    mode = "first",
    exist_flag = AVALC,
    false_value = "N",
    set_values_to = exprs(
      PARAMCD = "PD",
      PARAM = "Disease Progression by Investigator",
      PARCAT1 = "Tumor Response",
      PARCAT2 = "Investigator",
      PARCAT3 = "RECIST 1.1",
      AVAL = yn_to_numeric(AVALC),
      ANL01FL = "Y"
    )
  )

## Response ----
adrs <- adrs %>%
  derive_extreme_event(
    by_vars = get_admiral_option("subject_keys"),
    order = exprs(event_nr, ADT),
    tmp_event_nr_var = event_nr,
    mode = "first",
    events = list(rsp_y, no_data_n),
    source_datasets = list(
      ovr = ovr,
      adsl = adsl
    ),
    set_values_to = exprs(
      PARAMCD = "RSP",
      PARAM = "Response by Investigator (confirmation not required)",
      PARCAT1 = "Tumor Response",
      PARCAT2 = "Investigator",
      PARCAT3 = "RECIST 1.1",
      AVAL = yn_to_numeric(AVALC),
      ANL01FL = "Y"
    )
  )

## Clinical benefit ----
adrs <- adrs %>%
  derive_extreme_event(
    by_vars = get_admiral_option("subject_keys"),
    order = exprs(desc(AVALC), ADT, event_nr),
    tmp_event_nr_var = event_nr,
    mode = "first",
    events = list(rsp_y, cb_y, no_data_n),
    source_datasets = list(
      ovr = ovr,
      adsl = adsl
    ),
    set_values_to = exprs(
      PARAMCD = "CB",
      PARAM = "Clinical Benefit by Investigator (confirmation for response not required)",
      PARCAT1 = "Tumor Response",
      PARCAT2 = "Investigator",
      PARCAT3 = "RECIST 1.1",
      AVAL = yn_to_numeric(AVALC),
      ANL01FL = "Y"
    ),
    check_type = "none"
  )

## Best overall response (without confirmation) ----
# Please note that the order of the events specified for `events` is important.
# For example, a subject with `PR`, `PR`, `CR` qualifies for both `bor_cr` and
# `bor_pr`. As `bor_cr` is listed before `bor_pr`, CR is selected as best overall
# response for this subject.
adrs <- adrs %>%
  derive_extreme_event(
    by_vars = get_admiral_option("subject_keys"),
    order = exprs(event_nr, ADT),
    tmp_event_nr_var = event_nr,
    mode = "first",
    source_datasets = list(
      ovr = ovr,
      adsl = adsl
    ),
    events = list(bor_cr, bor_pr, bor_sd, bor_non_crpd, bor_pd, bor_ne, no_data_missing),
    set_values_to = exprs(
      PARAMCD = "BOR",
      PARAM = "Best Overall Response by Investigator (confirmation not required)",
      PARCAT1 = "Tumor Response",
      PARCAT2 = "Investigator",
      PARCAT3 = "RECIST 1.1",
      AVAL = aval_resp(AVALC),
      ANL01FL = "Y"
    )
  )

## Best overall response of CR/PR ----
adrs <- adrs %>%
  derive_extreme_records(
    dataset_ref = adsl,
    dataset_add = adrs,
    by_vars = get_admiral_option("subject_keys"),
    filter_add = PARAMCD == "BOR" & AVALC %in% c("CR", "PR"),
    exist_flag = AVALC,
    false_value = "N",
    set_values_to = exprs(
      PARAMCD = "BCP",
      PARAM = "Best Overall Response of CR/PR by Investigator (confirmation not required)",
      PARCAT1 = "Tumor Response",
      PARCAT2 = "Investigator",
      PARCAT3 = "RECIST 1.1",
      AVAL = yn_to_numeric(AVALC),
      ANL01FL = "Y"
    )
  )

## Confirmed response versions of the above parameters ----
adrs <- adrs %>%
  derive_extreme_event(
    by_vars = get_admiral_option("subject_keys"),
    order = exprs(desc(AVALC), ADT, event_nr),
    tmp_event_nr_var = event_nr,
    mode = "first",
    source_datasets = list(
      ovr = ovr,
      adsl = adsl
    ),
    events = list(crsp_y_cr, crsp_y_pr, no_data_n),
    set_values_to = exprs(
      PARAMCD = "CRSP",
      PARAM = "Confirmed Response by Investigator",
      PARCAT1 = "Tumor Response",
      PARCAT2 = "Investigator",
      PARCAT3 = "RECIST 1.1",
      AVAL = yn_to_numeric(AVALC),
      ANL01FL = "Y"
    )
  )

adrs <- adrs %>%
  derive_extreme_event(
    by_vars = get_admiral_option("subject_keys"),
    order = exprs(desc(AVALC), ADT, event_nr),
    tmp_event_nr_var = event_nr,
    mode = "first",
    events = list(crsp_y_cr, crsp_y_pr, cb_y, no_data_n),
    source_datasets = list(
      ovr = ovr,
      adsl = adsl
    ),
    set_values_to = exprs(
      PARAMCD = "CCB",
      PARAM = "Confirmed Clinical Benefit by Investigator",
      PARCAT1 = "Tumor Response",
      PARCAT2 = "Investigator",
      PARCAT3 = "RECIST 1.1",
      AVAL = yn_to_numeric(AVALC),
      ANL01FL = "Y"
    )
  )

adrs <- adrs %>%
  derive_extreme_event(
    by_vars = get_admiral_option("subject_keys"),
    order = exprs(event_nr, ADT),
    tmp_event_nr_var = event_nr,
    mode = "first",
    events = list(cbor_cr, cbor_pr, bor_sd, bor_non_crpd, bor_pd, bor_ne, no_data_missing),
    source_datasets = list(
      ovr = ovr,
      adsl = adsl
    ),
    set_values_to = exprs(
      PARAMCD = "CBOR",
      PARAM = "Best Confirmed Overall Response by Investigator",
      PARCAT1 = "Tumor Response",
      PARCAT2 = "Investigator",
      PARCAT3 = "RECIST 1.1",
      AVAL = aval_resp(AVALC),
      ANL01FL = "Y"
    )
  ) %>%
  derive_extreme_records(
    dataset_ref = adsl,
    dataset_add = adrs,
    by_vars = get_admiral_option("subject_keys"),
    filter_add = PARAMCD == "CBOR" & AVALC %in% c("CR", "PR"),
    exist_flag = AVALC,
    false_value = "N",
    set_values_to = exprs(
      PARAMCD = "CBCP",
      PARAM = "Best Confirmed Overall Response of CR/PR by Investigator",
      PARCAT1 = "Tumor Response",
      PARCAT2 = "Investigator",
      PARCAT3 = "RECIST 1.1",
      AVAL = yn_to_numeric(AVALC),
      ANL01FL = "Y"
    )
  )

## Death ----
adsldth <- adsl %>%
  select(!!!get_admiral_option("subject_keys"), DTHDT, !!!adsl_vars)

adrs <- adrs %>%
  derive_extreme_records(
    dataset_ref = adsldth,
    dataset_add = adsldth,
    by_vars = get_admiral_option("subject_keys"),
    filter_add = !is.na(DTHDT),
    exist_flag = AVALC,
    false_value = "N",
    set_values_to = exprs(
      PARAMCD = "DEATH",
      PARAM = "Death",
      PARCAT1 = "Reference Event",
      ANL01FL = "Y",
      AVAL = yn_to_numeric(AVALC),
      ADT = DTHDT
    )
  ) %>%
  select(-DTHDT)

## Last disease assessment ----
adrs <- adrs %>%
  derive_extreme_records(
    dataset_ref = adsl,
    dataset_add = adrs,
    by_vars = get_admiral_option("subject_keys"),
    filter_add = PARAMCD == "OVR" & ANL01FL == "Y",
    order = exprs(ADT, RSSEQ),
    mode = "last",
    set_values_to = exprs(
      PARAMCD = "LSTA",
      PARAM = "Last Disease Assessment by Investigator",
      PARCAT1 = "Tumor Response",
      PARCAT2 = "Investigator",
      PARCAT3 = "RECIST 1.1",
      ANL01FL = "Y"
    )
  )

## Measurable disease at baseline ----
adslmdis <- adsl %>%
  select(!!!get_admiral_option("subject_keys"), !!!adsl_vars)

adrs <- adrs %>%
  derive_param_exist_flag(
    dataset_ref = adslmdis,
    dataset_add = tu,
    condition = TUEVAL == "INVESTIGATOR" & TUSTRESC == "TARGET" & VISIT == "SCREENING",
    false_value = "N",
    missing_value = "N",
    set_values_to = exprs(
      PARAMCD = "MDIS",
      PARAM = "Measurable Disease at Baseline by Investigator",
      PARCAT2 = "Investigator",
      PARCAT3 = "RECIST 1.1",
      AVAL = yn_to_numeric(AVALC),
      ANL01FL = "Y"
    )
  )

# Derive analysis sequence
adrs <- adrs %>%
  derive_var_obs_number(
    by_vars = get_admiral_option("subject_keys"),
    order = exprs(PARAMCD, ADT, VISITNUM, RSSEQ),
    check_type = "error"
  )

# Join any required ADSL variables
adrs <- adrs %>%
  derive_vars_merged(
    dataset_add = select(adsl, !!!negate_vars(adsl_vars)),
    by_vars = get_admiral_option("subject_keys")
  )

# Save output ----

# Change to whichever directory you want to save the dataset in
dir <- tools::R_user_dir("admiralonco_templates_data", which = "cache")
if (!file.exists(dir)) {
  # Create the folder
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
}
save(adrs, file = file.path(dir, "adrs.rda"), compress = "bzip2")
```

ad_adtr.R

```
# Name: ADTR
#
# Label: Tumor Response Analysis Dataset
#
# Input: adsl, rs, tr, tu
library(admiral)
library(admiralonco)
library(pharmaversesdtm) # Contains example datasets from the CDISC pilot project
# pharmaverseadam contains example datasets generated from the CDISC pilot
# project SDTM ran through admiral templates
library(pharmaverseadam)
library(dplyr)
library(lubridate)
library(stringr)

# Load source datasets ----

# Use e.g. haven::read_sas to read in .sas7bdat, or other suitable functions
# as needed and assign to the variables below.
# For illustration purposes read in pharmaverse test data
data("adsl")
data("rs_onco_recist")
data("tu_onco_recist")
data("tr_onco_recist")
tu <- tu_onco_recist
tr <- tr_onco_recist
rs <- rs_onco_recist

tu <- convert_blanks_to_na(tu) %>%
  filter(TUEVAL == "INVESTIGATOR")
tr <- convert_blanks_to_na(tr) %>%
  filter(
    TREVAL == "INVESTIGATOR" & TRGRPID == "TARGET" & TRTESTCD %in% c("LDIAM", "LPERP")
  )
rs <- convert_blanks_to_na(rs)

# Get list of ADSL vars required for derivations
adsl_vars <- exprs(RANDDT)

# Join ADSL vars to TR
tr <- derive_vars_merged(
  tr,
  dataset_add = adsl,
  new_vars = adsl_vars,
  by_vars = get_admiral_option("subject_keys")
)

# Add variables from TU (location of tumor) ----
tr <- derive_vars_merged(
  tr,
  dataset_add = tu,
  new_vars = exprs(TULOC),
  by_vars = c(get_admiral_option("subject_keys"), exprs(TRLNKID = TULNKID))
) %>% mutate(
  TULOCGR1 = if_else(
    TULOC == "LYMPH NODE",
    "NODAL",
    "NON-NODAL"
  )
)

tr <- mutate(
  tr,
  LSEXP = TRLNKID,
  LSASS = if_else(!is.na(TRSTRESN), TRLNKID, NA_character_)
)

# Derive timing variables (ADT, ADY, AVISIT, AVISITN) ----
tr <- derive_vars_dt(
  tr,
  dtc = TRDTC,
  new_vars_prefix = "A",
  highest_imputation = "D",
  date_imputation = "first"
) %>%
  derive_vars_dy(
    reference_date = RANDDT,
    source_vars = exprs(ADT)
  ) %>%
  mutate(
    AVISIT = if_else(
      VISIT == "SCREENING",
      "BASELINE",
      VISIT
    ),
    AVISITN = if_else(
      AVISIT == "BASELINE",
      0,
      VISITNUM
    )
  )

# Derive parameters for lesion diameters (LDIAMn & NLDIAMn) ----
tr <- mutate(tr, tmp_lesion_nr = str_sub(TRLNKID, 3))
adtr <- bind_rows(
  tr %>%
    filter(TRTESTCD == "LDIAM") %>%
    mutate(
      PARAMCD = paste0("LDIAM", tmp_lesion_nr),
      PARAM = paste("Target Lesion", tmp_lesion_nr, "Analysis Diameter")
    ),
  tr %>%
    filter(TRTESTCD == "LPERP") %>%
    mutate(
      PARAMCD = paste0("NLDIAM", tmp_lesion_nr),
      PARAM = paste("Target Lesion", tmp_lesion_nr, "Analysis Perpendicular")
    )
) %>%
  mutate(
    PARCAT1 = "Target Lesion(s)",
    PARCAT2 = "Investigator",
    PARCAT3 = "RECIST 1.1",
    AVAL = TRSTRESN,
    ANL01FL = if_else(!is.na(AVAL), "Y", NA_character_)
  ) %>%
  select(-tmp_lesion_nr)

# Derive parameter for sum of diameter (SDIAM) ----
adtr_sum <- derive_summary_records(
  dataset_add = adtr,
  by_vars = exprs(!!!get_admiral_option("subject_keys"), !!!adsl_vars, AVISIT, AVISITN),
  filter_add = (str_starts(PARAMCD, "LDIAM") & TULOCGR1 == "NON-NODAL") |
    (str_starts(PARAMCD, "NLDIAM") & TULOCGR1 == "NODAL"),
  set_values_to = exprs(
    AVAL = sum(AVAL, na.rm = TRUE),
    ADY = min(ADY, na.rm = TRUE),
    ADT = min(ADT, na.rm = TRUE),
    PARAMCD = "SDIAM",
    PARAM = "Target Lesions Sum of Diameters by Investigator",
    PARCAT1 = "Target Lesion(s)",
    PARCAT2 = "Investigator",
    PARCAT3 = "RECIST 1.1"
  )
)

# Derive analysis flag (ANL01FL) ----
adtr_sum <- adtr_sum %>%
  derive_var_merged_summary(
    dataset_add = adtr,
    by_vars = get_admiral_option("subject_keys"),
    filter_add = AVISIT == "BASELINE" &
      ((str_starts(PARAMCD, "LDIAM") & TULOCGR1 == "NON-NODAL") |
        (str_starts(PARAMCD, "NLDIAM") & TULOCGR1 == "NODAL")),
    new_vars = exprs(LSEXP = paste(sort(TRLNKID), collapse = ", "))
  ) %>%
  derive_var_merged_summary(
    dataset_add = adtr,
    by_vars = c(get_admiral_option("subject_keys"), exprs(AVISIT)),
    filter_add = ((str_starts(PARAMCD, "LDIAM") & TULOCGR1 == "NON-NODAL") |
      (str_starts(PARAMCD, "NLDIAM") & TULOCGR1 == "NODAL")) & ANL01FL == "Y",
    new_vars = exprs(LSASS = paste(sort(TRLNKID), collapse = ", "))
  ) %>%
  mutate(
    ANL01FL = if_else(LSEXP == LSASS, "Y", NA_character_)
  )

# Derive baseline (ABLFL, BASE) ----
adtr_sum <- adtr_sum %>%
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = get_admiral_option("subject_keys"),
      order = exprs(ADY),
      new_var = ABLFL,
      mode = "last"
    ),
    filter = ADY <= 1
  ) %>%
  derive_var_base(
    by_vars = get_admiral_option("subject_keys")
  )

# Derive nadir (NADIR) ----
adtr_sum <- adtr_sum %>%
  derive_vars_joined(
    dataset_add = adtr_sum,
    by_vars = get_admiral_option("subject_keys"),
    order = exprs(AVAL),
    new_vars = exprs(NADIR = AVAL),
    join_vars = exprs(ADY),
    join_type = "all",
    filter_add = ANL01FL == "Y",
    filter_join = ADY.join < ADY,
    mode = "first",
    check_type = "none"
  )

# Derive change from baseline/nadir (CHG, PCHG, CHGNAD, PCHGNAD) ----
adtr_sum <- adtr_sum %>%
  derive_var_chg() %>%
  derive_var_pchg() %>%
  mutate(
    CHGNAD = AVAL - NADIR,
    PCHGNAD = if_else(NADIR == 0, NA_real_, 100 * CHGNAD / NADIR)
  )

# Derive additional flag variables (PDFL) ----
adtr_sum <- adtr_sum %>%
  derive_var_merged_exist_flag(
    dataset_add = derive_vars_dt(
      rs,
      dtc = RSDTC,
      new_vars_prefix = "A",
      highest_imputation = "D",
      flag_imputation = "none"
    ),
    filter_add = RSTESTCD == "OVRLRESP" & RSEVAL == "INVESTIGATOR",
    by_vars = c(get_admiral_option("subject_keys"), exprs(ADT)),
    new_var = PDFL,
    condition = RSSTRESC == "PD"
  )

# Derive analysis flags (ANLzzFL) ----
adtr_sum <- adtr_sum %>%
  derive_var_relative_flag(
    by_vars = get_admiral_option("subject_keys"),
    order = exprs(ADT),
    new_var = POSTRNDFL,
    condition = ADT > RANDDT,
    mode = "first",
    selection = "after",
    inclusive = TRUE,
    flag_no_ref_groups = FALSE
  ) %>%
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = get_admiral_option("subject_keys"),
      new_var = ANL02FL,
      order = exprs(PCHG),
      mode = "first",
      check_type = "none"
    ),
    filter = ANL01FL == "Y" & POSTRNDFL == "Y"
  ) %>%
  select(-POSTRNDFL) %>%
  restrict_derivation(
    derivation = derive_var_relative_flag,
    args = params(
      by_vars = get_admiral_option("subject_keys"),
      new_var = ANL03FL,
      condition = PDFL == "Y",
      order = exprs(ADY),
      mode = "first",
      selection = "before",
      inclusive = FALSE
    ),
    filter = ANL01FL == "Y" | PDFL == "Y"
  ) %>%
  mutate(
    ANL04FL = if_else(ANL01FL == "Y" | PDFL == "Y", "Y", NA_character_)
  )

# Derive analysis sequence number (ASEQ) ----
adtr <- adtr %>%
  bind_rows(adtr_sum) %>%
  derive_var_obs_number(
    by_vars = get_admiral_option("subject_keys"),
    order = exprs(PARAMCD, AVISITN, TRSEQ),
    check_type = "error"
  )

# Add ADSL variables ----
adtr <- adtr %>%
  derive_vars_merged(
    dataset_add = select(adsl, !!!negate_vars(adsl_vars)),
    by_vars = get_admiral_option("subject_keys")
  )

# Change to whichever directory you want to save the dataset in
dir <- tools::R_user_dir("admiralonco_templates_data", which = "cache")
if (!file.exists(dir)) {
  # Create the folder
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
}
save(adtr, file = file.path(dir, "adtr.rda"), compress = "bzip2")
```

ad_adtte.R

```
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
    by_vars = get_admiral_option("subject_keys"),
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
    by_vars = get_admiral_option("subject_keys"),
    order = exprs(PARAMCD),
    check_type = "error"
  )

# Join any required ADSL variables
adtte <- adtte %>%
  derive_vars_merged(
    dataset_add = adsl,
    new_vars = exprs(ARMCD, ARM, ACTARMCD, ACTARM, AGE, SEX),
    by_vars = get_admiral_option("subject_keys")
  )

# Save output ----

# Change to whichever directory you want to save the dataset in
dir <- tools::R_user_dir("admiralonco_templates_data", which = "cache")
if (!file.exists(dir)) {
  # Create the folder
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
}
save(adtte, file = file.path(dir, "adtte.rda"), compress = "bzip2")
```
