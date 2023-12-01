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
library(dplyr)
library(lubridate)
library(stringr)

# ---- Load source datasets ----

# Use e.g. haven::read_sas to read in .sas7bdat, or other suitable functions
# as needed and assign to the variables below.
# For illustration purposes read in pharmaverse test data

data("admiral_adsl")
data("rs_onco_recist")
data("tu_onco_recist")

adsl <- admiral_adsl
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
    by_vars = exprs(STUDYID, USUBJID)
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
    PARCAT3 = "Recist 1.1"
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
adrs <- adrs %>%
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = exprs(STUDYID, USUBJID, ADT),
      order = exprs(AVAL, RSSEQ),
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
    by_vars = exprs(STUDYID, USUBJID),
    filter_add = PARAMCD == "OVR" & AVALC == "PD" & ANL01FL == "Y",
    order = exprs(ADT, RSSEQ),
    mode = "first",
    exist_flag = AVALC,
    set_values_to = exprs(
      PARAMCD = "PD",
      PARAM = "Disease Progression by Investigator",
      PARCAT1 = "Tumor Response",
      PARCAT2 = "Investigator",
      PARCAT3 = "Recist 1.1",
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
      PARCAT3 = "Recist 1.1",
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
      PARCAT3 = "Recist 1.1",
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
      PARCAT3 = "Recist 1.1",
      AVAL = aval_resp(AVALC),
      ANL01FL = "Y"
    )
  )

# Best overall response of CR/PR
adrs <- adrs %>%
  derive_extreme_records(
    dataset_ref = adsl,
    dataset_add = adrs,
    by_vars = exprs(STUDYID, USUBJID),
    filter_add = PARAMCD == "BOR" & AVALC %in% c("CR", "PR"),
    order = exprs(ADT, RSSEQ),
    mode = "first",
    exist_flag = AVALC,
    set_values_to = exprs(
      PARAMCD = "BCP",
      PARAM = "Best Overall Response of CR/PR by Investigator (confirmation not required)",
      PARCAT1 = "Tumor Response",
      PARCAT2 = "Investigator",
      PARCAT3 = "Recist 1.1",
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
      PARCAT3 = "Recist 1.1",
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
      PARCAT3 = "Recist 1.1",
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
      PARCAT3 = "Recist 1.1",
      AVAL = aval_resp(AVALC),
      ANL01FL = "Y"
    )
  ) %>%
  derive_extreme_records(
    dataset_ref = adsl,
    dataset_add = adrs,
    by_vars = exprs(STUDYID, USUBJID),
    filter_add = PARAMCD == "CBOR" & AVALC %in% c("CR", "PR"),
    order = exprs(ADT, RSSEQ),
    mode = "first",
    exist_flag = AVALC,
    set_values_to = exprs(
      PARAMCD = "CBCP",
      PARAM = "Best Confirmed Overall Response of CR/PR by Investigator",
      PARCAT1 = "Tumor Response",
      PARCAT2 = "Investigator",
      PARCAT3 = "Recist 1.1",
      AVAL = yn_to_numeric(AVALC),
      ANL01FL = "Y"
    )
  )

# Death
adsldth <- adsl %>%
  select(STUDYID, USUBJID, DTHDT, !!!adsl_vars)

adrs <- adrs %>%
  derive_extreme_records(
    dataset_ref = adsldth,
    dataset_add = adsldth,
    by_vars = exprs(STUDYID, USUBJID),
    filter_add = !is.na(DTHDT),
    exist_flag = AVALC,
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
    by_vars = exprs(STUDYID, USUBJID),
    filter_add = PARAMCD == "OVR" & ANL01FL == "Y",
    order = exprs(ADT, RSSEQ),
    mode = "last",
    set_values_to = exprs(
      PARAMCD = "LSTA",
      PARAM = "Last Disease Assessment by Investigator",
      PARCAT1 = "Tumor Response",
      PARCAT2 = "Investigator",
      PARCAT3 = "Recist 1.1",
      ANL01FL = "Y"
    )
  )

# Measurable disease at baseline
adslmdis <- adsl %>%
  select(STUDYID, USUBJID, !!!adsl_vars)

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
      PARCAT3 = "Recist 1.1",
      AVAL = yn_to_numeric(AVALC),
      ANL01FL = "Y"
    )
  )

# Derive analysis sequence
adrs <- adrs %>%
  derive_var_obs_number(
    by_vars = exprs(STUDYID, USUBJID),
    order = exprs(PARAMCD, ADT, VISITNUM, RSSEQ),
    check_type = "error"
  )

# Join any required ADSL variables
adrs <- adrs %>%
  derive_vars_merged(
    dataset_add = select(adsl, !!!negate_vars(adsl_vars)),
    by_vars = exprs(STUDYID, USUBJID)
  )

# ---- Save output ----
# Change to whichever directory you want to save the dataset in
dir <- tools::R_user_dir("admiralonco_templates_data", which = "cache")
if (!file.exists(dir)) {
  # Create the folder
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
}
save(adrs, file = file.path(dir, "adrs_basic.rda"), compress = "bzip2")
