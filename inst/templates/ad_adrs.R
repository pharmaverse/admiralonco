# Name: ADRS
#
# Label: Response Analysis Dataset
#
# Input: adsl, rs, tu
library(admiral)
library(admiralonco)
library(pharmaversesdtm) # Contains example datasets from the CDISC pilot project
library(dplyr)
library(lubridate)
library(stringr)

# Load source datasets ----

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

# Derivations ----

# Get list of ADSL vars required for derivations - here we assume randomized study
adsl_vars <- exprs(RANDDT)

# Join ADSL vars to RS
adrs <- rs %>%
  derive_vars_merged(
    dataset_add = adsl,
    new_vars = adsl_vars,
    by_vars = exprs(STUDYID, USUBJID)
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
  ) %>%
  derive_var_relative_flag(
    by_vars = exprs(STUDYID, USUBJID),
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
  set_values_to = exprs(
    AVALC = "MISSING"
  ),
  keep_source_vars = adsl_vars
)

## Progressive disease ----
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

## Response ----
adrs <- adrs %>%
  derive_extreme_event(
    by_vars = exprs(STUDYID, USUBJID),
    order = exprs(ADT),
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
      PARCAT3 = "Recist 1.1",
      AVAL = yn_to_numeric(AVALC),
      ANL01FL = "Y"
    )
  )

## Clinical benefit ----
adrs <- adrs %>%
  derive_extreme_event(
    by_vars = exprs(STUDYID, USUBJID),
    order = exprs(desc(AVALC), ADT, PARAMCD),
    mode = "first",
    events = list(
      event(
        condition = PARAMCD == "RSP" & AVALC == "Y",
        set_values_to = exprs(AVALC = "Y")
      ),
      cb_y,
      no_data_n
    ),
    source_datasets = list(
      ovr = ovr,
      adsl = adsl
    ),
    ignore_event_order = TRUE,
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

## Best overall response (without confirmation) ----
# Please note that the order of the events specified for `events` is important.
# For example, a subject with `PR`, `PR`, `CR` qualifies for both `bor_cr` and
# `bor_pr`. As `bor_cr` is listed before `bor_pr`, CR is selected as best overall
# response for this subject.
adrs <- adrs %>%
  derive_extreme_event(
    by_vars = exprs(STUDYID, USUBJID),
    order = exprs(ADT),
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
      PARCAT3 = "Recist 1.1",
      AVAL = aval_resp(AVALC),
      ANL01FL = "Y"
    )
  )

## Best overall response of CR/PR ----
adrs <- adrs %>%
  derive_extreme_records(
    dataset_ref = adsl,
    dataset_add = adrs,
    by_vars = exprs(STUDYID, USUBJID),
    filter_add = PARAMCD == "BOR" & AVALC %in% c("CR", "PR"),
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

## Confirmed response versions of the above parameters ----
adrs <- adrs %>%
  derive_extreme_event(
    by_vars = exprs(STUDYID, USUBJID),
    order = exprs(desc(AVALC), ADT),
    mode = "first",
    source_datasets = list(
      ovr = ovr,
      adsl = adsl
    ),
    events = list(crsp_y_cr, crsp_y_pr, no_data_n),
    ignore_event_order = TRUE,
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

adrs <- adrs %>%
  derive_extreme_event(
    by_vars = exprs(STUDYID, USUBJID),
    order = exprs(desc(AVALC), ADT, PARAMCD),
    mode = "first",
    events = list(
      event(
        condition = PARAMCD == "CRSP" & AVALC == "Y",
        set_values_to = exprs(AVALC = "Y")
      ),
      cb_y,
      no_data_n
    ),
    source_datasets = list(
      ovr = ovr,
      adsl = adsl
    ),
    ignore_event_order = TRUE,
    set_values_to = exprs(
      PARAMCD = "CCB",
      PARAM = "Confirmed Clinical Benefit by Investigator",
      PARCAT1 = "Tumor Response",
      PARCAT2 = "Investigator",
      PARCAT3 = "Recist 1.1",
      AVAL = yn_to_numeric(AVALC),
      ANL01FL = "Y"
    )
  )

adrs <- adrs %>%
  derive_extreme_event(
    by_vars = exprs(STUDYID, USUBJID),
    order = exprs(ADT),
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

## Death ----
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

## Last disease assessment ----
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

## Measurable disease at baseline ----
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

# Save output ----

dir <- tempdir() # Change to whichever directory you want to save the dataset in
saveRDS(adrs, file = file.path(dir, "adrs.rds"), compress = "bzip2")
