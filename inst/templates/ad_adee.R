# Name: ADEE
#
# Label: Exposure Efficacy Response Data
#
# Input: adsl, adtte, adlb, advs, adex, adpp
# Load required packages
library(admiral)
library(admiralonco)
# pharmaverseadam contains example datasets generated from the CDISC pilot
# project SDTM ran through admiral templates
library(pharmaverseadam)
library(dplyr)
library(lubridate)
library(stringr)

# Load source datasets ----

# Use e.g. haven::read_sas to read in .sas7bdat, or other suitable functions
# as needed and assign to the variables below.
# For illustration purposes read in admiral test data

adsl <- pharmaverseadam::adsl
adtte <- pharmaverseadam::adtte_onco
adlb <- pharmaverseadam::adlb
advs <- pharmaverseadam::advs
adpp <- pharmaverseadam::adpp

# ---- Prepare adsl - add derived variables

# Ensure TRT01P/TRT01A exist
if (!"TRT01P" %in% names(adsl)) {
  adsl <- adsl %>% mutate(TRT01P = ARM)
}
if (!"TRT01A" %in% names(adsl)) {
  adsl <- adsl %>% mutate(TRT01A = ACTARM)
}

# Create numeric treatment variables
adsl <- adsl %>%
  mutate(
    TRT01PN = case_when(
      TRT01P == "Placebo" ~ 0,
      TRT01P == "Xanomeline Low Dose" ~ 1,
      TRT01P == "Xanomeline High Dose" ~ 2,
      TRUE ~ 3
    ),
    TRT01AN = case_when(
      TRT01A == "Placebo" ~ 0,
      TRT01A == "Xanomeline Low Dose" ~ 1,
      TRT01A == "Xanomeline High Dose" ~ 2,
      TRUE ~ 3
    )
  )

# Ensure PARAMN exists in ADTTE
if (!"PARAMN" %in% names(adtte)) {
  adtte <- adtte %>%
    mutate(
      PARAMN = case_when(
        PARAMCD == "PFS" ~ 1,
        PARAMCD == "OS" ~ 2,
        PARAMCD == "TTP" ~ 3,
        PARAMCD == "TTNT" ~ 4,
        TRUE ~ 99
      )
    )
}

# ---- Derive Baseline Covariates

## Numeric identifiers and demographics ----

adsl_cov <- adsl %>%
  mutate(
    # Study identifiers (numeric)
    STUDYIDN = as.numeric(word(USUBJID, 1, sep = fixed("-"))),
    SITEIDN = as.numeric(word(USUBJID, 2, sep = fixed("-"))),
    USUBJIDN = as.numeric(word(USUBJID, 3, sep = fixed("-"))),
    SUBJIDN = as.numeric(SUBJID),

    # Demographics (numeric)
    SEXN = case_when(SEX == "M" ~ 1, SEX == "F" ~ 2, TRUE ~ 3),
    RACEN = case_when(
      RACE == "AMERICAN INDIAN OR ALASKA NATIVE" ~ 1,
      RACE == "ASIAN" ~ 2,
      RACE == "BLACK OR AFRICAN AMERICAN" ~ 3,
      RACE == "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER" ~ 4,
      RACE == "WHITE" ~ 5,
      TRUE ~ 6
    ),
    ETHNICN = case_when(
      ETHNIC == "HISPANIC OR LATINO" ~ 1,
      ETHNIC == "NOT HISPANIC OR LATINO" ~ 2,
      TRUE ~ 3
    ),

    # Age groups
    AGEGR1 = case_when(
      AGE < 65 ~ "<65",
      AGE >= 65 & AGE < 75 ~ "65-75",
      AGE >= 75 ~ ">75",
      TRUE ~ NA_character_
    ),
    AGEGR1N = case_when(
      AGE < 65 ~ 1,
      AGE >= 65 & AGE < 75 ~ 2,
      AGE >= 75 ~ 3,
      TRUE ~ NA_real_
    ),

    # Treatment (numeric)
    ARMN = case_when(
      ARM == "Placebo" ~ 0,
      ARM == "Xanomeline Low Dose" ~ 1,
      ARM == "Xanomeline High Dose" ~ 2,
      TRUE ~ 3
    ),
    ACTARMN = case_when(
      ACTARM == "Placebo" ~ 0,
      ACTARM == "Xanomeline Low Dose" ~ 1,
      ACTARM == "Xanomeline High Dose" ~ 2,
      TRUE ~ 3
    )
  )

## Add baseline vitals ----

adsl_vs <- adsl_cov %>%
  derive_vars_merged(
    dataset_add = advs,
    filter_add = PARAMCD == "HEIGHT" & ABLFL == "Y",
    by_vars = exprs(STUDYID, USUBJID),
    new_vars = exprs(HTBL = AVAL)
  ) %>%
  derive_vars_merged(
    dataset_add = advs,
    filter_add = PARAMCD == "WEIGHT" & ABLFL == "Y",
    by_vars = exprs(STUDYID, USUBJID),
    new_vars = exprs(WTBL = AVAL)
  ) %>%
  mutate(
    BMIBL = compute_bmi(height = HTBL, weight = WTBL),
    BSABL = compute_bsa(height = HTBL, weight = WTBL, method = "Mosteller"),
    WTBLGR1 = case_when(
      WTBL < 70 ~ "<70 kg",
      WTBL >= 70 ~ ">=70 kg",
      TRUE ~ NA_character_
    )
  )

## Add baseline labs ----

labs_bl <- adlb %>%
  filter(ABLFL == "Y" & PARAMCD %in% c("CREAT", "ALT", "AST", "BILI", "ALB")) %>%
  mutate(PARAMCDB = paste0(PARAMCD, "BL")) %>%
  select(STUDYID, USUBJID, PARAMCDB, AVAL)

adsl_vslb <- adsl_vs %>%
  derive_vars_transposed(
    dataset_merge = labs_bl,
    by_vars = exprs(STUDYID, USUBJID),
    key_var = PARAMCDB,
    value_var = AVAL
  ) %>%
  mutate(
    TBILBL = BILIBL,
    CRCLBL = compute_egfr(
      creat = CREATBL, creatu = "SI", age = AGE,
      weight = WTBL, sex = SEX, method = "CRCL"
    ),
    EGFRBL = compute_egfr(
      creat = CREATBL, creatu = "SI", age = AGE,
      weight = WTBL, sex = SEX, method = "CKD-EPI"
    )
  ) %>%
  select(-BILIBL)

# ---- Derive Exposure Metrics

exposure_final <- adsl_vslb %>%
  derive_vars_transposed(
    dataset_merge = adpp,
    filter = PARAMCD %in% c("AUCLST", "CMAX"),
    by_vars = get_admiral_option("subject_keys"),
    key_var = PARAMCD,
    value_var = AVAL
  ) %>%
  rename(AUCSS = AUCLST, CMAXSS = CMAX)

# ---- Create adee base dataset

# Get variable names from both datasets
adsl_vars <- names(exposure_final)
adtte_vars <- names(adtte)

# Find common variables
common_vars <- intersect(adsl_vars, adtte_vars)

# Remove key variables to get variables to drop
vars_to_drop <- setdiff(common_vars, c("STUDYID", "USUBJID"))

adee_base <- adtte %>%
  # Filter to efficacy endpoints
  filter(PARAMCD %in% c("OS", "PFS", "TTP", "TTNT")) %>%
  # Add derived variables
  mutate(
    EVENT = 1 - CNSR,
    AVALU = if_else(!is.na(AVAL), "DAYS", NA_character_),
  ) %>%
  # Remove overlapping variables (use clean method)
  select(-any_of(vars_to_drop)) %>%
  # Merge exposure and covariates
  derive_vars_merged(
    dataset_add = exposure_final,
    by_vars = exprs(STUDYID, USUBJID)
  )

# ---- Add Anaylsis variables

adee <- adee_base %>%
  # Analysis flags
  mutate(
    ANL01FL = if_else(PARAMCD == "PFS", "Y", ""),
    ANL02FL = if_else(PARAMCD == "OS", "Y", ""),
    ANL03FL = if_else(PARAMCD == "TTP", "Y", ""),
    ANL04FL = if_else(PARAMCD == "TTNT", "Y", "")
  ) %>%
  # Parameter categories
  mutate(
    PARCAT1 = "EFFICACY",
    PARCAT2 = "TIME TO EVENT"
  ) %>%
  # Sequence number
  derive_var_obs_number(
    by_vars = exprs(STUDYID, USUBJID),
    order = exprs(PARAMN),
    new_var = ASEQ,
    check_type = "error"
  )

# Save output ----

# Change to whichever directory you want to save the dataset in
dir <- tools::R_user_dir("admiralonco_templates_data", which = "cache")
if (!file.exists(dir)) {
  # Create the folder
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
}
save(adee, file = file.path(dir, "adee.rda"), compress = "bzip2")
