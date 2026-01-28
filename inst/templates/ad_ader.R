# Name: ADER
#
# Label: Exposure Response Data
#
# Input: adsl, adrs, adtte, adlb, advs, adex
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

data("admiral_adsl")
data("admiral_adrs")
data("country_code_lookup")

adsl <- admiral_adsl
adrs <- admiral_adrs

adtte <- pharmaverseadam::adtte_onco
adlb <- pharmaverseadam::adlb
advs <- pharmaverseadam::advs
adex <- pharmaverseadam::adex %>%
  filter(PARCAT1 == "INDIVIDUAL")

# Derivations ----

# For ADTTE censor variables add "IND" to PARAMCD
adttei <- adtte %>%
  mutate(PARAMCD = paste0(PARAMCD, "IND"))

ader_tte <- adsl %>%
  select(!!!get_admiral_option("subject_keys")) %>%
  # Create OS and PFS variables from ADTTE
  derive_vars_transposed(
    dataset_merge = adtte,
    by_vars = get_admiral_option("subject_keys"),
    key_var = PARAMCD,
    value_var = AVAL
  ) %>%
  # Create OS and PFS censor variables
  derive_vars_transposed(
    dataset_merge = adttei,
    by_vars = get_admiral_option("subject_keys"),
    key_var = PARAMCD,
    value_var = CNSR
  )

# Derive Best Overall Response (BOR) variables from ADRS
ader_bor <- ader_tte %>%
  derive_vars_merged(
    dataset_add = adrs,
    filter_add = PARAMCD == "BOR" & ANL01FL == "Y",
    by_vars = get_admiral_option("subject_keys"),
    new_vars = exprs(BOR = AVAL, BORC = AVALC)
  )

ader_aseq <- ader_bor %>%
  derive_var_obs_number(
    by_vars = get_admiral_option("subject_keys"),
    check_type = "error"
  )

# ---- Derive Covariates ----
# Include numeric values for STUDYIDN, USUBJIDN, SEXN, RACEN etc.

covar <- adsl %>%
  derive_vars_merged(
    dataset_add = country_code_lookup,
    new_vars = exprs(COUNTRYN = country_number, COUNTRYL = country_name),
    by_vars = exprs(COUNTRY = country_code)
  ) %>%
  mutate(
    STUDYIDN = as.numeric(word(USUBJID, 1, sep = fixed("-"))),
    SITEIDN = as.numeric(word(USUBJID, 2, sep = fixed("-"))),
    USUBJIDN = as.numeric(word(USUBJID, 3, sep = fixed("-"))),
    SUBJIDN = as.numeric(SUBJID),
    SEXN = case_when(
      SEX == "M" ~ 1,
      SEX == "F" ~ 2,
      TRUE ~ 3
    ),
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
    ),
    COHORT = ARMN,
    COHORTC = ARM,
    ROUTE = unique(adex$EXROUTE)[1],
    ROUTEN = case_when(
      ROUTE == "TRANSDERMAL" ~ 3,
      TRUE ~ NA_real_
    ),
    FORM = unique(adex$EXDOSFRM)[1],
    FORMN = case_when(
      FORM == "PATCH" ~ 3,
      TRUE ~ 4
    )
  ) %>%
  select(
    STUDYID, STUDYIDN, SITEID, SITEIDN, USUBJID, USUBJIDN,
    SUBJID, SUBJIDN, AGE, SEX, SEXN, COHORT, COHORTC, ROUTE, ROUTEN,
    RACE, RACEN, ETHNIC, ETHNICN, FORM, FORMN, COUNTRY, COUNTRYN, COUNTRYL
  )

# ---- Derive additional baselines from ADVS and ADLB ----

labsbl <- adlb %>%
  filter(ABLFL == "Y" & PARAMCD %in% c("CREAT", "ALT", "AST", "BILI")) %>%
  mutate(PARAMCDB = paste0(PARAMCD, "BL")) %>%
  select(STUDYID, USUBJID, PARAMCDB, AVAL)

covar_vslb <- covar %>%
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
  derive_vars_transposed(
    dataset_merge = labsbl,
    by_vars = exprs(STUDYID, USUBJID),
    key_var = PARAMCDB,
    value_var = AVAL
  ) %>%
  mutate(
    BMIBL = compute_bmi(height = HTBL, weight = WTBL),
    BSABL = compute_bsa(
      height = HTBL,
      weight = WTBL,
      method = "Mosteller"
    ),
    CRCLBL = compute_egfr(
      creat = CREATBL, creatu = "SI", age = AGE, weight = WTBL, sex = SEX,
      method = "CRCL"
    ),
    EGFRBL = compute_egfr(
      creat = CREATBL, creatu = "SI", age = AGE, weight = WTBL, sex = SEX,
      method = "CKD-EPI"
    )
  ) %>%
  rename(TBILBL = BILIBL)

# Combine covariates with ADER data

ader <- ader_aseq %>%
  derive_vars_merged(
    dataset_add = covar_vslb,
    by_vars = exprs(STUDYID, USUBJID)
  )

# Save output ----

# Change to whichever directory you want to save the dataset in
dir <- tools::R_user_dir("admiralonco_templates_data", which = "cache")
if (!file.exists(dir)) {
  # Create the folder
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
}
save(ader, file = file.path(dir, "ader.rda"), compress = "bzip2")
