# Name: ADTRR
#
# Label: Exposure Tumor Response Data
#
# Input: adsl, adtr, adlb, advs, adex, adpp
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

# Use haven::read_sas() or similar to read in production data
# For illustration, using pharmaverseadam example data
library(pharmaverseadam)

adsl <- pharmaverseadam::adsl
adtr <- pharmaverseadam::adtr_onco # Has tumor measurements
adrs <- pharmaverseadam::adrs_onco # Has response evaluations
adlb <- pharmaverseadam::adlb
advs <- pharmaverseadam::advs

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

# ---- Derive Baseline Covariates

## Numeric identifiers and demographics ----

adsl_cov <- adsl %>%
  filter(SAFFL == "Y") %>%
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

# ---- Create tumor size parameter dataset

# Get variable names for clean dropping
adsl_vars <- names(exposure_final)
adtr_vars <- names(adtr)
common_vars <- intersect(adsl_vars, adtr_vars)
vars_to_drop <- setdiff(common_vars, c("STUDYID", "USUBJID"))

tsize_final <- adtr %>%
  filter(PARAMCD == "SDIAM") %>%
  mutate(
    PARAMCD = "TSIZE",
    PARAM = "Target Lesions Sum of Diameters",
    PARAMN = 1
  ) %>%
  derive_var_nfrlt(
    new_var = NFRLT,
    new_var_unit = FRLTU,
    out_unit = "HOURS",
    visit_day = ADY
  ) %>%
  select(-any_of(vars_to_drop)) %>%
  derive_vars_merged(
    dataset_add = exposure_final,
    by_vars = exprs(STUDYID, USUBJID)
  )

# ---- Add BOR from ADRS

adrs_vars <- names(adrs)
common_vars_adrs <- intersect(adsl_vars, adrs_vars)
vars_to_drop_adrs <- setdiff(common_vars_adrs, c("STUDYID", "USUBJID"))

bor <- adrs %>%
  filter(PARAMCD == "BOR" & SAFFL == "Y") %>%
  mutate(
    PARAMN = 2,
    # Create BORN from AVALC if AVAL doesn't exist
    BORN = if ("AVAL" %in% names(.)) {
      AVAL
    } else {
      case_when(
        AVALC == "CR" ~ 4,
        AVALC == "PR" ~ 3,
        AVALC == "SD" ~ 2,
        AVALC == "PD" ~ 1,
        AVALC == "NE" ~ 0,
        TRUE ~ NA_real_
      )
    }
  ) %>%
  select(-any_of(vars_to_drop_adrs)) %>%
  derive_vars_merged(
    dataset_add = exposure_final,
    by_vars = exprs(STUDYID, USUBJID)
  )

# ---- Derive Nadir

# Calculate nadir from TSIZE records
# Keep BASE, CHG, PCHG from the nadir timepoint
nadir <- tsize_final %>%
  filter(AVISITN > 0 & !is.na(AVAL)) %>%
  group_by(STUDYID, USUBJID) %>%
  filter(AVAL == min(AVAL, na.rm = TRUE)) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(
    PARAMCD = "NADIR",
    PARAM = "Nadir Tumor Size",
    PARAMN = 3,
    NADIR = AVAL,
    NADPCHG = PCHG, # Keep PCHG at nadir
    NADVST = AVISIT # Keep visit of nadir
  )

# ---- Combine parameters

adtrr_base <- bind_rows(
  tsize_final,
  bor,
  nadir
) %>%
  arrange(USUBJID, PARAMN, AVISITN)

# ---- Add analysis variables

# Ensure AVALU exists before mutating
if (!"AVALU" %in% names(adtrr_base)) {
  adtrr_base <- adtrr_base %>%
    mutate(AVALU = NA_character_)
}

adtrr <- adtrr_base %>%
  # Analysis flags
  mutate(
    # Baseline flag
    ABLFL = case_when(
      !is.na(ABLFL) ~ ABLFL,
      !is.na(AVISITN) & AVISITN == 0 ~ "Y",
      TRUE ~ ""
    ),

    # Post-baseline flag
    ANL01FL = if_else(!is.na(AVISITN) & AVISITN > 0, "Y", ""),

    # Responders (CR or PR)
    ANL02FL = if_else(!is.na(AVALC) & AVALC %in% c("CR", "PR"), "Y", ""),

    # Has change from baseline
    ANL03FL = if_else(!is.na(PCHG), "Y", "")
  ) %>%
  # Parameter categories
  mutate(
    PARCAT1 = "TUMOR RESPONSE",
    PARCAT2 = case_when(
      PARAMCD == "TSIZE" ~ "MEASUREMENT",
      PARAMCD == "BOR" ~ "OVERALL RESPONSE",
      PARAMCD == "NADIR" ~ "BEST RESPONSE",
      TRUE ~ NA_character_
    )
  ) %>%
  # Set AVALU (now guaranteed to exist)
  mutate(
    AVALU = case_when(
      !is.na(AVALU) & AVALU != "" ~ AVALU, # Keep existing non-empty
      PARAMCD == "TSIZE" ~ "mm",
      PARAMCD == "NADIR" ~ "mm",
      TRUE ~ NA_character_
    )
  ) %>%
  # Sequence number
  derive_var_obs_number(
    by_vars = exprs(STUDYID, USUBJID),
    order = exprs(PARAMN, AVISITN),
    new_var = ASEQ,
    check_type = "error"
  ) %>%
  arrange(USUBJID, PARAMN, AVISITN)

# Save output ----

# Change to whichever directory you want to save the dataset in
dir <- tools::R_user_dir("admiralonco_templates_data", which = "cache")
if (!file.exists(dir)) {
  # Create the folder
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
}
save(adtrr, file = file.path(dir, "adtrr.rda"), compress = "bzip2")
