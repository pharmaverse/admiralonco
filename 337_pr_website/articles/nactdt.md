# Creating and Using New Anti-Cancer Start Date

## Introduction

In Oncology trials, for censoring the patients for any time-to-event
analysis (e.g., progression free survival analysis, duration of
response, etc.), it is necessary to check if the patients have started
any new anti-cancer therapy. Anti-cancer therapy generally includes
radiation therapy, cancer related surgery and any systemic anti-cancer
therapy such as chemotherapy, t-cell therapy, etc. These therapies can
be collected on a prior or on-treatment CRF pages, with radiation and
surgery being mapped to the SDTM PR domain and systemic anti-cancer
therapy being mapped to the SDTM CM domain. Collection and mapping may
vary with each company and it is important that only on-treatment
therapies are considered for the censoring of time-to-event. Generally,
if the patients start a new anti-cancer therapy that were on-treatment,
they should be discontinued from the study treatment and no further
efficacy assessments should be performed.

This article describes a process for creating a new anti-cancer start
date from a single and multiple source SDTMs.

## Programming Workflow

- [Read in Data and create test data](#readdata)
- [Single Source](#input)
- [Multiple Sources - Prerequisite Step](#multiinput)
- [Multiple Sources - Derive New Anti-Cancer Start Date and Merge with
  `ADSL`](#nactdt)
- [Using the New Anti-Cancer Therapy Date](#unactdt)
- [Derive Date Parameter](#par)

### Read in Data

To start, all data frames needed for the creation of the new anti-cancer
therapy start date (`NACTDT`) should be read into the environment. This
will be a company specific process. Some of the data frames needed may
be `CM` and `PR`.

For example purpose, `CM` and `PR` has been generated (based on CDISC
Pilot test data), with oncology specific test data.

``` r
library(admiral)
library(pharmaverseadam)
library(dplyr)

data("adsl")
adsl_onco <- adsl
data("adrs_onco")

cm <- tribble(
  ~STUDYID, ~USUBJID, ~CMCAT, ~CMSCAT, ~CMTRT, ~CMSTDTC,
  "CDISCPILOT01", "01-701-1015", "PRIOR TREATMENT", "CHEMOTHERAPY", "DEXRAZOXANE", NA,
  "CDISCPILOT01", "01-701-1015", "ON TREATMENT", "CHEMOTHERAPY", "DEXROZOXANE", "2014-07-02",
  "CDISCPILOT01", "01-701-1015", "ON TREATMENT", "CHEMOTHERAPY", "DEXROZOXANE", "2014-06-19",
  "CDISCPILOT01", "01-701-1028", "PRIOR TREATMENT", "CHEMOTHERAPY", "METHOTREXATE", NA,
  "CDISCPILOT01", "01-701-1028", "ON TREATMENT", "CHEMOTHERAPY", "METHOTREXATE", "2014-01-14",
  "CDISCPILOT01", "01-701-1034", "PRIOR TREATMENT", "CHEMOTHERAPY", "OLAPARIB", NA,
  "CDISCPILOT01", "01-701-1034", "ON TREATMENT", "CHEMOTHERAPY", "OLAPARIB", "2014-12-30",
  "CDISCPILOT01", "01-701-1097", "PRIOR TREATMENT", "CHEMOTHERAPY", "TEMODAL", NA,
  "CDISCPILOT01", "01-701-1097", "ON TREATMENT", "CHEMOTHERAPY", "TEMODAL", "2013-12-31",
)

pr <- tribble(
  ~STUDYID, ~USUBJID, ~PRCAT, ~PRSCAT, ~PRTRT, ~PRSTDTC,
  "CDISCPILOT01", "01-701-1015", "CANCER RELATED", "ON TREATMENT", "SURGERY", "2014-06-18",
  "CDISCPILOT01", "01-701-1034", "CANCER RELATED", "ON TREATMENT", "SURGERY", "2014-12-16",
  "CDISCPILOT01", "01-701-1028", "CANCER RELATED", "PRIOR TREATMENT", "SURGERY", NA,
)
```

### Single Source

To derive the New Anti-Cancer Therapy start date with data from a single
source, the function
[`admiral::derive_vars_merged()`](https:/pharmaverse.github.io/admiral/v1.4.1/cran-release/reference/derive_vars_merged.html)
can be used. If numeric date is required,
[`admiral::convert_dtc_to_dt`](https:/pharmaverse.github.io/admiral/v1.4.1/cran-release/reference/convert_dtc_to_dt.html)
, or if time part is needed,
[`admiral::convert_dtc_to_dtm()`](https:/pharmaverse.github.io/admiral/v1.4.1/cran-release/reference/convert_dtc_to_dtm.html)
can be used as part of an expression for the `new_vars` argument.

``` r
adsl <- derive_vars_merged(
  adsl_onco,
  dataset_add = cm,
  by_vars = get_admiral_option("subject_keys"),
  order = exprs(NACTDT),
  mode = "first",
  new_vars = exprs(NACTDT = convert_dtc_to_dt(CMSTDTC)),
  filter_add = CMSCAT == "CHEMOTHERAPY" & CMCAT == "ON TREATMENT"
)
```

### Multiple Sources - Prerequisite Steps

To derive the New Anti-Cancer Therapy start date with data from multiple
sources, the function
[`admiral::derive_vars_extreme_event()`](https:/pharmaverse.github.io/admiral/v1.4.1/cran-release/reference/derive_vars_extreme_event.html)
can be used.

#### Prerequisite

Prior to calling the function, an input `event` object must be created
for each source dataset using
[`admiral::event()`](https:/pharmaverse.github.io/admiral/v1.4.1/cran-release/reference/event.html).
Within this function, any company-specific subsetting can be passed to
the `condition` argument. If numeric version of the dates will be
needed,
[`admiral::convert_dtc_to_dt()`](https:/pharmaverse.github.io/admiral/v1.4.1/cran-release/reference/convert_dtc_to_dt.html),
or if time part is needed,
[`admiral::convert_dtc_to_dtm()`](https:/pharmaverse.github.io/admiral/v1.4.1/cran-release/reference/convert_dtc_to_dtm.html)
can be used as part of an expression for the `set_values_to` argument.

``` r
cm_date <- event(
  dataset_name = "cm",
  condition = CMSCAT == "CHEMOTHERAPY" & CMCAT == "ON TREATMENT" & !is.na(CMSTDTC),
  set_values_to = exprs(NACTDT = convert_dtc_to_dt(CMSTDTC))
)

pr_date <- event(
  dataset_name = "pr",
  condition = PRCAT == "CANCER RELATED" & PRSCAT == "ON TREATMENT" & !is.na(PRSTDTC),
  set_values_to = exprs(NACTDT = convert_dtc_to_dt(PRSTDTC))
)
```

### Multiple Sources - Derive New Anti-Cancer Start Date and Merge with `ADSL`

After completion of the prerequisite step, the new anti-cancer date can
be derived while simultaneously adding it to `ADSL`.

``` r
adsl <- adsl_onco %>%
  derive_vars_extreme_event(
    by_vars = get_admiral_option("subject_keys"),
    order = exprs(NACTDT),
    new_vars = exprs(NACTDT),
    events = list(cm_date, pr_date),
    source_datasets = list(
      cm = cm,
      pr = pr
    ),
    mode = "first"
  )
```

### Using the New Anti-Cancer Therapy Date

Please refer to
[admiralonco](https://pharmaverse.github.io/admiralonco/) [Derive
`ANL01FL`](https:/pharmaverse.github.io/admiralonco/337_pr_website/articles/adrs.html#anl01fl)
for an example on the usage of `NACTDT`. Additionally,
[admiralonco](https://pharmaverse.github.io/admiralonco/) [Creating Your
Own Time-to-Event Source
Objects](https:/pharmaverse.github.io/admiralonco/337_pr_website/articles/adtte.html#tteobj)
may be referenced for case usage.

### Derive Date Parameter

The [admiral](https://pharmaverse.github.io/admiral/) function
[`admiral::derive_extreme_event()`](https:/pharmaverse.github.io/admiral/v1.4.1/cran-release/reference/derive_extreme_event.html)
can be used for deriving parameter based on the first or last
observation from single and multiple sources.

Based on individual company standards, this parameter may be added to an
event or date dataset such as `ADEVENT` or `ADDATES`, which are
generally intermediate datasets to `ADTTE`. For demonstration purpose, a
new anti-cancer date parameter will be merged into `ADRS`. A list of
[`event()`](https:/pharmaverse.github.io/admiral/v1.4.1/cran-release/reference/event.html)
objects is expected and this will contain the sources of the dates and
any company specific filtering.

``` r
adrs <- derive_extreme_event(
  dataset = adrs_onco,
  events = list(
    event(
      dataset_name = "cm",
      condition = CMSCAT == "CHEMOTHERAPY" & CMCAT == "ON TREATMENT" & !is.na(CMSTDTC),
      set_values_to = exprs(
        ADT = convert_dtc_to_dt(CMSTDTC),
        AVALC = CMTRT
      )
    ),
    event(
      dataset_name = "pr",
      condition = PRCAT == "CANCER RELATED" & PRSCAT == "ON TREATMENT" & !is.na(PRSTDTC),
      set_values_to = exprs(
        ADT = convert_dtc_to_dt(PRSTDTC),
        AVALC = PRTRT
      )
    )
  ),
  source_datasets = list(cm = cm, pr = pr),
  by_vars = get_admiral_option("subject_keys"),
  order = exprs(ADT),
  mode = "first",
  set_values_to = exprs(
    PARAMCD = "NACTDT",
    PARAM = "New Anti-Cancer Therapy Start Date"
  )
)
```
