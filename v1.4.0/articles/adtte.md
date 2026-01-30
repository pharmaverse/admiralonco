# Creating ADTTE

## Introduction

This article describes creating an `ADTTE` (time-to-event) ADaM with
common oncology endpoint parameters.

The main part in programming a time-to-event dataset is the definition
of the events and censoring times.
[admiral](https://pharmaverse.github.io/admiral/)/[admiralonco](https://pharmaverse.github.io/admiralonco/)
supports single events like death (Overall Survival) or composite events
like disease progression or death (Progression Free Survival). More than
one source dataset can be used for the definition of the event and
censoring times.

The majority of the functions used here exist from
[admiral](https://pharmaverse.github.io/admiral/), except for the
`tte_sources` helper object, provided as an example from
[admiralonco](https://pharmaverse.github.io/admiralonco/). In practice,
each company would create their own version of this, as likely the exact
specifications such as filtering condition or description metadata will
vary.

**Note**: *All examples assume CDISC SDTM and/or ADaM format as input
unless otherwise specified.*

### Required Packages

The examples of this vignette require the following packages.

``` r
library(admiral)
library(admiralonco)
library(pharmaverseadam)
library(dplyr)
library(lubridate)
```

## Programming Workflow

- [Read in Data](#readdata)
- [Derive Parameters (`CNSR`, `ADT`, `STARTDT`)](#parameters)
- [Derive Analysis Value (`AVAL`)](#aval)
- [Derive Analysis Sequence Number (`ASEQ`)](#aseq)
- [Add ADSL Variables](#adslvars)

### Read in Data

To start, all datasets needed for the creation of the time-to-event
dataset should be read into the environment. This will be a company
specific process.

For example purpose, the ADaM datasets—which are included in
[pharmaverseadam](https://pharmaverse.github.io/pharmaverseadam/)—are
used. An alternative might be to use `ADEVENT` as input.

``` r
data("adsl")
data("adrs_onco")
adrs <- adrs_onco
```

### Derive Parameters (`CNSR`, `ADT`, `STARTDT`)

To derive the parameter dependent variables like `CNSR`, `ADT`,
`STARTDT`, `EVNTDESC`, `SRCDOM`, `PARAMCD`, … the
[`admiral::derive_param_tte()`](https:/pharmaverse.github.io/admiral/v1.4.0/cran-release/reference/derive_param_tte.html)
function can be used. It adds one parameter to the input dataset with
one observation per subject. Usually it is called several times.

For each subject it is determined if an event occurred. In the
affirmative the analysis date `ADT` is set to the earliest event date.
If no event occurred, the analysis date is set to the latest censoring
date.

The events and censorings are defined by the
[`admiral::event_source()`](https:/pharmaverse.github.io/admiral/v1.4.0/cran-release/reference/event_source.html)
and the
[`admiral::censor_source()`](https:/pharmaverse.github.io/admiral/v1.4.0/cran-release/reference/censor_source.html)
class respectively. It defines

- which observations (`filter` parameter) of a source dataset
  (`dataset_name` parameter) are potential events or censorings,
- the value of the `CNSR` variable (`censor` parameter), and
- which variable provides the date (`date` parameter).

The date can be provided as date (`--DT` variable), datetime (`--DTM`
variable), or character ISO-8601 date (`--DTC` variable).

CDISC strongly recommends `CNSR = 0` for events and positive integers
for censorings.
[admiral](https://pharmaverse.github.io/admiral/)/[admiralonco](https://pharmaverse.github.io/admiralonco/)
enforce this recommendation. Therefore the `censor` parameter is
available for
[`admiral::censor_source()`](https:/pharmaverse.github.io/admiral/v1.4.0/cran-release/reference/censor_source.html)
only. It is defaulted to `1`.

The `dataset_name` parameter expects a character value which is used as
an identifier. The actual data which is used for the derivation of the
parameter is provided via the `source_datasets` parameter of
[`admiral::derive_param_tte()`](https:/pharmaverse.github.io/admiral/v1.4.0/cran-release/reference/derive_param_tte.html).
It expects a named list of datasets. The names correspond to the
identifiers specified for the `dataset_name` parameter. This allows to
define events and censoring independent of the data.

#### Pre-Defined Time-to-Event Source Objects

The table below shows all pre-defined `tte_source` objects which should
cover the most common oncology use cases.

[TABLE]

As mentioned in the introduction, each company would create their own
version of this with the required filtering conditions and metadata as
per your company approach. An example of a possible different approach
could be as follows, where death is sourced from `ADSL`, instead of
`ADRS`, and the given `EVNTDESC` is different.

``` r
adsl_death_event <- event_source(
  dataset_name = "adsl",
  date = DTHDT,
  set_values_to = exprs(
    EVNTDESC = "STUDY DEATH",
    SRCDOM = "ADSL",
    SRCVAR = "DTHDT"
  )
)
```

An optional step at this stage would be required to enable derivation of
duration of response: If using `ADRS` / `ADEVENT` parameters as input
for any response dates (instead of a variable in `ADSL`) then you would
need to use
[`admiral::derive_vars_merged()`](https:/pharmaverse.github.io/admiral/v1.4.0/cran-release/reference/derive_vars_merged.html)
to add the response date as a temporary variable (e.g. `TEMP_RESPDT`) to
be able to feed into
[`admiral::derive_param_tte()`](https:/pharmaverse.github.io/admiral/v1.4.0/cran-release/reference/derive_param_tte.html)
as the start date. You would also need to use this to filter the source
`ADSL` dataset so as to only derive the records for responders. This
could also be repeated as needed for IRF/BICR and confirmed responses.

Here is an example of the code needed.

``` r
adsl <- adsl %>%
  derive_vars_merged(
    dataset_add = adrs,
    filter_add = PARAMCD == "RSP" & AVALC == "Y" & ANL01FL == "Y",
    by_vars = get_admiral_option("subject_keys"),
    new_vars = exprs(TEMP_RESPDT = ADT)
  )
```

The pre-defined objects can be passed directly to
[`admiral::derive_param_tte()`](https:/pharmaverse.github.io/admiral/v1.4.0/cran-release/reference/derive_param_tte.html)
to create a new time-to-event parameter. Below shows example calls for
Overall Survival (OS), Progression Free Survival (PFS), and duration of
response (as above, this is only derived for responder patients so we
have to filter source `ADSL` dataset). Note that the reason for
including a randomization date censor is to catch those patients that
never have a tumor assessment.

``` r
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
```

#### Creating Your Own Time-to-Event Source Objects

We advise you consult the
[admiral](https://pharmaverse.github.io/admiral/) [Creating a BDS
Time-to-Event ADaM
vignette](https://pharmaverse.github.io/admiral/articles/bds_tte.html)
for further guidance on the different options available and more
examples.

One extra common oncology case we include here is around PFS when
censoring at new anti-cancer therapy. This could either be controlled
using `ANLzzFL` as explained in the ADRS vignette, so that records after
new anti-cancer therapy never contribute to the PD and DEATH parameters.
Or alternatively you can control this on the ADTTE side by filtering
which records are used in
[`admiral::event_source()`](https:/pharmaverse.github.io/admiral/v1.4.0/cran-release/reference/event_source.html)
and
[`admiral::censor_source()`](https:/pharmaverse.github.io/admiral/v1.4.0/cran-release/reference/censor_source.html),
e.g. for PD or death event date we can use `filter` argument to exclude
events occurring after new anti-cancer therapy.

The censor could be set as whichever date your analysis requires,
e.g. date of last tumor assessment prior to new anti-cancer therapy or
last radiological assessment. If you pass multiple censor dates then
remember the function will choose the latest occurring of these, so be
cautious here if feeding in say one censor date for last assessment
prior to new anti-cancer therapy and one for last assessment - as the
function would choose the maximum of these which in this case would be
incorrect. The easiest solution here would be to pass in one censor date
as the date of last assessment prior to new anti-cancer therapy or date
of last assessment **if no new anti-cancer therapy**. If you wanted to
use different censor dates which could have different `CNSDTDSC` values,
then you’d need to ensure only one is set per patient.

This case is demonstrated in the below example (where `NACTDT` would be
pre-derived as first date of new anti-cancer therapy, and `LASTANDT` as
the single tumor assessment censor date as described above. See
[admiralonco](https://pharmaverse.github.io/admiralonco/) [Creating and
Using New Anti-Cancer Start
Date](https:/pharmaverse.github.io/admiralonco/v1.4.0/articles/nactdt.md)
for deriving `NACTDT`).

``` r
pd_nact_event <- event_source(
  dataset_name = "adsl",
  filter = PDDT < NACTDT | is.na(NACTDT),
  date = PDDT,
  set_values_to = exprs(
    EVNTDESC = "Disease Progression prior to NACT",
    SRCDOM = "ADSL",
    SRCVAR = "PDDT"
  )
)

death_nact_event <- event_source(
  dataset_name = "adsl",
  filter = DTHDT < NACTDT | is.na(NACTDT),
  date = DTHDT,
  set_values_to = exprs(
    EVNTDESC = "Death prior to NACT",
    SRCDOM = "ADSL",
    SRCVAR = "DTHDT"
  )
)

lasta_nact_censor <- censor_source(
  dataset_name = "adsl",
  date = LASTANDT,
  set_values_to = exprs(
    EVNTDESC = "Last Tumor Assessment prior to NACT",
    CNSDTDSC = "Last Tumor Assessment prior to NACT",
    SRCDOM = "ADSL",
    SRCVAR = "LASTANDT"
  )
)

adtte <- derive_param_tte(
  dataset_adsl = adsl,
  start_date = RANDDT,
  event_conditions = list(pd_nact_event, death_nact_event),
  censor_conditions = list(lasta_nact_censor, rand_censor),
  source_datasets = list(adsl = adsl),
  set_values_to = exprs(PARAMCD = "PFSNACT", PARAM = "Progression Free Survival prior to NACT")
)
```

### Derive Analysis Value (`AVAL`)

The analysis value (`AVAL`) can be derived by calling
[`admiral::derive_vars_duration()`](https:/pharmaverse.github.io/admiral/v1.4.0/cran-release/reference/derive_vars_duration.html).

This example derives the time to event in days.

``` r
adtte <- adtte %>%
  derive_vars_duration(
    new_var = AVAL,
    start_date = STARTDT,
    end_date = ADT
  )
```

Other time units, such as months that we commonly see in oncology
analyses, can be requested by specifying the `out_unit` parameter. See
the example below. Note that because of the underlying
[`lubridate::time_length()`](https://lubridate.tidyverse.org/reference/time_length.html)
function that is used here this may perform slightly differently to your
expectations, e.g. both
`time_length(ymd("2021-01-01") %--% ymd("2021-02-01"), "month")` and
`time_length(ymd("2021-02-01") %--% ymd("2021-03-01"), "month")` results
in exactly 1 month, which is a logical approach but it gives a different
result to the convention of assuming every month has exactly equal days
and just using `/30.4375` here or some other such convention. The
difference would only be noticed for small durations, but if the user
prefers an alternative approach they could calculate in the default days
and then add extra processing to convert to months with their
company-specific convention.

``` r
adtte_months <- adtte %>%
  derive_vars_duration(
    new_var = AVAL,
    start_date = STARTDT,
    end_date = ADT,
    out_unit = "months"
  )
```

### Derive Analysis Sequence Number (`ASEQ`)

The [admiral](https://pharmaverse.github.io/admiral/) function
[`admiral::derive_var_obs_number()`](https:/pharmaverse.github.io/admiral/v1.4.0/cran-release/reference/derive_var_obs_number.html)
can be used to derive `ASEQ`:

``` r
adtte <- adtte %>%
  derive_var_obs_number(
    by_vars = get_admiral_option("subject_keys"),
    order = exprs(PARAMCD),
    check_type = "error"
  )
```

### Add ADSL Variables

Variables from ADSL which are required for time-to-event analyses, e.g.,
treatment variables or covariates can be added using
[`admiral::derive_vars_merged()`](https:/pharmaverse.github.io/admiral/v1.4.0/cran-release/reference/derive_vars_merged.html).

``` r
adtte <- adtte %>%
  derive_vars_merged(
    dataset_add = adsl,
    new_vars = exprs(ARMCD, ARM, ACTARMCD, ACTARM, AGE, SEX),
    by_vars = get_admiral_option("subject_keys")
  )
```

## Example Script

| ADaM  | Sample Code                                                  |
|-------|--------------------------------------------------------------|
| ADTTE | `admiral::use_ad_template("ADTTE", package = "admiralonco")` |
