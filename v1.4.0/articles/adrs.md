# Creating ADRS (Including Non-standard Endpoints)

## Introduction

This article describes creating an `ADRS` ADaM with oncology endpoint
parameters based on RECIST v1.1. It shows standard RECIST 1.1 endpoints
and additionally modified versions of the endpoints (see [Derive
Non-standard Parameters](#nonstandard)). Most of the endpoints are
derived by calling
[`admiral::derive_extreme_event()`](https:/pharmaverse.github.io/admiral/v1.4.0/cran-release/reference/derive_extreme_event.html).
It is very flexible. Thus the examples in this vignette can also be used
as a starting point for implementing other response criteria than RECIST
1.1, e.g., iRECIST or International Myeloma Working Group (IMWG)
criteria for the diagnosis of multiple myeloma.

For confirmation of response particularly, `CR`, the case that `CR` is
followed by `PR` (or `SD`) is considered as a data issue and the
derivations of the parameters don’t handle this case specially. It is
recommended to fix the issue in the source data, e.g., by changing the
`PR` to `PD` rather than handling it in the parameter derivations. This
ensures consistency across parameters. However,
[`admiral::derive_extreme_event()`](https:/pharmaverse.github.io/admiral/v1.4.0/cran-release/reference/derive_extreme_event.html)
is so flexible that it is possible to handle it in the parameter
derivations, for example, by redefining the `bor_pr` event and adding an
additional `PD` event.

Examples are currently presented and tested using `ADSL` (ADaM) and
`RS`, `TU` (SDTM) inputs. However, other domains could be used. The
functions and workflow could similarly be used to create an intermediary
`ADEVENT` ADaM.

**Note**: *All examples assume CDISC SDTM and/or ADaM format as input
unless otherwise specified.*

## Programming Workflow

- [Read in Data](#readdata)
- [Pre-processing of Input Records](#input)
- [Derive Progressive Disease Parameter](#pd)
- [Derive Response Parameter](#rsp)
- [Derive Clinical Benefit Parameter](#cb)
- [Derive Best Overall Response Parameter](#bor)
- [Derive Best Overall Response of CR/PR Parameter](#bcp)
- [Derive Response Parameters requiring Confirmation](#confirm)
- [Derive Non-standard Parameters](#nonstandard)
- [Derive Parameters using Independent Review Facility (IRF)/ Blinded
  Independent Central Review (BICR) responses](#irf)
- [Derive Death Parameter](#death)
- [Derive Last Disease Assessment Parameters](#lsta)
- [Derive Measurable Disease at Baseline Parameter](#mdis)
- [Derive `AVAL` for New Parameters](#aval)
- [Assign `ASEQ`](#aseq)
- [Add ADSL variables](#adsl_vars)

### Read in Data

To start, all data frames needed for the creation of `ADRS` should be
read into the environment. This will be a company specific process. Some
of the data frames needed may be `ADSL`, `RS` and `TU`.

For example purpose, the SDTM and ADaM datasets (based on CDISC Pilot
test data)—which are included in
[pharmaversesdtm](https://pharmaverse.github.io/pharmaversesdtm/) and
[pharmaverseadam](https://pharmaverse.github.io/pharmaverseadam/)—are
used.

``` r
library(admiral)
library(admiralonco)
library(dplyr)
library(pharmaversesdtm)
library(pharmaverseadam)
library(lubridate)
library(stringr)
data("adsl")
data("rs_onco_recist")
data("tu_onco_recist")

rs <- rs_onco_recist
tu <- tu_onco_recist

rs <- convert_blanks_to_na(rs)
tu <- convert_blanks_to_na(tu)
```

At this step, it may be useful to join `ADSL` to your `RS` domain. Only
the `ADSL` variables used for derivations are selected at this step. The
rest of the relevant `ADSL` would be added later.

``` r
adsl_vars <- exprs(RANDDT)
adrs <- derive_vars_merged(
  rs,
  dataset_add = adsl,
  new_vars = adsl_vars,
  by_vars = get_admiral_option("subject_keys")
)
```

### Pre-processing of Input Records

The first step involves company-specific pre-processing of records for
the required input to the downstream parameter functions. Note that this
could be needed multiple times (e.g. once for investigator and once for
Independent Review Facility (IRF)/Blinded Independent Central Review
(BICR) records). It could even involve merging input data from other
sources besides `RS`, such as `ADTR`.

This step would include any required selection/derivation of `ADT` or
applying any necessary partial date imputations, updating `AVAL` (e.g.
this should be ordered from best to worst response), and setting
analysis flag `ANL01FL`. Common options for `ANL01FL` would be to set
null for invalid assessments or those occurring after new anti-cancer
therapy, or to only flag assessments on or after after date of first
treatment/randomization, or rules to cover the case when a patient has
multiple observations per visit (e.g. by selecting worst value). Another
consideration could be extra potential protocol-specific sources of
Progressive Disease such as radiological assessments, which could be
pre-processed here to create a PD record to feed downstream derivations.

For the derivation of the parameters it is expected that the subject
identifier variables (usually `STUDYID` and `USUBJID`) and `ADT` are a
unique key. This can be achieved by deriving an analysis flag
(`ANLzzFL`). See [Derive `ANL01FL`](#anl01fl) for an example.

The below shows an example of a possible company-specific implementation
of this step.

#### Select Overall Response Records and Set Parameter Details

In this case we use the overall response records from `RS` from the
investigator as our starting point. The parameter details such as
`PARAMCD`, `PARAM` etc will always be company-specific, but an example
is shown below so that you can trace through how these records feed into
the other parameter derivations.

``` r
adrs <- adrs %>%
  filter(RSEVAL == "INVESTIGATOR" & RSTESTCD == "OVRLRESP") %>%
  mutate(
    PARAMCD = "OVR",
    PARAM = "Overall Response by Investigator",
    PARCAT1 = "Tumor Response",
    PARCAT2 = "Investigator",
    PARCAT3 = "RECIST 1.1"
  )
```

#### Partial Date Imputation and Deriving `ADT`, `ADTF`, `AVISIT` etc

If your data collection allows for partial dates, you could apply a
company-specific imputation rule at this stage when deriving `ADT`. For
this example, here we impute missing day to last possible date.

``` r
adrs <- adrs %>%
  derive_vars_dt(
    dtc = RSDTC,
    new_vars_prefix = "A",
    highest_imputation = "D",
    date_imputation = "last"
  ) %>%
  mutate(AVISIT = VISIT)
```

#### Derive `AVALC` and `AVAL`

Here we populate `AVALC` and create the numeric version as `AVAL`
(ordered from best to worst response). The `AVAL` values are not
considered in the parameter derivations below, and so changing `AVAL`
here would not change the result of those derivations.

``` r
adrs <- adrs %>%
  mutate(
    AVALC = RSSTRESC,
    AVAL = aval_resp(AVALC)
  )
```

#### Flag Worst Assessment at Each Date (`ANL01FL`)

When deriving `ANL01FL` this is an opportunity to exclude any records
that should not contribute to any downstream parameter derivations. In
the below example this includes only selecting valid assessments and
those occurring on or after randomization date. If there is more than
one assessment at a date, the worst one is flagged.

``` r
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
```

Here is an alternative example where those records occurring after new
anti-cancer therapy are additionally excluded (where `NACTDT` would be
pre-derived as first date of new anti-cancer therapy. See
[admiralonco](https://pharmaverse.github.io/admiralonco/) [Creating and
Using New Anti-Cancer Start
Date](https:/pharmaverse.github.io/admiralonco/v1.4.0/articles/nactdt.md)
for deriving this variable).

``` r
adrs <- adrs %>%
  mutate(
    ANL01FL = case_when(
      !is.na(AVAL) & ADT >= RANDDT & ADT < NACTDT ~ "Y",
      TRUE ~ NA_character_
    )
  )
```

#### Flag Assessments up to First PD (`ANL02FL`)

To restrict response data up to and including first reported progressive
disease `ANL02FL` flag could be created by using
[admiral](https://pharmaverse.github.io/admiral/) function
[`admiral::derive_var_relative_flag()`](https:/pharmaverse.github.io/admiral/v1.4.0/cran-release/reference/derive_var_relative_flag.html).

``` r
adrs <- adrs %>%
  derive_var_relative_flag(
    by_vars = get_admiral_option("subject_keys"),
    order = exprs(ADT, RSSEQ),
    new_var = ANL02FL,
    condition = AVALC == "PD",
    mode = "first",
    selection = "before",
    inclusive = TRUE
  )
```

#### Select Source Assessments for Parameter derivations

For most parameter derivations the post-baseline overall response
assessments up to and including first PD are considered.

``` r
ovr <- filter(adrs, PARAMCD == "OVR" & ANL01FL == "Y" & ANL02FL == "Y")
```

### Define Events

The building blocks for the events that contribute to deriving common
endpoints like what constitutes a responder, or a Best Overall Response
of complete response (CR), … are predefined in admiralonco (see
[Pre-Defined Response Event
Objects](https:/pharmaverse.github.io/admiralonco/v1.4.0/reference/event_objects.md)).
Some may need to be adjusted for study-specific needs, e.g., minimum
time between response and confirmation assessment. Here the confirmation
period and the `keep_source_vars` argument is updated.

``` r
confirmation_period <- 21

crsp_y_cr <- event_joined(
  description = paste(
    "Define confirmed response as CR followed by CR at least",
    confirmation_period,
    "days later and at most one NE in between"
  ),
  dataset_name = "ovr",
  join_vars = exprs(AVALC, ADT),
  join_type = "after",
  order = exprs(ADT),
  first_cond_upper = AVALC.join == "CR" &
    ADT.join >= ADT + days(confirmation_period),
  condition = AVALC == "CR" &
    all(AVALC.join %in% c("CR", "NE")) &
    count_vals(var = AVALC.join, val = "NE") <= 1,
  set_values_to = exprs(AVALC = "Y")
)

crsp_y_pr <- event_joined(
  description = paste(
    "Define confirmed response as PR followed by CR or PR at least",
    confirmation_period,
    "days later, at most one NE in between, and no PR after CR"
  ),
  dataset_name = "ovr",
  join_vars = exprs(AVALC, ADT),
  join_type = "after",
  order = exprs(ADT),
  first_cond_upper = AVALC.join %in% c("CR", "PR") &
    ADT.join >= ADT + days(confirmation_period),
  condition = AVALC == "PR" &
    all(AVALC.join %in% c("CR", "PR", "NE")) &
    count_vals(var = AVALC.join, val = "NE") <= 1 &
    (
      min_cond(
        var = ADT.join,
        cond = AVALC.join == "CR"
      ) > max_cond(var = ADT.join, cond = AVALC.join == "PR") |
        count_vals(var = AVALC.join, val = "CR") == 0 |
        count_vals(var = AVALC.join, val = "PR") == 0
    ),
  set_values_to = exprs(AVALC = "Y")
)

cbor_cr <- event_joined(
  description = paste(
    "Define complete response (CR) for confirmed best overall response (CBOR) as",
    "CR followed by CR at least",
    confirmation_period,
    "days later and at most one NE in between"
  ),
  dataset_name = "ovr",
  join_vars = exprs(AVALC, ADT),
  join_type = "after",
  first_cond_upper = AVALC.join == "CR" &
    ADT.join >= ADT + confirmation_period,
  condition = AVALC == "CR" &
    all(AVALC.join %in% c("CR", "NE")) &
    count_vals(var = AVALC.join, val = "NE") <= 1,
  set_values_to = exprs(AVALC = "CR")
)

cbor_pr <- event_joined(
  description = paste(
    "Define partial response (PR) for confirmed best overall response (CBOR) as",
    "PR followed by CR or PR at least",
    confirmation_period,
    "28 days later, at most one NE in between, and no PR after CR"
  ),
  dataset_name = "ovr",
  join_vars = exprs(AVALC, ADT),
  join_type = "after",
  first_cond_upper = AVALC.join %in% c("CR", "PR") &
    ADT.join >= ADT + confirmation_period,
  condition = AVALC == "PR" &
    all(AVALC.join %in% c("CR", "PR", "NE")) &
    count_vals(var = AVALC.join, val = "NE") <= 1 &
    (
      min_cond(
        var = ADT.join,
        cond = AVALC.join == "CR"
      ) > max_cond(var = ADT.join, cond = AVALC.join == "PR") |
        count_vals(var = AVALC.join, val = "CR") == 0 |
        count_vals(var = AVALC.join, val = "PR") == 0
    ),
  set_values_to = exprs(AVALC = "PR")
)

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
```

### Derive Progressive Disease Parameter

Now that we have the input records prepared above with any
company-specific requirements, we can start to derive new parameter
records. For the parameter derivations, all values except those
overwritten by `set_values_to` argument are kept from the earliest
occurring input record fulfilling the required criteria.

The function
[`admiral::derive_extreme_records()`](https:/pharmaverse.github.io/admiral/v1.4.0/cran-release/reference/derive_extreme_records.html)
can be used to find the date of first `PD`.

``` r
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
```

For progressive disease, response and death parameters shown in steps
here and below, in our examples we show these as `ADRS` parameters, but
they could equally be achieved via `ADSL` dates or `ADEVENT` parameters.
If you prefer to store as an ADSL date, then the function
[`admiral::derive_var_extreme_dt()`](https:/pharmaverse.github.io/admiral/v1.4.0/cran-release/reference/derive_var_extreme_dt.html)
could be used to find the date of first `PD` as a variable, rather than
as a new parameter record.

### Derive Response Parameter

The function
[`admiral::derive_extreme_event()`](https:/pharmaverse.github.io/admiral/v1.4.0/cran-release/reference/derive_extreme_event.html)
can then be used to find the date of first response. In the below
example, the response condition has been defined as `CR` or `PR` via the
`rsp_y`[¹](#fn1) event.

``` r
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
```

### Derive Clinical Benefit Parameter

The function
[`admiral::derive_extreme_event()`](https:/pharmaverse.github.io/admiral/v1.4.0/cran-release/reference/derive_extreme_event.html)
can then be used to derive the clinical benefit parameter, which we
define as a patient having had a response or a sustained period of time
before first `PD`. This could also be known as disease control. In this
example the “sustained period” has been defined as 42 days after
randomization date via the `cb_y`[²](#fn2) event.

Please note that the result `AVALC = "Y"` is defined by the first *two*
events specified for `events`. For subjects with observations fulfilling
both events the one with the earlier date should be selected (and not
the first one in the list). Thus `ignore_event_order = TRUE` is
specified.

``` r
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
    )
  )
```

### Derive Best Overall Response Parameter

The function
[`admiral::derive_extreme_event()`](https:/pharmaverse.github.io/admiral/v1.4.0/cran-release/reference/derive_extreme_event.html)
can be used to derive the best overall response (without confirmation
required) parameter. Similar to the above function you can optionally
decide what period would you consider a `SD` or `NON-CR/NON-PD` as being
eligible from. In this example, 42 days after randomization date has
been used again.

Please note that the order of the events specified for `events` is
important. For example, a subject with `PR`, `PR`, `CR` qualifies for
both `bor_cr` and `bor_pr`. As `bor_cr` is listed before `bor_pr`, CR is
selected as best overall response for this subject.

``` r
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
```

Note that the above gives pre-defined `AVAL` values (defined by
[`aval_resp()`](https:/pharmaverse.github.io/admiralonco/v1.4.0/reference/aval_resp.md))
of: `"CR" ~ 1`, `"PR" ~ 2`, `"SD" ~ 3`, `"NON-CR/NON-PD" ~ 4`,
`"PD" ~ 5`, `"NE" ~ 6`, `"MISSING" ~ 7`.

If you’d like to provide your own company-specific ordering here you
could do this as follows:

``` r
aval_resp_new <- function(arg) {
  case_when(
    arg == "CR" ~ 7,
    arg == "PR" ~ 6,
    arg == "SD" ~ 5,
    arg == "NON-CR/NON-PD" ~ 4,
    arg == "PD" ~ 3,
    arg == "NE" ~ 2,
    arg == "MISSING" ~ 1,
    TRUE ~ NA_real_
  )
}
```

Then update the definition of `AVAL` in the `set_values_to` argument of
the above
[`admiral::derive_extreme_event()`](https:/pharmaverse.github.io/admiral/v1.4.0/cran-release/reference/derive_extreme_event.html)
call. Be aware that this will only impact the `AVAL` mapping, not the
derivation of BOR in any way - as the function derivation relies only on
the events and their order specified for the `events` argument here.

### Derive Best Overall Response of CR/PR Parameter

The function
[`admiral::derive_extreme_records()`](https:/pharmaverse.github.io/admiral/v1.4.0/cran-release/reference/derive_extreme_records.html)
can be used to check if a patient had a response for BOR.

``` r
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
```

### Derive Response Parameters requiring Confirmation

Any of the above response parameters can be repeated for “confirmed”
responses only. For these the function
[`admiral::derive_extreme_event()`](https:/pharmaverse.github.io/admiral/v1.4.0/cran-release/reference/derive_extreme_event.html)
can be used with different events. Some of the other functions from
above can then be re-used passing in these confirmed response records.
See the examples below of derived parameters requiring confirmation. The
assessment and the confirmatory assessment here need to occur at least
28 days apart *(without any +1 applied to this calculation of days
between visits)*, using the `crsp_y_cr`[³](#fn3), `crsp_y_pr`[⁴](#fn4),
`cbor_cr`[⁵](#fn5), and `cbor_pr`[⁶](#fn6) event.

Please note that the result `AVALC = "Y"` for confirmed clinical benefit
is defined by the first *two* events specified for `events`. For
subjects with observations fulfilling both events the one with the
earlier date should be selected (and not the first one in the list).
Thus `ignore_event_order = TRUE` is specified.

``` r
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
```

### Derive Non-standard Parameters

As
[`admiral::derive_extreme_event()`](https:/pharmaverse.github.io/admiral/v1.4.0/cran-release/reference/derive_extreme_event.html)
is very flexible, it is easy to implement non-standard parameters. Below
two examples for modified RECIST 1.1 parameters are shown.

#### Adding a Criterion for Confirmed Clinical Benefit

Confirmed clinical benefit was defined before as confirmed response or
CR, PR, SD, or NON-CR/NON-PD at least 42 days after randomization. Here
an alternative definition is implemented which considers PD more than 42
days after randomization as an additional criterion for clinical
benefit.

``` r
cb_y_pd <- event(
  description = paste(
    "Define PD occuring more than 42 days after",
    "randomization as clinical benefit"
  ),
  dataset_name = "ovr",
  condition = AVALC == "PD" & ADT > RANDDT + 42,
  set_values_to = exprs(AVALC = "Y")
)

adrs <- adrs %>%
  derive_extreme_event(
    by_vars = get_admiral_option("subject_keys"),
    order = exprs(desc(AVALC), ADT, event_nr),
    tmp_event_nr_var = event_nr,
    mode = "first",
    events = list(crsp_y_cr, crsp_y_pr, cb_y, cb_y_pd, no_data_n),
    source_datasets = list(
      ovr = ovr,
      adsl = adsl
    ),
    set_values_to = exprs(
      PARAMCD = "ACCB",
      PARAM = "Alternative Confirmed Clinical Benefit by Investigator",
      PARCAT1 = "Tumor Response",
      PARCAT2 = "Investigator",
      PARCAT3 = "RECIST 1.1",
      AVAL = yn_to_numeric(AVALC),
      ANL01FL = "Y"
    )
  )
```

#### Considering Non-Standard Response Values

Assume no evidence of disease (NED) is a valid value collected for
overall response. A new event (`bor_ned`) can be defined for this
response value and be added to the list of events (`events`) in the
[`admiral::derive_extreme_event()`](https:/pharmaverse.github.io/admiral/v1.4.0/cran-release/reference/derive_extreme_event.html)
call.

``` r
bor_ned <- event(
  description = paste(
    "Define no evidence of disease (NED) for best overall response (BOR) as NED",
    "occuring at least 42 days after randomization"
  ),
  dataset_name = "ovr",
  condition = AVALC == "NED" & ADT >= RANDDT + 42,
  set_values_to = exprs(AVALC = "NED")
)

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
    events = list(bor_cr, bor_pr, bor_sd, bor_non_crpd, bor_ned, bor_pd, bor_ne, no_data_missing),
    set_values_to = exprs(
      PARAMCD = "A1BOR",
      PARAM = paste(
        "Best Overall Response by Investigator (confirmation not required)",
        "- RECIST 1.1 adjusted for NED at Baseline"
      ),
      PARCAT1 = "Tumor Response",
      PARCAT2 = "Investigator",
      PARCAT3 = "RECIST 1.1 adjusted for NED at Baseline",
      AVAL = aval_resp(AVALC),
      ANL01FL = "Y"
    )
  )
```

### Derive Parameters using Independent Review Facility (IRF)/Blinded Independent Central Review (BICR) responses

All of the above steps can be repeated for different sets of records,
such as now using assessments from the IRF/BICR instead of investigator.
For this you would just need to replace the first steps with selecting
the required records, create the variables `AVALC`, `AVAL`, `ADT`,
`AVISIT`, `ANL01FL`, `ANL02FL` and the dataset `ovrb` (see
[Pre-processing of Input Records](#input)) and then feed these as input
to the downstream parameter functions.

``` r
adrs_bicr <- rs %>%
  filter(
    RSEVAL == "INDEPENDENT ASSESSOR" & RSACPTFL == "Y" & RSTESTCD == "OVRLRESP"
  ) %>%
  mutate(
    PARAMCD = "OVRB",
    PARAM = "Overall Response by BICR",
    PARCAT1 = "Tumor Response",
    PARCAT2 = "Blinded Independent Central Review",
    PARCAT3 = "RECIST 1.1"
  )
```

Then in all the calls to the parameter derivation functions you would
replace `ovr = ovr` with `ovr == ovrb` in the value of the
`source_datasets` argument.

### Derive Death Parameter

The function
[`admiral::derive_extreme_records()`](https:/pharmaverse.github.io/admiral/v1.4.0/cran-release/reference/derive_extreme_records.html)
can be used to create a new death parameter using death date from
`ADSL`. We need to restrict the columns from `ADSL` as we’ll merge all
required variables later across all our `ADRS` records.

``` r
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
      AVAL = yn_to_numeric(AVALC),
      ANL01FL = "Y",
      ADT = DTHDT
    )
  ) %>%
  select(-DTHDT)
```

### Derive Last Disease Assessment Parameters

The function
[`admiral::derive_extreme_records()`](https:/pharmaverse.github.io/admiral/v1.4.0/cran-release/reference/derive_extreme_records.html)
can be used to create a parameter for last disease assessment.

``` r
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
```

### Derive Measurable Disease at Baseline Parameter

The function
[`admiral::derive_param_exist_flag()`](https:/pharmaverse.github.io/admiral/v1.4.0/cran-release/reference/derive_param_exist_flag.html)
can be used to check whether a patient has measurable disease at
baseline, according to a company-specific condition. In this example we
check `TU` for target lesions during the baseline visit. We need to
restrict the columns from `ADSL` as we’ll merge all required variables
later across all our `ADRS` records.

``` r
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
```

### Assign `ASEQ`

The function
[`admiral::derive_var_obs_number()`](https:/pharmaverse.github.io/admiral/v1.4.0/cran-release/reference/derive_var_obs_number.html)
can be used to derive `ASEQ`. An example call is:

``` r
adrs <- adrs %>%
  derive_var_obs_number(
    by_vars = get_admiral_option("subject_keys"),
    order = exprs(PARAMCD, ADT, VISITNUM, RSSEQ),
    check_type = "error"
  )
```

### Add ADSL variables

If needed, the other `ADSL` variables can now be added. List of ADSL
variables already merged held in vector `adsl_vars`.

``` r
adrs <- adrs %>%
  derive_vars_merged(
    dataset_add = select(adsl, !!!negate_vars(adsl_vars)),
    by_vars = get_admiral_option("subject_keys")
  )
```

## Example Script

| ADaM | Sample Code                                                 |
|------|-------------------------------------------------------------|
| ADRS | `admiral::use_ad_template("ADRS", package = "admiralonco")` |

------------------------------------------------------------------------

1.  ``` downlit
    rsp_y
    #> <event> object
    #> description: "Define CR or PR as (unconfirmed) response"
    #> dataset_name: "ovr"
    #> condition: AVALC %in% c("CR", "PR")
    #> mode: NULL
    #> order: NULL
    #> set_values_to:
    #>   AVALC: "Y"
    #> keep_source_vars: NULL
    ```

2.  ``` downlit
    cb_y
    #> <event> object
    #> description: "Define CR, PR, SD, or NON-CR/NON-PD occuring at least 42 days after randomization as clinical benefit"
    #> dataset_name: "ovr"
    #> condition: AVALC %in% c("CR", "PR", "SD", "NON-CR/NON-PD") & ADT >= RANDDT + 
    #>     42
    #> mode: NULL
    #> order: NULL
    #> set_values_to:
    #>   AVALC: "Y"
    #> keep_source_vars: NULL
    ```

3.  ``` downlit
    crsp_y_cr
    #> <event_joined> object
    #> description: "Define confirmed response as CR followed by CR at least 21 days later and at most one NE in between"
    #> dataset_name: "ovr"
    #> condition: AVALC == "CR" & all(AVALC.join %in% c("CR", "NE")) & count_vals(var = AVALC.join, 
    #>     val = "NE") <= 1
    #> order:
    #>   ADT
    #> join_vars:
    #>   AVALC
    #>   ADT
    #> join_type: "after"
    #> first_cond_lower: NULL
    #> first_cond_upper: AVALC.join == "CR" & ADT.join >= ADT + days(confirmation_period)
    #> set_values_to:
    #>   AVALC: "Y"
    #> keep_source_vars: NULL
    ```

4.  ``` downlit
    crsp_y_pr
    #> <event_joined> object
    #> description: "Define confirmed response as PR followed by CR or PR at least 21 days later, at most one NE in between, and no PR after CR"
    #> dataset_name: "ovr"
    #> condition: AVALC == "PR" & all(AVALC.join %in% c("CR", "PR", "NE")) & count_vals(var = AVALC.join, 
    #>     val = "NE") <= 1 & (min_cond(var = ADT.join, cond = AVALC.join == 
    #>     "CR") > max_cond(var = ADT.join, cond = AVALC.join == "PR") | 
    #>     count_vals(var = AVALC.join, val = "CR") == 0 | count_vals(var = AVALC.join, 
    #>     val = "PR") == 0)
    #> order:
    #>   ADT
    #> join_vars:
    #>   AVALC
    #>   ADT
    #> join_type: "after"
    #> first_cond_lower: NULL
    #> first_cond_upper: AVALC.join %in% c("CR", "PR") & ADT.join >= ADT + days(confirmation_period)
    #> set_values_to:
    #>   AVALC: "Y"
    #> keep_source_vars: NULL
    ```

5.  ``` downlit
    cbor_cr
    #> <event_joined> object
    #> description: "Define complete response (CR) for confirmed best overall response (CBOR) as CR followed by CR at least 21 days later and at most one NE in between"
    #> dataset_name: "ovr"
    #> condition: AVALC == "CR" & all(AVALC.join %in% c("CR", "NE")) & count_vals(var = AVALC.join, 
    #>     val = "NE") <= 1
    #> order: NULL
    #> join_vars:
    #>   AVALC
    #>   ADT
    #> join_type: "after"
    #> first_cond_lower: NULL
    #> first_cond_upper: AVALC.join == "CR" & ADT.join >= ADT + confirmation_period
    #> set_values_to:
    #>   AVALC: "CR"
    #> keep_source_vars: NULL
    ```

6.  ``` downlit
    cbor_pr
    #> <event_joined> object
    #> description: "Define partial response (PR) for confirmed best overall response (CBOR) as PR followed by CR or PR at least 21 28 days later, at most one NE in between, and no PR after CR"
    #> dataset_name: "ovr"
    #> condition: AVALC == "PR" & all(AVALC.join %in% c("CR", "PR", "NE")) & count_vals(var = AVALC.join, 
    #>     val = "NE") <= 1 & (min_cond(var = ADT.join, cond = AVALC.join == 
    #>     "CR") > max_cond(var = ADT.join, cond = AVALC.join == "PR") | 
    #>     count_vals(var = AVALC.join, val = "CR") == 0 | count_vals(var = AVALC.join, 
    #>     val = "PR") == 0)
    #> order: NULL
    #> join_vars:
    #>   AVALC
    #>   ADT
    #> join_type: "after"
    #> first_cond_lower: NULL
    #> first_cond_upper: AVALC.join %in% c("CR", "PR") & ADT.join >= ADT + confirmation_period
    #> set_values_to:
    #>   AVALC: "PR"
    #> keep_source_vars: NULL
    ```
