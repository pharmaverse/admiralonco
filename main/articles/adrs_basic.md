# Creating a Basic ADRS

## Important Note

Many of the functions used in this vignette are deprecated. Thus it is
recommended to use [Creating ADRS (Including Non-standard
Endpoints)](https:/pharmaverse.github.io/admiralonco/main/articles/adrs.md)
instead.

## Introduction

This article describes creating an `ADRS` ADaM with common oncology
endpoint parameters based on RECIST v1.1. Therefore response values are
expected as either `CR`, `PR`, `SD`, `NON-CR/NON-PD`, `PD` or `NE`.

For confirmation of response particularly, `CR`, the case that `CR` is
followed by `PR` (or `SD`) is considered as a data issue and the
derivations of the parameters don’t handle this case specially. The
[admiralonco](https://pharmaverse.github.io/admiralonco/) functions
don’t provide functionality to handle this case. It is recommended to
fix the issue in the source data, e.g., by changing the `PR` to `PD`
rather than handling it in the parameter derivations. This ensures
consistency across parameters. The functions
[`derive_param_confirmed_bor()`](https:/pharmaverse.github.io/admiralonco/main/reference/derive_param_confirmed_bor.md)
and
[`derive_param_confirmed_resp()`](https:/pharmaverse.github.io/admiralonco/main/reference/derive_param_confirmed_resp.md)
issue a warning if `CR` is followed by `PR` (the warning does not
display if `CR` is followed by `SD`).

Please note that this vignette describes the endpoints which were
considered by the admiralonco team as the most common ones. The
admiralonco functions used to derive these endpoints provide a certain
flexibility, e.g., specifying the reference date or time windows for
confirmation or stable disease. If different endpoints or more
flexibility is required please read [Creating ADRS (Including
Non-standard
Endpoints)](https:/pharmaverse.github.io/admiralonco/main/articles/adrs.md).

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
(ordered from best to worst response). This ordering is already covered
within our RECIST v1.1 parameter derivation functions, and so changing
`AVAL` here would not change the result of those derivations.

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
Date](https:/pharmaverse.github.io/admiralonco/main/articles/nactdt.md)
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

Note here that we don’t filter out records after first `PD` at this
stage, as that is specifically catered for in the
[admiralonco](https://pharmaverse.github.io/admiralonco/) parameter
derivation functions in the below steps, via `source_pd` arguments.

#### Flag Assessments up to First PD (`ANL02FL`)

However, if you prefer not to rely on `source_pd` arguments, then the
user is free to filter out records after first `PD` at this stage in a
similar way via a `ANLzzFL` flag, and then you could leave `source_pd`
as null in all downstream parameter derivation function calls. So, for
example the user could create `ANL02FL` flag to subset the post-baseline
response data up to and including first reported progressive disease.
This would be an alternative and transparent method to the use of
`source_pd` argument approach to create ADRS parameters below. Using
[admiral](https://pharmaverse.github.io/admiral/) function
[`admiral::derive_var_relative_flag()`](https:/pharmaverse.github.io/admiral/v1.4.1/cran-release/reference/derive_var_relative_flag.html)
we could create `ANL02FL` as below.

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

### Derive Progressive Disease Parameter

Now that we have the input records prepared above with any
company-specific requirements, we can start to derive new parameter
records. For the parameter derivations, all values except those
overwritten by `set_values_to` argument are kept from the earliest
occurring input record fulfilling the required criteria.

The function
[`admiral::derive_extreme_records()`](https:/pharmaverse.github.io/admiral/v1.4.1/cran-release/reference/derive_extreme_records.html)
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
[`admiral::derive_var_extreme_dt()`](https:/pharmaverse.github.io/admiral/v1.4.1/cran-release/reference/derive_var_extreme_dt.html)
could be used to find the date of first `PD` as a variable, rather than
as a new parameter record. All the parameter derivation functions that
use these dates are flexible to allow sourcing these from any input
source using
[`admiral::date_source()`](https:/pharmaverse.github.io/admiral/v1.4.1/cran-release/reference/date_source.html).
See examples below.

### Derive Response Parameter

The next required step is to define the source location for this newly
derived `PD` date.

``` r
pd <- date_source(
  dataset_name = "adrs",
  date = ADT,
  filter = PARAMCD == "PD" & AVALC == "Y"
)
#> `date_source()` was deprecated in admiralonco 1.4.0.
#> ✖ This message will turn into a warning {at the beginning of 2027}.
#> ℹ See admiral's deprecation guidance:
#>   https://pharmaverse.github.io/admiraldev/dev/articles/programming_strategy.html#deprecation
```

An equivalent example if using `ADSL` instead could be as follows (where
`PDDT` would be pre-derived as first date of progressive disease).

``` r
pd <- date_source(
  dataset_name = "adsl",
  date = PDDT
)
```

The function
[`derive_param_response()`](https:/pharmaverse.github.io/admiralonco/main/reference/derive_param_response.md)
can then be used to find the date of first response. This differs from
the
[`admiral::derive_extreme_records()`](https:/pharmaverse.github.io/admiral/v1.4.1/cran-release/reference/derive_extreme_records.html)
function in that it only looks for events occurring prior to first `PD`.
In the below example, the response condition has been defined as `CR` or
`PR`.

``` r
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
#> `derive_param_response()` was deprecated in admiralonco 1.4.
#> ℹ Please use `admiral::derive_extreme_event()` instead.
#> ✖ This message will turn into a warning at the beginning of 2027.
#> ℹ See admiral's deprecation guidance:
#>   https://pharmaverse.github.io/admiraldev/dev/articles/programming_strategy.html#deprecation
```

### Derive Clinical Benefit Parameter

Similarly, we now define the source location for this newly derived
first response date.

``` r
resp <- date_source(
  dataset_name = "adrs",
  date = ADT,
  filter = PARAMCD == "RSP" & AVALC == "Y"
)
```

The function
[`derive_param_clinbenefit()`](https:/pharmaverse.github.io/admiralonco/main/reference/derive_param_clinbenefit.md)
can then be used to derive the clinical benefit parameter, which we
define as a patient having had a response or a sustained period of time
before first `PD`. This could also be known as disease control. In this
example the “sustained period” has been defined as 42 days after
randomization date, using the `ref_start_window` argument.

``` r
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
#> `derive_param_clinbenefit()` was deprecated in admiralonco 1.4.
#> ℹ Please use `admiral::derive_extreme_event()` instead.
#> ✖ This message will turn into a warning at the beginning of 2027.
#> ℹ See admiral's deprecation guidance:
#>   https://pharmaverse.github.io/admiraldev/dev/articles/programming_strategy.html#deprecation
```

### Derive Best Overall Response Parameter

The function
[`derive_param_bor()`](https:/pharmaverse.github.io/admiralonco/main/reference/derive_param_bor.md)
can be used to derive the best overall response (without confirmation
required) parameter. Similar to the above function you can optionally
decide what period would you consider a `SD` or `NON-CR/NON-PD` as being
eligible from. In this example, 42 days after randomization date has
been used again.

``` r
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
#> `derive_param_bor()` was deprecated in admiralonco 1.4.
#> ℹ Please use `admiral::derive_extreme_event()` instead.
#> ✖ This message will turn into a warning at the beginning of 2027.
#> ℹ See admiral's deprecation guidance:
#>   https://pharmaverse.github.io/admiraldev/dev/articles/programming_strategy.html#deprecation
```

Note that the above gives pre-defined `AVAL` values (defined by
[`aval_resp()`](https:/pharmaverse.github.io/admiralonco/main/reference/aval_resp.md))
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
[`derive_param_bor()`](https:/pharmaverse.github.io/admiralonco/main/reference/derive_param_bor.md)
call. Be aware that this will only impact the `AVAL` mapping, not the
derivation of BOR in any way - as the function derivation relies only on
`AVALC` here.

### Derive Best Overall Response of CR/PR Parameter

The function
[`admiral::derive_extreme_records()`](https:/pharmaverse.github.io/admiral/v1.4.1/cran-release/reference/derive_extreme_records.html)
can be used to check if a patient had a response for BOR.

``` r
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
```

### Derive Response Parameters requiring Confirmation

Any of the above response parameters can be repeated for “confirmed”
responses only. For these the functions
[`derive_param_confirmed_resp()`](https:/pharmaverse.github.io/admiralonco/main/reference/derive_param_confirmed_resp.md)
and
[`derive_param_confirmed_bor()`](https:/pharmaverse.github.io/admiralonco/main/reference/derive_param_confirmed_bor.md)
can be used. Some of the other functions from above can then be re-used
passing in these confirmed response records. See the examples below of
derived parameters requiring confirmation. The assessment and the
confirmatory assessment here need to occur at least 28 days apart
*(without any +1 applied to this calculation of days between visits)*,
using the `ref_confirm` argument.

``` r
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
#> `derive_param_confirmed_resp()` was deprecated in admiralonco 1.4.
#> ℹ Please use `admiral::derive_extreme_event()` instead.
#> ✖ This message will turn into a warning at the beginning of 2027.
#> ℹ See admiral's deprecation guidance:
#>   https://pharmaverse.github.io/admiraldev/dev/articles/programming_strategy.html#deprecation

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
#> `derive_param_confirmed_bor()` was deprecated in admiralonco 1.4.
#> ℹ Please use `admiral::derive_extreme_event()` instead.
#> ✖ This message will turn into a warning at the beginning of 2027.
#> ℹ See admiral's deprecation guidance:
#>   https://pharmaverse.github.io/admiraldev/dev/articles/programming_strategy.html#deprecation
```

### Derive Parameters using Independent Review Facility (IRF)/Blinded Independent Central Review (BICR) responses

All of the above steps can be repeated for different sets of records,
such as now using assessments from the IRF/BICR instead of investigator.
For this you would just need to replace the first steps with selecting
the required records, and then feed these as input to the downstream
parameter functions.

Remember that a new progressive disease and response source object would
be required for passing to `source_pd` and `source_resp` respectively.

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
replace the `PARAMCD == "OVR"` source with `PARAMCD == "OVRR1"`.

### Derive Death Parameter

The function
[`admiral::derive_extreme_records()`](https:/pharmaverse.github.io/admiral/v1.4.1/cran-release/reference/derive_extreme_records.html)
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
[`admiral::derive_extreme_records()`](https:/pharmaverse.github.io/admiral/v1.4.1/cran-release/reference/derive_extreme_records.html)
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
[`admiral::derive_param_exist_flag()`](https:/pharmaverse.github.io/admiral/v1.4.1/cran-release/reference/derive_param_exist_flag.html)
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
[`admiral::derive_var_obs_number()`](https:/pharmaverse.github.io/admiral/v1.4.1/cran-release/reference/derive_var_obs_number.html)
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

| ADaM       | Sample Code                                                       |
|------------|-------------------------------------------------------------------|
| ADRS_BASIC | `admiral::use_ad_template("ADRS_BASIC", package = "admiralonco")` |
