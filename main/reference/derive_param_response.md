# Adds a Parameter Indicating If a Subject Had a Response before Progressive Disease

**\[superseded\]** The `derive_param_response()` function has been
superseded in favor of
[`derive_extreme_event()`](https:/pharmaverse.github.io/admiral/v1.3.1/cran-release/reference/derive_extreme_event.html).

Adds a parameter indicating if a response has been observed. If a
response has been observed, `AVALC` is set to "Y", `AVAL` to 1 and `ADT`
is set to the first date when a response has been observed. If a
response has not been observed, `AVALC` is set to "N", `AVAL` to 0 and
`ADT` is set NA.

## Usage

``` r
derive_param_response(
  dataset,
  dataset_adsl,
  filter_source,
  source_pd = NULL,
  source_datasets = NULL,
  set_values_to,
  aval_fun,
  subject_keys = get_admiral_option("subject_keys")
)
```

## Arguments

- dataset:

  Input dataset

  The variables specified by the `subject_keys`and `ADT` are expected.

  After applying `filter_source` and/or `source_pd` the variable `ADT`
  and the variables specified by `subject_keys` must be a unique key of
  the dataset.

- dataset_adsl:

  Input dataset

  - The variables specified for `subject_keys` are expected.

  - For each observation of the specified dataset a new observation is
    added to the input dataset. This is to capture those patients that
    may never have had a tumor assessment.

- filter_source:

  Source filter

  All observations in the `dataset` data fulfilling the specified
  condition are selected.

- source_pd:

  Sources and conditions defining the end of the assessment period for
  the responses.

  An object of type `date_source` is expected

  All observations in `dataset` defining the response data fulfilling
  the `filter_source` condition are considered as response if they fall
  before the end of the assessment period as defined by `source_pd`.

  - For subjects with at least one response before the end of the
    assessment period, `AVALC` is set to `"Y"`, `AVAL` to `1`, and `ADT`
    to the first date when the response occurred.

  - For all other subjects `AVALC` is set to `"N"`, `AVAL` to `0`, and
    `ADT` to `NA`.

- source_datasets:

  Source dataset

  A named list of datasets with one element is expected (e.g.
  `list(adrs= adrs)`).

  The name must match the `dataset_name` field of the
  [`admiral::date_source()`](https:/pharmaverse.github.io/admiral/v1.3.1/cran-release/reference/date_source.html)
  object specified for `source_pd`.

  The variables specified by the `subject_keys` argument and the `date`
  field of the
  [`admiral::date_source()`](https:/pharmaverse.github.io/admiral/v1.3.1/cran-release/reference/date_source.html)
  object are expected in the dataset.

- set_values_to:

  Variables to set

  A named list returned by
  [`exprs()`](https://rlang.r-lib.org/reference/defusing-advanced.html)
  defining the variables to be set for the new parameter, e.g.
  `exprs(PARAMCD = "RSP", PARAM = "Response by investigator")` is
  expected.

  The values must be symbols, character strings, numeric values or `NA`.

- aval_fun:

  *Deprecated*, please use `set_values_to` instead.

  Function to map character analysis value (`AVALC`) to numeric analysis
  value (`AVAL`)

  The (first) argument of the function must expect a character vector
  and the function must return a numeric vector.

- subject_keys:

  Variables to uniquely identify a subject

  A list of symbols created using
  [`exprs()`](https://rlang.r-lib.org/reference/defusing-advanced.html)
  is expected.

## Value

The input dataset with a new parameter indicating if and when a response
occurred

## Details

1.  The Date of the end of the assessment period (e.g. Progressive
    disease, as defined by `pd_source`) is added to the response
    dataset.

2.  The response dataset is restricted to observations occurring before
    **or on** the date of progressive disease.

3.  For each subject (with respect to the variables specified for the
    `subject_keys` parameter), the first observation (with respect to
    `ADT`) where the response condition (`filter_source` parameter) is
    fulfilled is selected.

4.  For each observation in `dataset_adsl` a new observation is created.

    - For subjects with a response `AVALC` is set to `"Y"`, `AVAL` to
      `1`, and `ADT` to the first date (`ADT`) where the response
      condition is fulfilled.

    - For all other subjects `AVALC` is set to `"N"`, `AVAL` to `0` and
      `ADT` to `NA`.

5.  The variables specified by the `set_values_to` parameter are added
    to the new observations.

6.  The new observations are added to input dataset.

## See also

Other superseded:
[`derive_param_bor()`](https:/pharmaverse.github.io/admiralonco/main/reference/derive_param_bor.md),
[`derive_param_clinbenefit()`](https:/pharmaverse.github.io/admiralonco/main/reference/derive_param_clinbenefit.md),
[`derive_param_confirmed_bor()`](https:/pharmaverse.github.io/admiralonco/main/reference/derive_param_confirmed_bor.md),
[`derive_param_confirmed_resp()`](https:/pharmaverse.github.io/admiralonco/main/reference/derive_param_confirmed_resp.md),
[`filter_pd()`](https:/pharmaverse.github.io/admiralonco/main/reference/filter_pd.md)

## Author

Samia Kabi

## Examples

``` r
library(dplyr)
library(admiral)
# ensure that `date_source()` from admiralonco is used to avoid deprecation
# warning
unloadNamespace("admiralonco")
library(admiralonco)
#> 
#> Attaching package: ‘admiralonco’
#> The following objects are masked from ‘package:admiral’:
#> 
#>     date_source, death_event, lastalive_censor
library(lubridate)
library(tibble)

adsl <- tribble(
  ~USUBJID,
  "1",
  "2",
  "3",
  "4"
) %>%
  mutate(STUDYID = "XX1234")

adrs <- tribble(
  ~USUBJID, ~PARAMCD, ~ADTC,         ~AVALC, ~ANL01FL,
  "1",      "OVR",    "2020-01-02",  "PR",   "Y",
  "1",      "OVR",    "2020-02-01",  "CR",   "Y",
  "1",      "OVR",    "2020-03-01",  "CR",   "Y",
  "1",      "OVR",    "2020-04-01",  "SD",   "Y",
  "1",      "PD",     NA_character_, "N",    "Y",
  "2",      "OVR",    "2021-06-15",  "SD",   "Y",
  "2",      "OVR",    "2021-07-16",  "PD",   "Y",
  "2",      "OVR",    "2021-09-14",  "PD",   "Y",
  "2",      "PD",     "2021-09-14",  "Y",    "Y",
  "3",      "OVR",    "2021-09-14",  "SD",   "Y",
  "3",      "OVR",    "2021-10-30",  "PD",   "Y",
  "3",      "OVR",    "2021-12-25",  "CR",   "Y",
  "3",      "PD",     "2021-10-30",  "Y",    "Y"
) %>%
  mutate(
    STUDYID = "XX1234",
    ADT = ymd(ADTC),
    ANL01FL = "Y"
  ) %>%
  select(-ADTC)

# Define the end of the assessment period for responses:
# all responses before or on the first PD will be used.
pd <- date_source(
  dataset_name = "adrs",
  date = ADT,
  filter = PARAMCD == "PD" & AVALC == "Y"
)
# Derive the response parameter
derive_param_response(
  dataset = adrs,
  dataset_adsl = adsl,
  filter_source = PARAMCD == "OVR" & AVALC %in% c("CR", "PR") & ANL01FL == "Y",
  source_pd = pd,
  source_datasets = list(adrs = adrs),
  set_values_to = exprs(
    AVAL = yn_to_numeric(AVALC),
    PARAMCD = "RSP",
    PARAM = "Response by investigator"
  ),
  subject_keys = get_admiral_option("subject_keys")
) %>%
  arrange(USUBJID, PARAMCD, ADT)
#> # A tibble: 17 × 8
#>    USUBJID PARAMCD AVALC ANL01FL STUDYID ADT         AVAL PARAM                 
#>    <chr>   <chr>   <chr> <chr>   <chr>   <date>     <dbl> <chr>                 
#>  1 1       OVR     PR    Y       XX1234  2020-01-02    NA NA                    
#>  2 1       OVR     CR    Y       XX1234  2020-02-01    NA NA                    
#>  3 1       OVR     CR    Y       XX1234  2020-03-01    NA NA                    
#>  4 1       OVR     SD    Y       XX1234  2020-04-01    NA NA                    
#>  5 1       PD      N     Y       XX1234  NA            NA NA                    
#>  6 1       RSP     Y     Y       XX1234  2020-01-02     1 Response by investiga…
#>  7 2       OVR     SD    Y       XX1234  2021-06-15    NA NA                    
#>  8 2       OVR     PD    Y       XX1234  2021-07-16    NA NA                    
#>  9 2       OVR     PD    Y       XX1234  2021-09-14    NA NA                    
#> 10 2       PD      Y     Y       XX1234  2021-09-14    NA NA                    
#> 11 2       RSP     N     NA      XX1234  NA             0 Response by investiga…
#> 12 3       OVR     SD    Y       XX1234  2021-09-14    NA NA                    
#> 13 3       OVR     PD    Y       XX1234  2021-10-30    NA NA                    
#> 14 3       OVR     CR    Y       XX1234  2021-12-25    NA NA                    
#> 15 3       PD      Y     Y       XX1234  2021-10-30    NA NA                    
#> 16 3       RSP     N     NA      XX1234  NA             0 Response by investiga…
#> 17 4       RSP     N     NA      XX1234  NA             0 Response by investiga…
```
