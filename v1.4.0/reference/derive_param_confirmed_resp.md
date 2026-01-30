# Adds a Parameter for Confirmed Response

**\[deprecated\]** The `derive_param_confirmed_resp()` function has been
superseded in favor of
[`derive_extreme_event()`](https:/pharmaverse.github.io/admiral/v1.4.0/cran-release/reference/derive_extreme_event.html).

Adds a parameter for confirmed response

## Usage

``` r
derive_param_confirmed_resp(
  dataset,
  dataset_adsl,
  filter_source,
  source_pd = NULL,
  source_datasets = NULL,
  ref_confirm,
  max_nr_ne = 1,
  accept_sd = FALSE,
  aval_fun,
  set_values_to,
  subject_keys = get_admiral_option("subject_keys")
)
```

## Arguments

- dataset:

  Input dataset

  The `PARAMCD`, `ADT`, and `AVALC` variables and the variables
  specified by `subject_keys` and `reference_date` are expected.

  After applying `filter_source` and/or `source_pd` the variable `ADT`
  and the variables specified by `subject_keys` must be a unique key of
  the dataset.

- dataset_adsl:

  ADSL input dataset

  The variables specified for `subject_keys` are expected. For each
  subject of the specified dataset a new observation is added to the
  input dataset.

- filter_source:

  Source filter

  All observations in `dataset_source` fulfilling the specified
  condition are considered for deriving the confirmed response.

- source_pd:

  Date of first progressive disease (PD)

  If the parameter is specified, the observations of the input dataset
  for deriving the new parameter are restricted to observations up to
  the specified date. Observations at the specified date are included.
  For subjects without first PD date all observations are take into
  account.

  *Permitted Values:* a `date_source` object (see
  [`admiral::date_source()`](https:/pharmaverse.github.io/admiral/v1.4.0/cran-release/reference/date_source.html)
  for details)

- source_datasets:

  Source dataset for the first PD date

  A named list of datasets is expected. It links the `dataset_name` from
  `source_pd` with an existing dataset.

  For example if `source_pd = pd_date` with

      pd_date <- date_source(
        dataset_name = "adrs",
        date = ADT,
        filter = PARAMCD == PD
      )

  and the actual response dataset in the script is `myadrs`,
  `source_datasets = list(adrs = myadrs)` should be specified.

- ref_confirm:

  Minimum time period for confirmation

  The assessment and the confirmatory assessment for `"CR"` and `"PR"`
  have to be at least the specified number of days apart.

- max_nr_ne:

  The specified number of `"NE"` assessments between the assessment and
  the confirmatory assessment for `"CR"` and `"PR"` response is
  accepted.

  *Permitted Values:* a non-negative numeric scalar

- accept_sd:

  Accept `"SD"` for `"PR"`?

  If the argument is set to `TRUE`, one `"SD"` assessment between the
  assessment and the confirmatory assessment for `"PR"` response is
  accepted. Otherwise, no `"SD"` assessment must occur between the two
  assessments.

  *Permitted Values:* a logical scalar

- aval_fun:

  **\[deprecated\]** Please use `set_values_to` instead.

  Function to map character analysis value (`AVALC`) to numeric analysis
  value (`AVAL`)

  The (first) argument of the function must expect a character vector
  and the function must return a numeric vector.

- set_values_to:

  Variables to set

  A named list returned by
  [`exprs()`](https://rlang.r-lib.org/reference/defusing-advanced.html)
  defining the variables to be set for the new parameter, e.g.
  `exprs(PARAMCD = "CRSP", PARAM = "Confirmed Response")` is expected.
  The values must be symbols, character strings, numeric values, or
  `NA`.

- subject_keys:

  Variables to uniquely identify a subject

  A list of symbols created using
  [`exprs()`](https://rlang.r-lib.org/reference/defusing-advanced.html)
  is expected.

## Value

The input dataset with a new parameter for confirmed response

## Details

1.  The input dataset (`dataset`) is restricted to the observations
    matching `filter_source` and to observations before or at the date
    specified by `source_pd`.

2.  A subject is considered as responder if there is at least one
    observation in the restricted dataset with

    - `AVALC == "CR"`,

    - there is a confirmatory assessment with `AVALC == "CR"` at least
      `ref_confirm` days after the assessment,

    - all assessments between the assessment and the confirmatory
      assessment are `"CR"` or `"NE"`, and

    - there are at most `max_nr_ne` `"NE"` assessments between the
      assessment and the confirmatory assessment.

    or at least one observation with

    - `AVALC == "PR"`,

    - there is a confirmatory assessment with `AVALC %in% c("CR", "PR")`
      at least `ref_confirm` days after the assessment,

    - all assessments between the assessment and the confirmatory
      assessment are `"CR"`, `"PR"`, `"SD"`, or `"NE"`,

    - there is no `"PR"` assessment after a `"CR"` assessment in the
      confirmation period,

    - there are at most `max_nr_ne` `"NE"` assessments between the
      assessment and the confirmatory assessment,

    - if the `accept_sd` argument is set to `TRUE`, one `"SD"`
      assessment in the confirmation period is accepted. Otherwise, no
      `"SD"` assessment must occur within the confirmation period.

3.  For responders `AVALC` is set to `"Y"` and `ADT` to the first date
    where the response criteria are fulfilled. For all other subjects in
    `dataset_adsl` `AVALC` is set to `"N"` and `ADT` to `NA`.

4.  The variables specified by the `set_values_to` parameter are added
    to the new observations.

5.  The new observations are added to input dataset.

## See also

Other deprecated:
[`date_source()`](https:/pharmaverse.github.io/admiralonco/v1.4.0/reference/date_source.md),
[`derive_param_bor()`](https:/pharmaverse.github.io/admiralonco/v1.4.0/reference/derive_param_bor.md),
[`derive_param_clinbenefit()`](https:/pharmaverse.github.io/admiralonco/v1.4.0/reference/derive_param_clinbenefit.md),
[`derive_param_confirmed_bor()`](https:/pharmaverse.github.io/admiralonco/v1.4.0/reference/derive_param_confirmed_bor.md),
[`derive_param_response()`](https:/pharmaverse.github.io/admiralonco/v1.4.0/reference/derive_param_response.md),
[`filter_pd()`](https:/pharmaverse.github.io/admiralonco/v1.4.0/reference/filter_pd.md)

## Author

Stefan Bundfuss

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

# Create ADSL dataset
adsl <- tibble::tribble(
  ~USUBJID, ~TRTSDTC,
  "1",      "2020-01-01",
  "2",      "2019-12-12",
  "3",      "2019-11-11",
  "4",      "2019-12-30",
  "5",      "2020-01-01",
  "6",      "2020-02-02",
  "7",      "2020-02-02",
  "8",      "2020-04-01",
  "9",      "2020-03-01"
) %>%
  mutate(
    STUDYID = "XX1234"
  )

# Create ADRS dataset
ovr_obs <- tibble::tribble(
  ~USUBJID, ~ADTC,        ~AVALC,
  "1",      "2020-01-01", "PR",
  "1",      "2020-02-01", "CR",
  "1",      "2020-02-16", "NE",
  "1",      "2020-03-01", "CR",
  "1",      "2020-04-01", "SD",
  "2",      "2020-01-01", "SD",
  "2",      "2020-02-01", "PR",
  "2",      "2020-03-01", "SD",
  "2",      "2020-03-13", "CR",
  "3",      "2019-11-12", "CR",
  "3",      "2019-12-02", "CR",
  "3",      "2020-01-01", "SD",
  "4",      "2020-01-01", "PR",
  "4",      "2020-03-01", "SD",
  "4",      "2020-04-01", "SD",
  "4",      "2020-05-01", "PR",
  "4",      "2020-05-15", "NON-CR/NON-PD",
  "5",      "2020-01-01", "PR",
  "5",      "2020-01-10", "SD",
  "5",      "2020-01-20", "PR",
  "5",      "2020-05-15", "NON-CR/NON-PD",
  "6",      "2020-02-06", "PR",
  "6",      "2020-02-16", "CR",
  "6",      "2020-03-30", "PR",
  "6",      "2020-04-12", "PD",
  "6",      "2020-05-01", "CR",
  "6",      "2020-06-01", "CR",
  "7",      "2020-02-06", "PR",
  "7",      "2020-02-16", "CR",
  "7",      "2020-04-01", "NE",
  "9",      "2020-03-16", "CR",
  "9",      "2020-04-01", "NE",
  "9",      "2020-04-16", "NE",
  "9",      "2020-05-01", "CR"
) %>%
  mutate(PARAMCD = "OVR", ANL01FL = "Y")

pd_obs <-
  bind_rows(tibble::tribble(
    ~USUBJID, ~ADTC,        ~AVALC,
    "6",      "2020-04-12", "Y"
  ) %>%
    mutate(PARAMCD = "PD", ANL01FL = "Y"))

adrs <- bind_rows(ovr_obs, pd_obs) %>%
  mutate(
    ADT = lubridate::ymd(ADTC),
    STUDYID = "XX1234"
  ) %>%
  select(-ADTC)

pd_date <- date_source(
  dataset_name = "adrs",
  date = ADT,
  filter = PARAMCD == "PD" & ANL01FL == "Y"
)

# Derive confirmed response parameter
derive_param_confirmed_resp(
  adrs,
  dataset_adsl = adsl,
  filter_source = PARAMCD == "OVR" & ANL01FL == "Y",
  source_pd = pd_date,
  source_datasets = list(adrs = adrs),
  ref_confirm = 28,
  set_values_to = exprs(
    PARAMCD = "CRSP",
    PARAM = "Confirmed Response by Investigator"
  )
) %>%
  filter(PARAMCD == "CRSP")
#> `derive_param_confirmed_resp()` was deprecated in admiralonco 1.4.
#> ℹ Please use `admiral::derive_extreme_event()` instead.
#> ✖ This message will turn into a warning at the beginning of 2027.
#> ℹ See admiral's deprecation guidance:
#>   https://pharmaverse.github.io/admiraldev/dev/articles/programming_strategy.html#deprecation
#> Warning: Dataset contains CR records followed by PR.
#> Run `get_crpr_dataset()` to access the CR records records followed by PR
#> # A tibble: 9 × 7
#>   USUBJID AVALC PARAMCD ANL01FL ADT        STUDYID PARAM                        
#>   <chr>   <chr> <chr>   <chr>   <date>     <chr>   <chr>                        
#> 1 1       Y     CRSP    Y       2020-01-01 XX1234  Confirmed Response by Invest…
#> 2 2       N     CRSP    NA      NA         XX1234  Confirmed Response by Invest…
#> 3 3       N     CRSP    NA      NA         XX1234  Confirmed Response by Invest…
#> 4 4       N     CRSP    NA      NA         XX1234  Confirmed Response by Invest…
#> 5 5       N     CRSP    NA      NA         XX1234  Confirmed Response by Invest…
#> 6 6       N     CRSP    NA      NA         XX1234  Confirmed Response by Invest…
#> 7 7       N     CRSP    NA      NA         XX1234  Confirmed Response by Invest…
#> 8 8       N     CRSP    NA      NA         XX1234  Confirmed Response by Invest…
#> 9 9       N     CRSP    NA      NA         XX1234  Confirmed Response by Invest…

# Derive confirmed response parameter (accepting SD for PR and two NEs)
derive_param_confirmed_resp(
  adrs,
  dataset_adsl = adsl,
  filter_source = PARAMCD == "OVR" & ANL01FL == "Y",
  source_pd = pd_date,
  source_datasets = list(adrs = adrs),
  ref_confirm = 28,
  max_nr_ne = 2,
  accept_sd = TRUE,
  set_values_to = exprs(
    PARAMCD = "CRSP",
    PARAM = "Confirmed Response by Investigator"
  )
) %>%
  filter(PARAMCD == "CRSP")
#> Warning: Dataset contains CR records followed by PR.
#> Run `get_crpr_dataset()` to access the CR records records followed by PR
#> # A tibble: 9 × 7
#>   USUBJID AVALC PARAMCD ANL01FL ADT        STUDYID PARAM                        
#>   <chr>   <chr> <chr>   <chr>   <date>     <chr>   <chr>                        
#> 1 1       Y     CRSP    Y       2020-01-01 XX1234  Confirmed Response by Invest…
#> 2 2       Y     CRSP    Y       2020-02-01 XX1234  Confirmed Response by Invest…
#> 3 3       N     CRSP    NA      NA         XX1234  Confirmed Response by Invest…
#> 4 4       N     CRSP    NA      NA         XX1234  Confirmed Response by Invest…
#> 5 5       N     CRSP    NA      NA         XX1234  Confirmed Response by Invest…
#> 6 6       N     CRSP    NA      NA         XX1234  Confirmed Response by Invest…
#> 7 7       N     CRSP    NA      NA         XX1234  Confirmed Response by Invest…
#> 8 8       N     CRSP    NA      NA         XX1234  Confirmed Response by Invest…
#> 9 9       Y     CRSP    Y       2020-03-16 XX1234  Confirmed Response by Invest…
```
