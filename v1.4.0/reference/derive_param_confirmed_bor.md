# Adds a Parameter for Confirmed Best Overall Response

**\[deprecated\]** The `derive_param_confirmed_bor()` function has been
superseded in favor of
[`derive_extreme_event()`](https:/pharmaverse.github.io/admiral/v1.4.0/cran-release/reference/derive_extreme_event.html).

Adds a parameter for confirmed best overall response (BOR)

## Usage

``` r
derive_param_confirmed_bor(
  dataset,
  dataset_adsl,
  filter_source,
  source_pd = NULL,
  source_datasets = NULL,
  reference_date,
  ref_start_window,
  ref_confirm,
  max_nr_ne = 1,
  accept_sd = FALSE,
  missing_as_ne = FALSE,
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
  condition are considered for deriving the confirmed best overall
  response.

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

- reference_date:

  Reference date

  The reference date is used for the derivation of `"SD"` and
  `"NON-CR/NON-PD"` response (see "Details" section). Usually it is
  treatment start date (`TRTSDT`) or randomization date (`RANDDT`).

  *Permitted Values:* a numeric date variable

- ref_start_window:

  Stable disease time window

  Assessments at least the specified number of days after the reference
  date (i.e. where `ADT` \>= `reference_date` + `ref_start_window`) with
  response `"CR"`, `"PR"`, `"SD"`, or `"NON-CR/NON-PD"` are considered
  for `"SD"` or `"NON-CR/NON-PD"` response.

  *Permitted Values:* a non-negative numeric scalar

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

- missing_as_ne:

  Consider no assessments as `"NE"`?

  If the argument is set to `TRUE`, the response is set to `"NE"` for
  subjects without an assessment in the input dataset. Otherwise, the
  response is set to `"MISSING"` for these subjects.

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
  `exprs(PARAMCD = "CBOR", PARAM = "Confirmed Best Overall Response")`
  is expected. The values must be symbols, character strings, numeric
  values, or `NA`.

- subject_keys:

  Variables to uniquely identify a subject

  A list of symbols created using
  [`exprs()`](https://rlang.r-lib.org/reference/defusing-advanced.html)
  is expected.

## Value

The input dataset with a new parameter for confirmed best overall
response

## Details

1.  The input dataset (`dataset`) is restricted to the observations
    matching `filter_source` and to observations before or at the date
    specified by `source_pd`.

2.  The following potential confirmed responses are selected from the
    restricted input dataset:

    - `"CR"`: An assessment is considered as complete response (CR) if

      - `AVALC == "CR"`,

      - there is a confirmatory assessment with `AVALC == "CR"` at least
        `ref_confirm` days after the assessment,

      - all assessments between the assessment and the confirmatory
        assessment are `"CR"` or `"NE"`, and

      - there are at most `max_nr_ne` `"NE"` assessments between the
        assessment and the confirmatory assessment.

    - `"PR"`: An assessment is considered as partial response (PR) if

      - `AVALC == "PR"`,

      - there is a confirmatory assessment with
        `AVALC %in% c("CR", "PR")` at least `ref_confirm` days after the
        assessment,

      - all assessments between the assessment and the confirmatory
        assessment are `"CR"`, `"PR"`, `"SD"`, or `"NE"`,

      - there is no `"PR"` assessment after a `"CR"` assessment in the
        confirmation period,

      - there are at most `max_nr_ne` `"NE"` assessments between the
        assessment and the confirmatory assessment, and

      - if the `accept_sd` argument is set to `TRUE`, one `"SD"`
        assessment in the confirmation period is accepted. Otherwise, no
        `"SD"` assessment must occur within the confirmation period.

    - `"SD"`: An assessment is considered as stable disease (SD) if

      - `AVALC %in% c("CR", "PR", "SD")` and

      - the assessment is at least `ref_start_window` days after
        `reference_date`.

    - `"NON-CR/NON-PD"`: An assessment is considered as NON-CR/NON-PD if

      - `AVALC = "NON-CR/NON-PD"` and

      - the assessment is at least `ref_start_window` days after
        `reference_date`.

    - `"PD"`: An assessment is considered as progressive disease (PD) if
      `AVALC == "PD"`.

    - `"NE"`: An assessment is considered as not estimable (NE) if

      - `AVALC == "NE"` or

      - `AVALC %in% c("CR", "PR", "SD", "NON-CR/NON-PD")` and the
        assessment is less than `ref_start_window` days after
        `reference_date`.

    - `"ND"`: An assessment is considered as not done (ND) if
      `AVALC == "ND"`.

    - `"MISSING"`: An assessment is considered as missing (MISSING) if a
      subject has no observation in the input dataset.

      If the `missing_as_ne` argument is set to `TRUE`, `AVALC` is set
      to `"NE"` for these subjects.

3.  For each subject the best response as derived in the previous step
    is selected, where `"CR"` is best and `"MISSING"` is worst in the
    order above. If the best response is not unique, the first one (with
    respect to `ADT`) is selected. If the selected record is from the
    input dataset, all variables are kept. If the selected record is
    from `dataset_adsl`, all variables which are in both `dataset` and
    `dataset_adsl` are kept.

4.  The variables specified by the `set_values_to` parameter are added
    to the new observations.

5.  The new observations are added to input dataset.

## See also

Other deprecated:
[`date_source()`](https:/pharmaverse.github.io/admiralonco/v1.4.0/reference/date_source.md),
[`derive_param_bor()`](https:/pharmaverse.github.io/admiralonco/v1.4.0/reference/derive_param_bor.md),
[`derive_param_clinbenefit()`](https:/pharmaverse.github.io/admiralonco/v1.4.0/reference/derive_param_clinbenefit.md),
[`derive_param_confirmed_resp()`](https:/pharmaverse.github.io/admiralonco/v1.4.0/reference/derive_param_confirmed_resp.md),
[`derive_param_response()`](https:/pharmaverse.github.io/admiralonco/v1.4.0/reference/derive_param_response.md),
[`filter_pd()`](https:/pharmaverse.github.io/admiralonco/v1.4.0/reference/filter_pd.md)

## Author

Stefan Bundfuss

## Examples

``` r
library(dplyr)
library(lubridate)
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
    TRTSDT = ymd(TRTSDTC),
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
    ADT = ymd(ADTC),
    STUDYID = "XX1234"
  ) %>%
  select(-ADTC) %>%
  derive_vars_merged(
    dataset_add = adsl,
    by_vars = exprs(STUDYID, USUBJID),
    new_vars = exprs(TRTSDT)
  )

pd_date <- date_source(
  dataset_name = "adrs",
  date = ADT,
  filter = PARAMCD == "PD" & ANL01FL == "Y"
)

# Derive confirmed best overall response parameter
derive_param_confirmed_bor(
  adrs,
  dataset_adsl = adsl,
  filter_source = PARAMCD == "OVR" & ANL01FL == "Y",
  source_pd = pd_date,
  source_datasets = list(adrs = adrs),
  reference_date = TRTSDT,
  ref_start_window = 28,
  ref_confirm = 28,
  set_values_to = exprs(
    PARAMCD = "CBOR",
    PARAM = "Best Confirmed Overall Response by Investigator"
  )
) %>%
  filter(PARAMCD == "CBOR")
#> `derive_param_confirmed_bor()` was deprecated in admiralonco 1.4.
#> ℹ Please use `admiral::derive_extreme_event()` instead.
#> ✖ This message will turn into a warning at the beginning of 2027.
#> ℹ See admiral's deprecation guidance:
#>   https://pharmaverse.github.io/admiraldev/dev/articles/programming_strategy.html#deprecation
#> Warning: Dataset contains CR records followed by PR.
#> Run `get_crpr_dataset()` to access the CR records records followed by PR
#> # A tibble: 9 × 8
#>   USUBJID AVALC         PARAMCD ANL01FL ADT        STUDYID TRTSDT     PARAM     
#>   <chr>   <chr>         <chr>   <chr>   <date>     <chr>   <date>     <chr>     
#> 1 1       CR            CBOR    Y       2020-02-01 XX1234  2020-01-01 Best Conf…
#> 2 2       SD            CBOR    Y       2020-02-01 XX1234  2019-12-12 Best Conf…
#> 3 3       SD            CBOR    Y       2020-01-01 XX1234  2019-11-11 Best Conf…
#> 4 4       SD            CBOR    Y       2020-03-01 XX1234  2019-12-30 Best Conf…
#> 5 5       NON-CR/NON-PD CBOR    Y       2020-05-15 XX1234  2020-01-01 Best Conf…
#> 6 6       SD            CBOR    Y       2020-03-30 XX1234  2020-02-02 Best Conf…
#> 7 7       NE            CBOR    Y       2020-02-06 XX1234  2020-02-02 Best Conf…
#> 8 8       MISSING       CBOR    NA      NA         XX1234  2020-04-01 Best Conf…
#> 9 9       SD            CBOR    Y       2020-05-01 XX1234  2020-03-01 Best Conf…

# Derive confirmed best overall response parameter (accepting SD for PR,
# accept two NEs, and considering missings as NE)
derive_param_confirmed_bor(
  adrs,
  dataset_adsl = adsl,
  filter_source = PARAMCD == "OVR" & ANL01FL == "Y",
  source_pd = pd_date,
  source_datasets = list(adrs = adrs),
  reference_date = TRTSDT,
  ref_start_window = 28,
  ref_confirm = 28,
  max_nr_ne = 2,
  accept_sd = TRUE,
  missing_as_ne = TRUE,
  set_values_to = exprs(
    PARAMCD = "CBOR",
    PARAM = "Best Confirmed Overall Response by Investigator"
  )
) %>%
  filter(PARAMCD == "CBOR")
#> Warning: Dataset contains CR records followed by PR.
#> Run `get_crpr_dataset()` to access the CR records records followed by PR
#> # A tibble: 9 × 8
#>   USUBJID AVALC         PARAMCD ANL01FL ADT        STUDYID TRTSDT     PARAM     
#>   <chr>   <chr>         <chr>   <chr>   <date>     <chr>   <date>     <chr>     
#> 1 1       CR            CBOR    Y       2020-02-01 XX1234  2020-01-01 Best Conf…
#> 2 2       PR            CBOR    Y       2020-02-01 XX1234  2019-12-12 Best Conf…
#> 3 3       SD            CBOR    Y       2020-01-01 XX1234  2019-11-11 Best Conf…
#> 4 4       SD            CBOR    Y       2020-03-01 XX1234  2019-12-30 Best Conf…
#> 5 5       NON-CR/NON-PD CBOR    Y       2020-05-15 XX1234  2020-01-01 Best Conf…
#> 6 6       SD            CBOR    Y       2020-03-30 XX1234  2020-02-02 Best Conf…
#> 7 7       NE            CBOR    Y       2020-02-06 XX1234  2020-02-02 Best Conf…
#> 8 8       NE            CBOR    NA      NA         XX1234  2020-04-01 Best Conf…
#> 9 9       CR            CBOR    Y       2020-03-16 XX1234  2020-03-01 Best Conf…
```
