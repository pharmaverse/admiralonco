# Adds a Parameter for Clinical Benefit

**\[deprecated\]** The `derive_param_clinbenefit()` function has been
deprecated in favor of
[`derive_extreme_event()`](https:/pharmaverse.github.io/admiral/v1.4.0/cran-release/reference/derive_extreme_event.html).

Adds a parameter for clinical benefit/disease control

## Usage

``` r
derive_param_clinbenefit(
  dataset,
  dataset_adsl,
  filter_source,
  source_resp,
  source_pd = NULL,
  source_datasets,
  reference_date,
  ref_start_window,
  aval_fun,
  clinben_vals = c("CR", "PR", "SD", "NON-CR/NON-PD"),
  set_values_to,
  subject_keys = get_admiral_option("subject_keys")
)
```

## Arguments

- dataset:

  Input dataset. This is the dataset to which the clinical benefit rate
  parameter will be added.

  The variables `PARAMCD`, `AVALC`, `ADT`, and those specified by the
  `subject_keys` parameter and the `reference_date` parameter are
  expected.

  After applying `filter_source` and/or `source_pd` the variable `ADT`
  and the variables specified by `subject_keys` must be a unique key of
  the dataset.

- dataset_adsl:

  ADSL input dataset.

  The variables specified for `subject_keys`is expected. For each
  subject of the specified dataset a new observation is added to the
  input dataset. Variables in `dataset_adsl` that also appear in
  `dataset` will be populated with the appropriate subject-specific
  value for these new observations.

- filter_source:

  Filter condition in `dataset` that represents records for overall
  disease response assessment for a subject at a given timepoint, e.g.
  `PARAMCD == "OVR"` or `PARAMCD == "OVRLRESP"`.

- source_resp:

  A `date_source` object specifying the dataset, date variable, and
  filter condition used to identify response status.

- source_pd:

  A `date_source` object specifying the dataset, date variable, and
  filter condition used to identify disease progression.

- source_datasets:

  A named list of data sets is expected.

  The list must contain the names provided by the `dataset_name` field
  of the
  [`date_source()`](https:/pharmaverse.github.io/admiralonco/v1.4.0/reference/date_source.md)
  objects specified for `source_pd` and `source_resp`.

- reference_date:

  Name of variable representing the index date for `ref_start_window`. A
  variable providing a date. An unquoted symbol is expected.

- ref_start_window:

  Integer representing number of days from `reference_date` that must
  elapse before an evaluable non-PD assessment counts toward determining
  clinical benefit.

- aval_fun:

  **\[deprecated\]** Please use `set_values_to` instead.

  Function to map character analysis value (`AVALC`) to numeric analysis
  value (`AVAL`)

  The (first) argument of the function must expect a character vector
  and the function must return a numeric vector.

- clinben_vals:

  A vector of response values to be considered when determining clinical
  benefit.

- set_values_to:

  A named list returned by
  [`exprs()`](https://rlang.r-lib.org/reference/defusing-advanced.html)
  containing new variables and their static value to be populated for
  the clinical benefit rate parameter records, e.g.
  `exprs(PARAMCD = "CBR", PARAM = "Clinical Benefit Rate")`.

- subject_keys:

  A named list returned by
  [`exprs()`](https://rlang.r-lib.org/reference/defusing-advanced.html)
  containing variables used to uniquely identify subjects.

## Value

The input dataset with a new parameter for clinical benefit

## Details

Clinical benefit/disease control is first identified by looking for
subjects having response status, and then derived for subjects that have
at least one evaluable non-PD response assessment prior to first PD
(Progressive Disease) (i.e., responses inclusive of `CR`, `PR`, `SD`,
and `NON-CR/NON-PD`) and after a specified amount of time from a
reference date (`ref_start_window`).

Note: The user input values they wish to include when determining
clinical benefit using the argument `clinben_vals`. The default values
for this are `CR`, `PR`, `SD`, and `NON-CR/NON-PD`, as listed above. In
the below example, eligible values be limited to `CR` and `PR`.

Example: `clinben_vals <- c("CR", "PR")`

1.  The input dataset (`dataset`) is restricted to the observations
    matching `filter_source` and to observations before or at the date
    specified by `source_pd`.

2.  This dataset is further restricted to include user-generated
    response assessments from `clinben_vals` or include response
    assessments of `CR`, `PR`, `SD`, and `NON-CR/NON-PD`, exclude
    missing response assessments, and exclude those less than
    `ref_start_window` after `reference_date`. The earliest assessment
    by `ADT` is then selected.

3.  The dataset identified by `dataset` in `source_resp` is restricted
    according to its `filter` argument. The variable corresponding to
    the `date` parameter of `source_resp` is considered together with
    `ADT` from the previous step.

4.  For the observations being added to `dataset`, `ADT` is set to the
    earlier of the first assessment date representing an evaluable
    non-PD assessment prior to first PD, or the date representing the
    start of response.

5.  For the observations being added to `dataset`, `AVALC` is set to

    - `Y` for those subjects in the `dataset` meeting the criteria for
      clinical benefit above

    - `N` for subjects not meeting the clinical benefit criteria in
      `dataset` or the dataset identified in `source_resp`

    - `N` for subjects present in `dataset_adsl` but not present in
      `dataset` or the dataset identified in `source_resp`.

6.  The variables specified by `set_values_to` are added to the new
    observations with values equal to the values specified in the same.

7.  The new observations are added to `dataset`. Variables held in
    common between `dataset` and `dataset_adsl` are kept for the new
    observations, and are populated with their values from
    `dataset_adsl`.

## See also

Other deprecated:
[`date_source()`](https:/pharmaverse.github.io/admiralonco/v1.4.0/reference/date_source.md),
[`derive_param_bor()`](https:/pharmaverse.github.io/admiralonco/v1.4.0/reference/derive_param_bor.md),
[`derive_param_confirmed_bor()`](https:/pharmaverse.github.io/admiralonco/v1.4.0/reference/derive_param_confirmed_bor.md),
[`derive_param_confirmed_resp()`](https:/pharmaverse.github.io/admiralonco/v1.4.0/reference/derive_param_confirmed_resp.md),
[`derive_param_response()`](https:/pharmaverse.github.io/admiralonco/v1.4.0/reference/derive_param_response.md),
[`filter_pd()`](https:/pharmaverse.github.io/admiralonco/v1.4.0/reference/filter_pd.md)

## Author

Andrew Smith

## Examples

``` r
library(lubridate)
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

adsl <- tibble::tribble(
  ~USUBJID, ~TRTSDT,
  "01",     ymd("2020-01-14"),
  "02",     ymd("2021-02-16"),
  "03",     ymd("2021-03-09"),
  "04",     ymd("2021-04-21")
) %>%
  mutate(STUDYID = "AB42")

adrs <- tibble::tribble(
  ~USUBJID, ~PARAMCD, ~AVALC, ~ADT,
  "01",     "RSP",    "Y",    ymd("2021-03-14"),
  "02",     "RSP",    "N",    ymd("2021-05-07"),
  "03",     "RSP",    "N",    NA,
  "04",     "RSP",    "N",    NA,
  "01",     "PD",     "N",    NA,
  "02",     "PD",     "Y",    ymd("2021-05-07"),
  "03",     "PD",     "N",    NA,
  "04",     "PD",     "N",    NA,
  "01",     "OVR",    "SD",   ymd("2020-03-14"),
  "01",     "OVR",    "PR",   ymd("2021-04-13"),
  "02",     "OVR",    "PR",   ymd("2021-04-08"),
  "02",     "OVR",    "PD",   ymd("2021-05-07"),
  "02",     "OVR",    "CR",   ymd("2021-06-20"),
  "03",     "OVR",    "SD",   ymd("2021-03-30"),
  "04",     "OVR",    "NE",   ymd("2021-05-21"),
  "04",     "OVR",    "NA",   ymd("2021-06-30"),
  "04",     "OVR",    "NE",   ymd("2021-07-24"),
  "04",     "OVR",    "ND",   ymd("2021-09-04"),
) %>%
  mutate(STUDYID = "AB42", ANL01FL = "Y") %>%
  derive_vars_merged(
    dataset_add = adsl,
    by_vars = exprs(STUDYID, USUBJID),
    new_vars = exprs(TRTSDT)
  )

pd <- date_source(
  dataset_name = "adrs",
  date = ADT,
  filter = PARAMCD == "PD" & AVALC == "Y" & ANL01FL == "Y"
)

resp <- date_source(
  dataset_name = "adrs",
  date = ADT,
  filter = PARAMCD == "RSP" & AVALC == "Y" & ANL01FL == "Y"
)

derive_param_clinbenefit(
  dataset = adrs,
  dataset_adsl = adsl,
  filter_source = PARAMCD == "OVR" & ANL01FL == "Y",
  source_resp = resp,
  source_pd = pd,
  source_datasets = list(adrs = adrs),
  reference_date = TRTSDT,
  ref_start_window = 28,
  set_values_to = exprs(
    PARAMCD = "CBR"
  )
) %>%
  filter(PARAMCD == "CBR")
#> `derive_param_clinbenefit()` was deprecated in admiralonco 1.4.
#> ℹ Please use `admiral::derive_extreme_event()` instead.
#> ✖ This message will turn into a warning at the beginning of 2027.
#> ℹ See admiral's deprecation guidance:
#>   https://pharmaverse.github.io/admiraldev/dev/articles/programming_strategy.html#deprecation
#> # A tibble: 4 × 7
#>   USUBJID PARAMCD AVALC ADT        STUDYID ANL01FL TRTSDT    
#>   <chr>   <chr>   <chr> <date>     <chr>   <chr>   <date>    
#> 1 01      CBR     Y     2020-03-14 AB42    Y       2020-01-14
#> 2 02      CBR     Y     2021-04-08 AB42    Y       2021-02-16
#> 3 03      CBR     N     NA         AB42    NA      2021-03-09
#> 4 04      CBR     N     NA         AB42    NA      2021-04-21
```
