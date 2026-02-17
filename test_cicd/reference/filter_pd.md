# Filter up to First PD (Progressive Disease) Date

**\[deprecated\]** The `filter_pd()` function has been deprecated in
favor of
[`filter_relative()`](https:/pharmaverse.github.io/admiral/v1.4.1/cran-release/reference/filter_relative.html).

Filter a dataset to only include the source parameter records up to and
including the first PD (progressive disease). These records are passed
to downstream derivations regarding responses such as BOR (best overall
response).

## Usage

``` r
filter_pd(
  dataset,
  filter,
  source_pd,
  source_datasets,
  subject_keys = get_admiral_option("subject_keys")
)
```

## Arguments

- dataset:

  Input dataset

  The variables `ADT` and those specified by `subject_keys` are
  expected.

- filter:

  Filter condition for restricting the input dataset

- source_pd:

  A
  [`admiral::date_source()`](https:/pharmaverse.github.io/admiral/v1.4.1/cran-release/reference/date_source.html)
  object providing the date of first PD

  For each subject the first date (`date` field) in the provided dataset
  (`dataset_name` field) restricted by `filter` field is considered as
  first PD date.

- source_datasets:

  A named list of data sets is expected.

  The name must match the name provided by the `dataset_name` field of
  the
  [`admiral::date_source()`](https:/pharmaverse.github.io/admiral/v1.4.1/cran-release/reference/date_source.html)
  object specified for `source_pd`.

- subject_keys:

  Variables to uniquely identify a subject

  A list of symbols created using
  [`exprs()`](https://rlang.r-lib.org/reference/defusing-advanced.html)
  is expected.

## Value

A subset of the input dataset

## Details

1.  The input dataset (`dataset`) is restricted by `filter`.

2.  For each subject the first PD date is derived as the first date
    (`source_pd$date`) in the source pd dataset
    (`source_datasets[[source_pd$dataset_name]]`) restricted by
    `source_pd$filter`.

3.  The restricted input dataset is restricted to records up to first PD
    date. Records matching first PD date are included. For subject
    without any first PD date, all records are included.

## See also

Other deprecated:
[`date_source()`](https:/pharmaverse.github.io/admiralonco/test_cicd/reference/date_source.md),
[`derive_param_bor()`](https:/pharmaverse.github.io/admiralonco/test_cicd/reference/derive_param_bor.md),
[`derive_param_clinbenefit()`](https:/pharmaverse.github.io/admiralonco/test_cicd/reference/derive_param_clinbenefit.md),
[`derive_param_confirmed_bor()`](https:/pharmaverse.github.io/admiralonco/test_cicd/reference/derive_param_confirmed_bor.md),
[`derive_param_confirmed_resp()`](https:/pharmaverse.github.io/admiralonco/test_cicd/reference/derive_param_confirmed_resp.md),
[`derive_param_response()`](https:/pharmaverse.github.io/admiralonco/test_cicd/reference/derive_param_response.md)

## Author

Teckla Akinyi, Stefan Bundfuss

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

# Filter OVR records up to first PD, first PD date provided in separate BDS dataset (adevent)
adrs <- tibble::tribble(
  ~STUDYID,       ~USUBJID,      ~PARAMCD, ~AVALC, ~ADT,         ~ANL01FL,
  "CDISCPILOT01", "01-701-1015", "OVR",    "CR",   "2016-01-25", "Y",
  "CDISCPILOT01", "01-701-1015", "OVR",    "SD",   "2016-02-22", NA_character_,
  "CDISCPILOT01", "01-701-1015", "OVR",    "PD",   "2016-02-22", "Y",
  "CDISCPILOT01", "01-701-1015", "BOR",    "CR",   "2016-01-25", "Y",
  "CDISCPILOT01", "01-701-1034", "OVR",    "SD",   "2015-12-07", "Y",
  "CDISCPILOT01", "01-701-1034", "OVR",    "PD",   "2016-04-25", "Y",
  "CDISCPILOT01", "01-701-1034", "OVR",    "PD",   "2016-06-25", "Y",
  "CDISCPILOT01", "01-701-1034", "BOR",    "SD",   "2015-12-07", "Y",
  "CDISCPILOT01", "01-701-1035", "OVR",    "SD",   "2016-04-25", "Y",
  "CDISCPILOT01", "01-701-1035", "OVR",    "PR",   "2016-06-25", "Y",
  "CDISCPILOT01", "01-701-1035", "BOR",    "PR",   "2016-06-25", "Y"
) %>% mutate(
  ADT = as_date(ADT)
)

adevent <- tibble::tribble(
  ~STUDYID,       ~USUBJID,      ~PARAMCD, ~AVALC, ~ADT,
  "CDISCPILOT01", "01-701-1015", "PD",     "Y",    "2016-02-22",
  "CDISCPILOT01", "01-701-1034", "PD",     "Y",    "2016-04-25"
) %>% mutate(
  ADT = as_date(ADT)
)

pd <- date_source(
  dataset_name = "adevent",
  date = ADT,
  filter = PARAMCD == "PD"
)

filter_pd(
  dataset = adrs,
  filter = PARAMCD == "OVR" & ANL01FL == "Y",
  source_pd = pd,
  source_datasets = list(adevent = adevent)
)
#> `filter_pd()` was deprecated in admiralonco 1.4.
#> ℹ Please use `admiral::filter_relative()` instead.
#> ✖ This message will turn into a warning at the beginning of 2027.
#> ℹ See admiral's deprecation guidance:
#>   https://pharmaverse.github.io/admiraldev/dev/articles/programming_strategy.html#deprecation
#> # A tibble: 6 × 6
#>   STUDYID      USUBJID     PARAMCD AVALC ADT        ANL01FL
#>   <chr>        <chr>       <chr>   <chr> <date>     <chr>  
#> 1 CDISCPILOT01 01-701-1015 OVR     CR    2016-01-25 Y      
#> 2 CDISCPILOT01 01-701-1015 OVR     PD    2016-02-22 Y      
#> 3 CDISCPILOT01 01-701-1034 OVR     SD    2015-12-07 Y      
#> 4 CDISCPILOT01 01-701-1034 OVR     PD    2016-04-25 Y      
#> 5 CDISCPILOT01 01-701-1035 OVR     SD    2016-04-25 Y      
#> 6 CDISCPILOT01 01-701-1035 OVR     PR    2016-06-25 Y      

# Filter OVR records up to first PD, first PD date provided in ADSL dataset
adsl <- tibble::tribble(
  ~STUDYID,       ~USUBJID,      ~PDDT,
  "CDISCPILOT01", "01-701-1015", "2016-02-22",
  "CDISCPILOT01", "01-701-1034", "2016-04-25",
  "CDISCPILOT01", "01-701-1035", ""
) %>% mutate(
  PDDT = as_date(PDDT)
)

pd <- date_source(
  dataset_name = "adsl",
  date = PDDT
)

filter_pd(
  dataset = adrs,
  filter = PARAMCD == "OVR" & ANL01FL == "Y",
  source_pd = pd,
  source_datasets = list(adsl = adsl)
)
#> # A tibble: 6 × 6
#>   STUDYID      USUBJID     PARAMCD AVALC ADT        ANL01FL
#>   <chr>        <chr>       <chr>   <chr> <date>     <chr>  
#> 1 CDISCPILOT01 01-701-1015 OVR     CR    2016-01-25 Y      
#> 2 CDISCPILOT01 01-701-1015 OVR     PD    2016-02-22 Y      
#> 3 CDISCPILOT01 01-701-1034 OVR     SD    2015-12-07 Y      
#> 4 CDISCPILOT01 01-701-1034 OVR     PD    2016-04-25 Y      
#> 5 CDISCPILOT01 01-701-1035 OVR     SD    2016-04-25 Y      
#> 6 CDISCPILOT01 01-701-1035 OVR     PR    2016-06-25 Y      

# Filter OVR records up to first PD, first PD date provided in input dataset (PD parameter)
adrs <- tibble::tribble(
  ~STUDYID,       ~USUBJID,      ~PARAMCD, ~AVALC, ~ADT,         ~ANL01FL,
  "CDISCPILOT01", "01-701-1015", "OVR",    "CR",   "2016-01-25", "Y",
  "CDISCPILOT01", "01-701-1015", "OVR",    "SD",   "2016-02-22", NA_character_,
  "CDISCPILOT01", "01-701-1015", "OVR",    "PD",   "2016-02-22", "Y",
  "CDISCPILOT01", "01-701-1015", "BOR",    "CR",   "2016-01-25", "Y",
  "CDISCPILOT01", "01-701-1034", "OVR",    "SD",   "2015-12-07", "Y",
  "CDISCPILOT01", "01-701-1034", "OVR",    "PD",   "2016-04-25", "Y",
  "CDISCPILOT01", "01-701-1034", "OVR",    "PD",   "2016-06-25", "Y",
  "CDISCPILOT01", "01-701-1034", "BOR",    "SD",   "2015-12-07", "Y",
  "CDISCPILOT01", "01-701-1035", "OVR",    "SD",   "2016-04-25", "Y",
  "CDISCPILOT01", "01-701-1035", "OVR",    "PR",   "2016-06-25", "Y",
  "CDISCPILOT01", "01-701-1035", "BOR",    "PR",   "2016-06-25", "Y",
  "CDISCPILOT01", "01-701-1015", "PD",     "Y",    "2016-02-22", "Y",
  "CDISCPILOT01", "01-701-1034", "PD",     "Y",    "2016-04-25", "Y"
) %>% mutate(
  ADT = as_date(ADT)
)

pd <- date_source(
  dataset_name = "adrs",
  date = ADT,
  filter = PARAMCD == "PD"
)

filter_pd(
  dataset = adrs,
  filter = PARAMCD == "OVR" & ANL01FL == "Y",
  source_pd = pd,
  source_datasets = list(adrs = adrs)
)
#> # A tibble: 6 × 6
#>   STUDYID      USUBJID     PARAMCD AVALC ADT        ANL01FL
#>   <chr>        <chr>       <chr>   <chr> <date>     <chr>  
#> 1 CDISCPILOT01 01-701-1015 OVR     CR    2016-01-25 Y      
#> 2 CDISCPILOT01 01-701-1015 OVR     PD    2016-02-22 Y      
#> 3 CDISCPILOT01 01-701-1034 OVR     SD    2015-12-07 Y      
#> 4 CDISCPILOT01 01-701-1034 OVR     PD    2016-04-25 Y      
#> 5 CDISCPILOT01 01-701-1035 OVR     SD    2016-04-25 Y      
#> 6 CDISCPILOT01 01-701-1035 OVR     PR    2016-06-25 Y      

# Filter OVR records up to first PD, first PD date derived from OVR records
adrs <- tibble::tribble(
  ~STUDYID,       ~USUBJID,      ~PARAMCD, ~AVALC, ~ADT,         ~ANL01FL,
  "CDISCPILOT01", "01-701-1015", "OVR",    "CR",   "2016-01-25", "Y",
  "CDISCPILOT01", "01-701-1015", "OVR",    "SD",   "2016-02-22", NA_character_,
  "CDISCPILOT01", "01-701-1015", "OVR",    "PD",   "2016-02-22", "Y",
  "CDISCPILOT01", "01-701-1015", "BOR",    "CR",   "2016-01-25", "Y",
  "CDISCPILOT01", "01-701-1034", "OVR",    "SD",   "2015-12-07", "Y",
  "CDISCPILOT01", "01-701-1034", "OVR",    "PD",   "2016-04-25", "Y",
  "CDISCPILOT01", "01-701-1034", "OVR",    "PD",   "2016-06-25", "Y",
  "CDISCPILOT01", "01-701-1034", "BOR",    "SD",   "2015-12-07", "Y",
  "CDISCPILOT01", "01-701-1035", "OVR",    "SD",   "2016-04-25", "Y",
  "CDISCPILOT01", "01-701-1035", "OVR",    "PR",   "2016-06-25", "Y",
  "CDISCPILOT01", "01-701-1035", "BOR",    "PR",   "2016-06-25", "Y"
) %>% mutate(
  ADT = as_date(ADT)
)

pd <- date_source(
  dataset_name = "adrs",
  date = ADT,
  filter = PARAMCD == "OVR" & ANL01FL == "Y" & AVALC == "PD"
)

filter_pd(
  dataset = adrs,
  filter = PARAMCD == "OVR" & ANL01FL == "Y",
  source_pd = pd,
  source_datasets = list(adrs = adrs)
)
#> # A tibble: 6 × 6
#>   STUDYID      USUBJID     PARAMCD AVALC ADT        ANL01FL
#>   <chr>        <chr>       <chr>   <chr> <date>     <chr>  
#> 1 CDISCPILOT01 01-701-1015 OVR     CR    2016-01-25 Y      
#> 2 CDISCPILOT01 01-701-1015 OVR     PD    2016-02-22 Y      
#> 3 CDISCPILOT01 01-701-1034 OVR     SD    2015-12-07 Y      
#> 4 CDISCPILOT01 01-701-1034 OVR     PD    2016-04-25 Y      
#> 5 CDISCPILOT01 01-701-1035 OVR     SD    2016-04-25 Y      
#> 6 CDISCPILOT01 01-701-1035 OVR     PR    2016-06-25 Y      
```
