# Get CR Records Followed by PR That Lead to a Prior Error

Get CR Records Followed by PR That Lead to a Prior Error

## Usage

``` r
get_crpr_dataset()
```

## Value

A `data.frame` or `NULL`

## Details

Some `{admiralonco}` functions check that in the source records CR is
not followed by PR and throw an error otherwise. The
`get_crpr_dataset()` function allows one to retrieve the duplicate
records that lead to an error.

Note that the function always returns the dataset of duplicates from the
last error that has been thrown in the current R session. Thus, after
restarting the R sessions `get_crpr_dataset()` will return `NULL` and
after a second error has been thrown, the dataset of the first error can
no longer be accessed (unless it has been saved in a variable).

## See also

[`signal_crpr()`](https:/pharmaverse.github.io/admiralonco/v1.4.0/reference/signal_crpr.md)

Utilities for Dataset Checking:
[`signal_crpr()`](https:/pharmaverse.github.io/admiralonco/v1.4.0/reference/signal_crpr.md)

## Author

Stefan Bundfuss

## Examples

``` r
library(tibble)
library(dplyr)
library(lubridate)
library(admiralonco)
library(rlang)

adrs <- tribble(
  ~USUBJID, ~ADTC,        ~AVALC,
  "1",      "2020-01-01", "PR",
  "1",      "2020-02-01", "CR",
  "1",      "2020-02-16", "NE",
  "1",      "2020-03-01", "CR",
  "2",      "2020-02-06", "PR",
  "2",      "2020-02-16", "CR",
  "2",      "2020-03-30", "PR",
) %>%
  mutate(
    ADT = ymd(ADTC),
    STUDYID = "XX1234"
  )

signal_crpr(adrs, order = exprs(ADT))
#> Warning: Dataset contains CR records followed by PR.
#> Run `get_crpr_dataset()` to access the CR records records followed by PR

get_crpr_dataset()
#> # A tibble: 2 × 5
#>   USUBJID ADTC       AVALC ADT        STUDYID
#>   <chr>   <chr>      <chr> <date>     <chr>  
#> 1 2       2020-02-16 CR    2020-02-16 XX1234 
#> 2 2       2020-03-30 PR    2020-03-30 XX1234 
```
