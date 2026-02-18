# Signal CR Records Followed by PR

Signal CR Records Followed by PR

## Usage

``` r
signal_crpr(
  dataset,
  order,
  msg = "Dataset contains CR records followed by PR.",
  subject_keys = get_admiral_option("subject_keys"),
  check_type = "warning"
)
```

## Arguments

- dataset:

  A data frame

- order:

  A list of variables created using
  [`exprs()`](https://rlang.r-lib.org/reference/defusing-advanced.html)
  determining the order or the records

- msg:

  The condition message

- subject_keys:

  Variables to uniquely identify a subject

  A list of symbols created using
  [`exprs()`](https://rlang.r-lib.org/reference/defusing-advanced.html)
  is expected.

- check_type:

  Type of message to issue when detecting PR after CR.

  *Permitted Values*: `"message"`, `"warning"` or `"error"`

## Value

No return value, called for side effects

## See also

[`get_crpr_dataset()`](https:/pharmaverse.github.io/admiralonco/337_pr_website/reference/get_crpr_dataset.md)

Utilities for Dataset Checking:
[`get_crpr_dataset()`](https:/pharmaverse.github.io/admiralonco/337_pr_website/reference/get_crpr_dataset.md)

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
```
