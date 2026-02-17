# Create a `date_source` object

**\[deprecated\]** The `date_source()` function has been deprecated
without replacement as all functions using the `source_pd` argument are
deprecated as well.

Create a `date_source` object as input for `source_pd` argument in
`{admiralonco}` functions.

## Usage

``` r
date_source(dataset_name, filter = NULL, date, set_values_to = NULL)
```

## Arguments

- dataset_name:

  The name of the dataset, i.e. a string, used to search for the date.

- filter:

  An unquoted condition for filtering `dataset`.

- date:

  A variable or an expression providing a date. A date or a datetime can
  be specified. An unquoted symbol or expression is expected.

- set_values_to:

  Variables to be set

## Value

An object of class `date_source`.

## See also

Other deprecated:
[`derive_param_bor()`](https:/pharmaverse.github.io/admiralonco/test_cicd/reference/derive_param_bor.md),
[`derive_param_clinbenefit()`](https:/pharmaverse.github.io/admiralonco/test_cicd/reference/derive_param_clinbenefit.md),
[`derive_param_confirmed_bor()`](https:/pharmaverse.github.io/admiralonco/test_cicd/reference/derive_param_confirmed_bor.md),
[`derive_param_confirmed_resp()`](https:/pharmaverse.github.io/admiralonco/test_cicd/reference/derive_param_confirmed_resp.md),
[`derive_param_response()`](https:/pharmaverse.github.io/admiralonco/test_cicd/reference/derive_param_response.md),
[`filter_pd()`](https:/pharmaverse.github.io/admiralonco/test_cicd/reference/filter_pd.md)

## Examples

``` r
pd <- date_source(
  dataset_name = "adrs",
  date = ADT,
  filter = PARAMCD == "PD"
)
#> `date_source()` was deprecated in admiralonco 1.4.0.
#> ✖ This message will turn into a warning {at the beginning of 2027}.
#> ℹ See admiral's deprecation guidance:
#>   https://pharmaverse.github.io/admiraldev/dev/articles/programming_strategy.html#deprecation
```
