# Map Character Response Values to Numeric Values

Map character response values like `"PR"` or `"SD"` to numeric values.

## Usage

``` r
aval_resp(arg)
```

## Arguments

- arg:

  Character vector

## Value

- `1` if `arg` equals `"CR"`,

- `2` if `arg` equals `"PR"`,

- `3` if `arg` equals `"SD"`,

- `4` if `arg` equals `"NON-CR/NON-PD"`,

- `5` if `arg` equals `"PD"`,

- `6` if `arg` equals `"NE"`,

- `7` if `arg` equals `"MISSING"`,

- `NA_real_` otherwise

## Author

Stefan Bundfuss

## Examples

``` r
aval_resp(c("CR", "PR", "SD", "NON-CR/NON-PD", "PD", "NE", "MISSING", "ND", NA_character_))
#> [1]  1  2  3  4  5  6  7 NA NA
```
