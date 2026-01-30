# Pre-Defined Time-to-Event Source Objects

These pre-defined `tte_source` objects can be used as input to
[`admiral::derive_param_tte()`](https:/pharmaverse.github.io/admiral/v1.4.0/cran-release/reference/derive_param_tte.html).

## Usage

``` r
death_event

lastalive_censor

pd_event

lasta_censor

rand_censor

trts_censor
```

## Details

To see the definition of the various objects simply print the object in
the R console, e.g. `print(death_event)`. For details of how to use
these objects please refer to
[`admiral::derive_param_tte()`](https:/pharmaverse.github.io/admiral/v1.4.0/cran-release/reference/derive_param_tte.html).

Printing an object will display input dataset_name, filter (if
applicable), date variable, and appropriate values for `EVNTDESC`,
`CNSDTDSC`, `SRCDOM`, `SRCVAR`, and `SRCSEQ`.

## See also

[`admiral::derive_param_tte()`](https:/pharmaverse.github.io/admiral/v1.4.0/cran-release/reference/derive_param_tte.html),
[`admiral::tte_source()`](https:/pharmaverse.github.io/admiral/v1.4.0/cran-release/reference/tte_source.html),
[`admiral::event_source()`](https:/pharmaverse.github.io/admiral/v1.4.0/cran-release/reference/event_source.html),
[`admiral::censor_source()`](https:/pharmaverse.github.io/admiral/v1.4.0/cran-release/reference/censor_source.html)

## Examples

``` r
# This shows the definition of all pre-defined `tte_source` objects that ship
# with {admiralonco}
for (obj in admiral::list_tte_source_objects(package = "admiralonco")$object) {
  cat(obj, "\n")
  print(get(obj, envir = getNamespace("admiralonco")))
  cat("\n")
}
#> lastalive_censor 
#> <censor_source> object
#> dataset_name: "adsl"
#> filter: NULL
#> date: LSTALVDT
#> censor: 1
#> set_values_to:
#>   EVNTDESC: "Alive"
#>   CNSDTDSC: "Alive During Study"
#>   SRCDOM: "ADSL"
#>   SRCVAR: "LSTALVDT"
#> order: NULL
#> 
#> trts_censor 
#> <censor_source> object
#> dataset_name: "adsl"
#> filter: NULL
#> date: TRTSDT
#> censor: 1
#> set_values_to:
#>   EVNTDESC: "Treatment Start"
#>   CNSDTDSC: "Treatment Start"
#>   SRCDOM: "ADSL"
#>   SRCVAR: "TRTSDT"
#> order: NULL
#> 
#> pd_event 
#> <event_source> object
#> dataset_name: "adrs"
#> filter: PARAMCD == "PD" & AVALC == "Y" & ANL01FL == "Y"
#> date: ADT
#> censor: 0
#> set_values_to:
#>   EVNTDESC: "Disease Progression"
#>   SRCDOM: "ADRS"
#>   SRCVAR: "ADT"
#>   SRCSEQ: ASEQ
#> order: NULL
#> 
#> death_event 
#> <event_source> object
#> dataset_name: "adrs"
#> filter: PARAMCD == "DEATH" & AVALC == "Y" & ANL01FL == "Y"
#> date: ADT
#> censor: 0
#> set_values_to:
#>   EVNTDESC: "Death"
#>   SRCDOM: "ADRS"
#>   SRCVAR: "ADT"
#>   SRCSEQ: ASEQ
#> order: NULL
#> 
#> lasta_censor 
#> <censor_source> object
#> dataset_name: "adrs"
#> filter: PARAMCD == "LSTA" & ANL01FL == "Y"
#> date: ADT
#> censor: 1
#> set_values_to:
#>   EVNTDESC: "Last Tumor Assessment"
#>   CNSDTDSC: "Last Tumor Assessment"
#>   SRCDOM: "ADRS"
#>   SRCVAR: "ADT"
#>   SRCSEQ: ASEQ
#> order: NULL
#> 
#> rand_censor 
#> <censor_source> object
#> dataset_name: "adsl"
#> filter: NULL
#> date: RANDDT
#> censor: 1
#> set_values_to:
#>   EVNTDESC: "Randomization"
#>   CNSDTDSC: "Randomization"
#>   SRCDOM: "ADSL"
#>   SRCVAR: "RANDDT"
#> order: NULL
#> 
```
