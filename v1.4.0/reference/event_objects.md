# Pre-Defined Response Event Objects

These pre-defined
[`event()`](https:/pharmaverse.github.io/admiral/v1.4.0/cran-release/reference/event.html)
and
[`event_joined()`](https:/pharmaverse.github.io/admiral/v1.4.0/cran-release/reference/event_joined.html)
objects can be used as input to
[`admiral::derive_extreme_event()`](https:/pharmaverse.github.io/admiral/v1.4.0/cran-release/reference/derive_extreme_event.html).

## Usage

``` r
rsp_y

no_data_n

cb_y

bor_cr

bor_pr

bor_sd

bor_non_crpd

bor_pd

bor_ne

no_data_missing

crsp_y_cr

crsp_y_pr

cbor_cr

cbor_pr
```

## Details

To see the definition of the various objects simply print the object in
the R console, e.g. `bor_sd`. For details of how to use these objects
please refer to
[`admiral::derive_extreme_event()`](https:/pharmaverse.github.io/admiral/v1.4.0/cran-release/reference/derive_extreme_event.html).

It is assumed that `dataset_name = "ovr"` refers to the dataset of the
only overall response assessments at each visit which should be
considered for the parameter derivations. For example the dataset should
include only post-baseline assessments up to first PD and before start
of anti-cancer therapy.

## See also

[`admiral::derive_extreme_event()`](https:/pharmaverse.github.io/admiral/v1.4.0/cran-release/reference/derive_extreme_event.html),
[`admiral::event()`](https:/pharmaverse.github.io/admiral/v1.4.0/cran-release/reference/event.html),
[`admiral::event_joined()`](https:/pharmaverse.github.io/admiral/v1.4.0/cran-release/reference/event_joined.html)

## Examples

``` r
# This shows the definition of all pre-defined `event` objects that ship
# with {admiralonco}
exports <- sort(getNamespaceExports("admiralonco"))
for (obj_name in exports) {
  obj <- getExportedValue("admiralonco", obj_name)
  if (inherits(obj, "event_def")) {
    cat("\n", obj_name, ":\n", sep = "")
    print(obj, indent = 2)
  }
}
#> 
#> bor_cr:
#>   <event> object
#>   description: "Define complete response (CR) for best overall response (BOR)"
#>   dataset_name: "ovr"
#>   condition: AVALC == "CR"
#>   mode: NULL
#>   order: NULL
#>   set_values_to:
#>     AVALC: "CR"
#>   keep_source_vars: NULL
#> 
#> bor_ne:
#>   <event> object
#>   description: "Define not evaluable (NE) for best overall response (BOR) as CR, PR, SD, NON-CR/NON-PD, or NE (should be specified after bor_sd and bor_non_crpd)"
#>   dataset_name: "ovr"
#>   condition: AVALC %in% c("CR", "PR", "SD", "NON-CR/NON-PD", "NE")
#>   mode: NULL
#>   order: NULL
#>   set_values_to:
#>     AVALC: "NE"
#>   keep_source_vars: NULL
#> 
#> bor_non_crpd:
#>   <event> object
#>   description: "Define NON-CR/NON-PD for best overall response (BOR) as NON-CR/NON-PD occuring at least 42 days after randomization"
#>   dataset_name: "ovr"
#>   condition: AVALC == "NON-CR/NON-PD" & ADT >= RANDDT + 42
#>   mode: NULL
#>   order: NULL
#>   set_values_to:
#>     AVALC: "NON-CR/NON-PD"
#>   keep_source_vars: NULL
#> 
#> bor_pd:
#>   <event> object
#>   description: "Define progressive disease (PD) for best overall response (BOR)"
#>   dataset_name: "ovr"
#>   condition: AVALC == "PD"
#>   mode: NULL
#>   order: NULL
#>   set_values_to:
#>     AVALC: "PD"
#>   keep_source_vars: NULL
#> 
#> bor_pr:
#>   <event> object
#>   description: "Define partial response (PR) for best overall response (BOR)"
#>   dataset_name: "ovr"
#>   condition: AVALC == "PR"
#>   mode: NULL
#>   order: NULL
#>   set_values_to:
#>     AVALC: "PR"
#>   keep_source_vars: NULL
#> 
#> bor_sd:
#>   <event> object
#>   description: "Define stable disease (SD) for best overall respone (BOR) as CR, PR, or SD occurring at least 42 days after randomization"
#>   dataset_name: "ovr"
#>   condition: AVALC %in% c("CR", "PR", "SD") & ADT >= RANDDT + 42
#>   mode: NULL
#>   order: NULL
#>   set_values_to:
#>     AVALC: "SD"
#>   keep_source_vars: NULL
#> 
#> cb_y:
#>   <event> object
#>   description: "Define CR, PR, SD, or NON-CR/NON-PD occuring at least 42 days after randomization as clinical benefit"
#>   dataset_name: "ovr"
#>   condition: AVALC %in% c("CR", "PR", "SD", "NON-CR/NON-PD") & ADT >= RANDDT + 
#>     42
#>   mode: NULL
#>   order: NULL
#>   set_values_to:
#>     AVALC: "Y"
#>   keep_source_vars: NULL
#> 
#> cbor_cr:
#>   <event_joined> object
#>   description: "Define complete response (CR) for confirmed best overall response (CBOR) as CR followed by CR at least 28 days later and at most one NE in between"
#>   dataset_name: "ovr"
#>   condition: AVALC == "CR" & all(AVALC.join %in% c("CR", "NE")) & count_vals(var = AVALC.join, 
#>     val = "NE") <= 1
#>   order: NULL
#>   join_vars:
#>     AVALC
#>     ADT
#>   join_type: "after"
#>   first_cond_lower: NULL
#>   first_cond_upper: AVALC.join == "CR" & ADT.join >= ADT + 28
#>   set_values_to:
#>     AVALC: "CR"
#>   keep_source_vars: NULL
#> 
#> cbor_pr:
#>   <event_joined> object
#>   description: "Define partial response (PR) for confirmed best overall response (CBOR) as PR followed by CR or PR at least 28 days later, at most one NE in between, and no PR after CR"
#>   dataset_name: "ovr"
#>   condition: AVALC == "PR" & all(AVALC.join %in% c("CR", "PR", "NE")) & count_vals(var = AVALC.join, 
#>     val = "NE") <= 1 & (min_cond(var = ADT.join, cond = AVALC.join == 
#>     "CR") > max_cond(var = ADT.join, cond = AVALC.join == "PR") | 
#>     count_vals(var = AVALC.join, val = "CR") == 0 | count_vals(var = AVALC.join, 
#>     val = "PR") == 0)
#>   order: NULL
#>   join_vars:
#>     AVALC
#>     ADT
#>   join_type: "after"
#>   first_cond_lower: NULL
#>   first_cond_upper: AVALC.join %in% c("CR", "PR") & ADT.join >= ADT + 28
#>   set_values_to:
#>     AVALC: "PR"
#>   keep_source_vars: NULL
#> 
#> crsp_y_cr:
#>   <event_joined> object
#>   description: "Define confirmed response as CR followed by CR at least 28 days later and at most one NE in between"
#>   dataset_name: "ovr"
#>   condition: AVALC == "CR" & all(AVALC.join %in% c("CR", "NE")) & count_vals(var = AVALC.join, 
#>     val = "NE") <= 1
#>   order:
#>     ADT
#>   join_vars:
#>     AVALC
#>     ADT
#>   join_type: "after"
#>   first_cond_lower: NULL
#>   first_cond_upper: AVALC.join == "CR" & ADT.join >= ADT + days(28)
#>   set_values_to:
#>     AVALC: "Y"
#>   keep_source_vars: NULL
#> 
#> crsp_y_pr:
#>   <event_joined> object
#>   description: "Define confirmed response as PR followed by CR or PR at least 28 days later, at most one NE in between, and no PR after CR"
#>   dataset_name: "ovr"
#>   condition: AVALC == "PR" & all(AVALC.join %in% c("CR", "PR", "NE")) & count_vals(var = AVALC.join, 
#>     val = "NE") <= 1 & (min_cond(var = ADT.join, cond = AVALC.join == 
#>     "CR") > max_cond(var = ADT.join, cond = AVALC.join == "PR") | 
#>     count_vals(var = AVALC.join, val = "CR") == 0 | count_vals(var = AVALC.join, 
#>     val = "PR") == 0)
#>   order:
#>     ADT
#>   join_vars:
#>     AVALC
#>     ADT
#>   join_type: "after"
#>   first_cond_lower: NULL
#>   first_cond_upper: AVALC.join %in% c("CR", "PR") & ADT.join >= ADT + days(28)
#>   set_values_to:
#>     AVALC: "Y"
#>   keep_source_vars: NULL
#> 
#> no_data_missing:
#>   <event> object
#>   description: "Define missing response (MISSING) for all subjects in adsl in your population who has no post baseline response (should be used as last event)"
#>   dataset_name: "adsl"
#>   condition: TRUE
#>   mode: NULL
#>   order: NULL
#>   set_values_to:
#>     AVALC: "MISSING"
#>   keep_source_vars:
#>     RANDDT
#> 
#> no_data_n:
#>   <event> object
#>   description: "Define no response for all patients in adsl (should be used as last event)"
#>   dataset_name: "adsl"
#>   condition: TRUE
#>   mode: NULL
#>   order: NULL
#>   set_values_to:
#>     AVALC: "N"
#>   keep_source_vars:
#>     RANDDT
#> 
#> rsp_y:
#>   <event> object
#>   description: "Define CR or PR as (unconfirmed) response"
#>   dataset_name: "ovr"
#>   condition: AVALC %in% c("CR", "PR")
#>   mode: NULL
#>   order: NULL
#>   set_values_to:
#>     AVALC: "Y"
#>   keep_source_vars: NULL
```
