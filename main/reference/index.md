# Package index

## Pre-Defined Objects

Objects defined by
[admiralonco](https://pharmaverse.github.io/admiralonco/) that can be
used as input for derivations

- [`rsp_y`](https:/pharmaverse.github.io/admiralonco/main/reference/event_objects.md)
  [`no_data_n`](https:/pharmaverse.github.io/admiralonco/main/reference/event_objects.md)
  [`cb_y`](https:/pharmaverse.github.io/admiralonco/main/reference/event_objects.md)
  [`bor_cr`](https:/pharmaverse.github.io/admiralonco/main/reference/event_objects.md)
  [`bor_pr`](https:/pharmaverse.github.io/admiralonco/main/reference/event_objects.md)
  [`bor_sd`](https:/pharmaverse.github.io/admiralonco/main/reference/event_objects.md)
  [`bor_non_crpd`](https:/pharmaverse.github.io/admiralonco/main/reference/event_objects.md)
  [`bor_pd`](https:/pharmaverse.github.io/admiralonco/main/reference/event_objects.md)
  [`bor_ne`](https:/pharmaverse.github.io/admiralonco/main/reference/event_objects.md)
  [`no_data_missing`](https:/pharmaverse.github.io/admiralonco/main/reference/event_objects.md)
  [`crsp_y_cr`](https:/pharmaverse.github.io/admiralonco/main/reference/event_objects.md)
  [`crsp_y_pr`](https:/pharmaverse.github.io/admiralonco/main/reference/event_objects.md)
  [`cbor_cr`](https:/pharmaverse.github.io/admiralonco/main/reference/event_objects.md)
  [`cbor_pr`](https:/pharmaverse.github.io/admiralonco/main/reference/event_objects.md)
  : Pre-Defined Response Event Objects
- [`death_event`](https:/pharmaverse.github.io/admiralonco/main/reference/tte_source_objects.md)
  [`lastalive_censor`](https:/pharmaverse.github.io/admiralonco/main/reference/tte_source_objects.md)
  [`pd_event`](https:/pharmaverse.github.io/admiralonco/main/reference/tte_source_objects.md)
  [`lasta_censor`](https:/pharmaverse.github.io/admiralonco/main/reference/tte_source_objects.md)
  [`rand_censor`](https:/pharmaverse.github.io/admiralonco/main/reference/tte_source_objects.md)
  [`trts_censor`](https:/pharmaverse.github.io/admiralonco/main/reference/tte_source_objects.md)
  : Pre-Defined Time-to-Event Source Objects

## Utility Functions

### Utilities for Formatting Observations

- [`aval_resp()`](https:/pharmaverse.github.io/admiralonco/main/reference/aval_resp.md)
  : Map Character Response Values to Numeric Values

### Utilities for Dataset Checking

- [`get_crpr_dataset()`](https:/pharmaverse.github.io/admiralonco/main/reference/get_crpr_dataset.md)
  : Get CR Records Followed by PR That Lead to a Prior Error
- [`signal_crpr()`](https:/pharmaverse.github.io/admiralonco/main/reference/signal_crpr.md)
  : Signal CR Records Followed by PR

## Example Datasets

You can run
[`admiral::use_ad_template()`](https:/pharmaverse.github.io/admiral/v1.3.1/cran-release/reference/use_ad_template.html)
to produce additional datasets

- [`admiral_adrs`](https:/pharmaverse.github.io/admiralonco/main/reference/admiral_adrs.md)
  : Response Analysis Dataset

## Superseded

Superseded functions have been replaced with more flexible functions
that allow for construction of more diverse endpoints for a wider array
of oncology indications, beyond solid tumor/RECIST. However, we will not
be removing these functions from
[admiralonco](https://pharmaverse.github.io/admiralonco/) in the near
future and they will continue to be supported.

- [`derive_param_bor()`](https:/pharmaverse.github.io/admiralonco/main/reference/derive_param_bor.md)
  **\[superseded\]** : Adds a Parameter for Best Overall Response
  (without confirmation)
- [`derive_param_clinbenefit()`](https:/pharmaverse.github.io/admiralonco/main/reference/derive_param_clinbenefit.md)
  **\[superseded\]** : Adds a Parameter for Clinical Benefit
- [`derive_param_confirmed_bor()`](https:/pharmaverse.github.io/admiralonco/main/reference/derive_param_confirmed_bor.md)
  **\[superseded\]** : Adds a Parameter for Confirmed Best Overall
  Response
- [`derive_param_confirmed_resp()`](https:/pharmaverse.github.io/admiralonco/main/reference/derive_param_confirmed_resp.md)
  **\[superseded\]** : Adds a Parameter for Confirmed Response
- [`derive_param_response()`](https:/pharmaverse.github.io/admiralonco/main/reference/derive_param_response.md)
  **\[superseded\]** : Adds a Parameter Indicating If a Subject Had a
  Response before Progressive Disease
- [`filter_pd()`](https:/pharmaverse.github.io/admiralonco/main/reference/filter_pd.md)
  **\[superseded\]** : Filter up to First PD (Progressive Disease) Date
