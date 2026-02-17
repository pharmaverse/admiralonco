# Package index

## Pre-Defined Objects

Objects defined by
[admiralonco](https://pharmaverse.github.io/admiralonco/) that can be
used as input for derivations

- [`rsp_y`](https:/pharmaverse.github.io/admiralonco/test_cicd/reference/event_objects.md)
  [`no_data_n`](https:/pharmaverse.github.io/admiralonco/test_cicd/reference/event_objects.md)
  [`cb_y`](https:/pharmaverse.github.io/admiralonco/test_cicd/reference/event_objects.md)
  [`bor_cr`](https:/pharmaverse.github.io/admiralonco/test_cicd/reference/event_objects.md)
  [`bor_pr`](https:/pharmaverse.github.io/admiralonco/test_cicd/reference/event_objects.md)
  [`bor_sd`](https:/pharmaverse.github.io/admiralonco/test_cicd/reference/event_objects.md)
  [`bor_non_crpd`](https:/pharmaverse.github.io/admiralonco/test_cicd/reference/event_objects.md)
  [`bor_pd`](https:/pharmaverse.github.io/admiralonco/test_cicd/reference/event_objects.md)
  [`bor_ne`](https:/pharmaverse.github.io/admiralonco/test_cicd/reference/event_objects.md)
  [`no_data_missing`](https:/pharmaverse.github.io/admiralonco/test_cicd/reference/event_objects.md)
  [`crsp_y_cr`](https:/pharmaverse.github.io/admiralonco/test_cicd/reference/event_objects.md)
  [`crsp_y_pr`](https:/pharmaverse.github.io/admiralonco/test_cicd/reference/event_objects.md)
  [`cbor_cr`](https:/pharmaverse.github.io/admiralonco/test_cicd/reference/event_objects.md)
  [`cbor_pr`](https:/pharmaverse.github.io/admiralonco/test_cicd/reference/event_objects.md)
  : Pre-Defined Response Event Objects
- [`death_event`](https:/pharmaverse.github.io/admiralonco/test_cicd/reference/tte_source_objects.md)
  [`lastalive_censor`](https:/pharmaverse.github.io/admiralonco/test_cicd/reference/tte_source_objects.md)
  [`pd_event`](https:/pharmaverse.github.io/admiralonco/test_cicd/reference/tte_source_objects.md)
  [`lasta_censor`](https:/pharmaverse.github.io/admiralonco/test_cicd/reference/tte_source_objects.md)
  [`rand_censor`](https:/pharmaverse.github.io/admiralonco/test_cicd/reference/tte_source_objects.md)
  [`trts_censor`](https:/pharmaverse.github.io/admiralonco/test_cicd/reference/tte_source_objects.md)
  : Pre-Defined Time-to-Event Source Objects

## Utility Functions

### Utilities for Formatting Observations

- [`aval_resp()`](https:/pharmaverse.github.io/admiralonco/test_cicd/reference/aval_resp.md)
  : Map Character Response Values to Numeric Values

### Utilities for Dataset Checking

- [`get_crpr_dataset()`](https:/pharmaverse.github.io/admiralonco/test_cicd/reference/get_crpr_dataset.md)
  : Get CR Records Followed by PR That Lead to a Prior Error
- [`signal_crpr()`](https:/pharmaverse.github.io/admiralonco/test_cicd/reference/signal_crpr.md)
  : Signal CR Records Followed by PR

## Example Datasets

You can run
[`admiral::use_ad_template()`](https:/pharmaverse.github.io/admiral/v1.4.1/cran-release/reference/use_ad_template.html)
to produce additional datasets

- [`admiral_adrs`](https:/pharmaverse.github.io/admiralonco/test_cicd/reference/admiral_adrs.md)
  : Response Analysis Dataset

## Deprecated

Functions and arguments may need to be removed or replaced over time. In
such cases, the function or argument will enter a 3 year deprecation
cycle. The cycle will be tied as close as sensibly possible to a package
release.

When a function is deprecated:

- In Year 1, there will be a message issued if you use the
  function/argument, but it will still be available to use.
- In Year 2, a warning will be produced if you use the
  function/argument, but it will still be available to use.
- In Year 3, an error will be produced if you use the function/argument
  and no longer be able to use.
- Finally, after 3 years, the function/argument and related
  documentation and tests will be removed from
  [admiralonco](https://pharmaverse.github.io/admiralonco/).

*Note: Guidance on replacement functionality will be found in the
message produced as well as in the function’s documentation.*

Below, you can find a list of functions in the process of being
deprecated:

- [`date_source()`](https:/pharmaverse.github.io/admiralonco/test_cicd/reference/date_source.md)
  **\[deprecated\]** :

  Create a `date_source` object

- [`derive_param_bor()`](https:/pharmaverse.github.io/admiralonco/test_cicd/reference/derive_param_bor.md)
  **\[deprecated\]** : Adds a Parameter for Best Overall Response
  (without confirmation)

- [`derive_param_clinbenefit()`](https:/pharmaverse.github.io/admiralonco/test_cicd/reference/derive_param_clinbenefit.md)
  **\[deprecated\]** : Adds a Parameter for Clinical Benefit

- [`derive_param_confirmed_bor()`](https:/pharmaverse.github.io/admiralonco/test_cicd/reference/derive_param_confirmed_bor.md)
  **\[deprecated\]** : Adds a Parameter for Confirmed Best Overall
  Response

- [`derive_param_confirmed_resp()`](https:/pharmaverse.github.io/admiralonco/test_cicd/reference/derive_param_confirmed_resp.md)
  **\[deprecated\]** : Adds a Parameter for Confirmed Response

- [`derive_param_response()`](https:/pharmaverse.github.io/admiralonco/test_cicd/reference/derive_param_response.md)
  **\[deprecated\]** : Adds a Parameter Indicating If a Subject Had a
  Response before Progressive Disease

- [`filter_pd()`](https:/pharmaverse.github.io/admiralonco/test_cicd/reference/filter_pd.md)
  **\[deprecated\]** : Filter up to First PD (Progressive Disease) Date
