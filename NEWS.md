# admiralonco 0.4.0

## Documentation

- New vignette "Creating and Using New Anti-Cancer Start Date" (#17)
- "Create `ADRS`" and "Create `ADTTE`" vignette has been updated to include a link to "Creating and Using New Anti-Cancer Start Date" vignette

## New Features

- Added a "Report a bug" link to `{admiralonco}` website (#211)

- Added a section regarding dependencies to the `{admiralonco}` website (#223)

- Vignettes and templates were updated due to changes in admiral and
admiralonco: (#216)
    - The `AVAL` variable is now populated via the `set_values_to` argument as
    the `aval_fun` argument was deprecated.
    - `admiral::derive_param_extreme_event()` calls were replaced by
    `admiral::derive_extreme_records()` calls due to deprecation of
    `admiral::derive_param_extreme_event()`.
    - `admiral::derive_param_exist_flag()` calls were updated as the
    `dataset_adsl` argument was renamed to `dataset_ref`.

## Breaking Changes

- Function `derive_param_lasta()`, which was deprecated in admiralonco 0.2.0,
has been removed. (#216)

- The `aval_fun` argument of `derive_param_bor()`, `derive_param_clinbenefit()`,
`derive_param_confirmed_bor()`, `derive_param_confirmed_resp()`,
`derive_param_response()` was deprecated in favor of the `set_values_to`
argument. (#216)

- The `call_aval_fun()` function was deprecated in favor or
`admiraldev::process_set_values_to()`. (#216)


# admiralonco 0.3.0

## Breaking Changes

- All function arguments which expected a list of quosures created by `vars()`
are now expecting a list of expressions created by `exprs()`. For example,
instead of `by_vars = vars(STUDYID, USUBJID)` `by_vars = exprs(STUDYID,
USUBJID)` must be used now. (#197)

## Documentation

- New vignette "Create `ADTR`" (#16)

## Various

- ADTTE template simplified for duration of response in line with `admiral::derive_param_tte()` enhancement (#203)

# admiralonco 0.2.0

## Updates of Existing Functions

- Functions that use `subject_keys` as a function argument now default to `get_admiral_option("subject_keys")`instead of `vars(STUDYID, USUBJID)` to allow users to change the variables that uniquely identify a subject once using `set_admiral_options()` to avoid several instances of find + replace in a script. (#175)

## Breaking Changes

- Function `derive_param_lasta()` has been deprecated. Please use `admiral::derive_param_extreme_event()` instead (#89).

## Documentation

- Showed alternative approach in `ADRS` vignette for filtering out assessments after progressive disease compared to `source_pd` argument (#168)
- Extra terminology added for Independent Review Facility (IRF) as Blinded Independent Central Review (BICR) to make vignettes more accessible (#187)

## Various

- Templates now save datasets as .rds instead of .rda (#177)

# admiralonco 0.1.0

- Initial package release focused mainly on solid tumor / RECIST v1.1.
