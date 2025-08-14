# admiralonco (development version)

- New vignette "Creating ADRS with Prostate Cancer Working Group 3 (PCWG3)
Criteria". (#304)

# admiralonco 1.2.0

## Documentation

- New vignette "Creating ADRS with GCIG Criteria". (#294)

- The vignettes "Creating a Basic ADRS" and "Creating ADRS (Including
Non-standard Endpoints)" were updated to describe how the case `CR` followed by
`PR` is handled. (#276)

- The documentation of the time-to-event source objects was fixed. (#293)

# admiralonco 1.1.0

## Documentation

- New vignette "Creating ADRS with IMWG Criteria". (#277)

## Updates of Existing Functions

- Replaced hard coding of `exprs(USUBJID, STUDYID)` and `exprs(USUBJID)` with
`get_admiral_option("subject_keys")` so the argument is flexible and can be
changed from the default by the user. (#281)

# admiralonco 1.0.0

## Updates of Existing Functions

- The `bor_ne` event object was updated such that `CR` and `PR` are additionally
considered as event. This ensures that the confirmed best overall response for a
patient with a single `CR` or `PR` assessment is derived as `"NE"` instead of
`"MISSING"`. (#253)

## Documentation

- New vignette "Creating ADRS with iRECIST endpoints". (#233)

- All vignettes and templates were updated to be in line with the changes in
`{admiral}` (see [Breaking
Changes](https://pharmaverse.github.io/admiral/news/index.html#breaking-changes-1-0-0)
for details)(#256).

## Breaking Changes

- The `derive_*()` functions were superseded in favor of
`derive_extreme_event()`. Any superseded functions can continue to be used as
demonstrated via the "Basic ADRS" vignette, as they will remain supported and
won't be deprecated in the near future. (#256)

- The `filter_pd()` function was superseded in favor of `filter_relative()`. (#256)

- The function `call_aval_fun()`, which was deprecated in admiralonco 0.4.0,
has been removed. (#256)

## Various

- Website now has button/links to Slack channel and GitHub Issues. (#262)

# admiralonco 0.5.0

## New Features

- Oncology specific events are provided for deriving oncology parameters with
`admiral::derive_extreme_event()`. (#234)

## Documentation

- The `ADRS` vignette and template were split into two (#234):
    - A basic version which uses the `{admiralonco}` functions to cover standard
    RECIST 1.1.
    - A more flexible version which uses `admiral::derive_extreme_event()`. This
    should be used if non-standard `ADRS` parameters should be implemented or
    non-standard response values should be considered. This version can also be
    used as a starting point for implementing other response criteria than
    RECIST 1.1, e.g., iRECIST or International Myeloma Working Group (IMWG)
    criteria for the diagnosis of multiple myeloma.

- The `ADRS` vignette and template were updated such that test data from
`{pharmaversesdtm}` instead of `{admiral.test}` is used. (#226)

- The "Create `ADTR`" vignette and the `ADTR` template were updated such that
test data from `{pharmaversesdtm}` instead of `{admiral.test}` is used. (#247)

- The confirmed response derivation was fixed in the basic `ADRS` vignette and
template. When calling `derive_param_confirmed_resp()` the `filter_source`
argument has to be set to `PARAMCD == "OVR" & ANL01FL == "Y"` instead of
`PARAMCD == "OVR" & AVALC %in% c("CR", "PR") & ANL01FL == "Y"`. Otherwise,
responses like `CR`, `NE`, `NE`, `CR` are considered as confirmed response.
(#245)

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
