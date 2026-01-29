# Changelog

## admiralonco 1.4.0

### New Features

- The deprecated function
  [`date_source()`](https:/pharmaverse.github.io/admiralonco/main/reference/date_source.md)
  was copied to
  [admiralonco](https://pharmaverse.github.io/admiralonco/) to provide a
  deprecation *message* rather than a deprecation *warning*. For this to
  have an effect the
  [admiralonco](https://pharmaverse.github.io/admiralonco/) package must
  be loaded *after* the
  [admiral](https://pharmaverse.github.io/admiral/) package.
  ([\#319](https://github.com/pharmaverse/admiralonco/issues/319))

### Breaking Changes

- The following functions/arguments are entering the next phase of the
  [deprecation
  process](https://pharmaverse.github.io/admiraldev/articles/programming_strategy.html#deprecation):

  **Phase 1 (message)**

  - [`derive_param_bor()`](https:/pharmaverse.github.io/admiralonco/main/reference/derive_param_bor.md)
    is deprecated in favor of
    [`admiral::derive_extreme_event()`](https:/pharmaverse.github.io/admiral/v1.4.0/cran-release/reference/derive_extreme_event.html).
    ([\#320](https://github.com/pharmaverse/admiralonco/issues/320))
  - [`derive_param_clinbenefit()`](https:/pharmaverse.github.io/admiralonco/main/reference/derive_param_clinbenefit.md)
    is deprecated in favor of
    [`admiral::derive_extreme_event()`](https:/pharmaverse.github.io/admiral/v1.4.0/cran-release/reference/derive_extreme_event.html).
    ([\#320](https://github.com/pharmaverse/admiralonco/issues/320))
  - [`derive_param_confirmed_bor()`](https:/pharmaverse.github.io/admiralonco/main/reference/derive_param_confirmed_bor.md)
    is deprecated in favor of
    [`admiral::derive_extreme_event()`](https:/pharmaverse.github.io/admiral/v1.4.0/cran-release/reference/derive_extreme_event.html).
    ([\#320](https://github.com/pharmaverse/admiralonco/issues/320))
  - [`derive_param_confirmed_resp()`](https:/pharmaverse.github.io/admiralonco/main/reference/derive_param_confirmed_resp.md)
    is deprecated in favor of
    [`admiral::derive_extreme_event()`](https:/pharmaverse.github.io/admiral/v1.4.0/cran-release/reference/derive_extreme_event.html).
    ([\#320](https://github.com/pharmaverse/admiralonco/issues/320))
  - [`derive_param_response()`](https:/pharmaverse.github.io/admiralonco/main/reference/derive_param_response.md)
    is deprecated in favor of
    [`admiral::derive_extreme_event()`](https:/pharmaverse.github.io/admiral/v1.4.0/cran-release/reference/derive_extreme_event.html).
    ([\#320](https://github.com/pharmaverse/admiralonco/issues/320))
  - [`filter_pd()`](https:/pharmaverse.github.io/admiralonco/main/reference/filter_pd.md)
    is deprecated in favor of
    [`admiral::filter_relative()`](https:/pharmaverse.github.io/admiral/v1.4.0/cran-release/reference/filter_relative.html).
    ([\#320](https://github.com/pharmaverse/admiralonco/issues/320))

  **Phase 2 (warning)**

  No functions or arguments in this phase

  **Phase 3 (error)**

  - The `aval_fun` argument in
    [`derive_param_bor()`](https:/pharmaverse.github.io/admiralonco/main/reference/derive_param_bor.md),
    [`derive_param_clinbenefit()`](https:/pharmaverse.github.io/admiralonco/main/reference/derive_param_clinbenefit.md),
    [`derive_param_confirmed_bor()`](https:/pharmaverse.github.io/admiralonco/main/reference/derive_param_confirmed_bor.md),
    [`derive_param_confirmed_resp()`](https:/pharmaverse.github.io/admiralonco/main/reference/derive_param_confirmed_resp.md),
    and
    [`derive_param_response()`](https:/pharmaverse.github.io/admiralonco/main/reference/derive_param_response.md)
    is deprecated in favor of the `set_values_to` argument.

  **Phase 4 (removed)**

  No functions or arguments in this phase

### Documentation

- PSA response endpoints were added to the “Creating ADRS with Prostate
  Cancer Working Group 3 (PCWG3) Criteria” vignette.
  ([\#317](https://github.com/pharmaverse/admiralonco/issues/317))

- The response values in the “Creating ADRS with Prostate Cancer Working
  Group 3 (PCWG3) Criteria” vignette were aligned to the standard
  terminology and the data issue in the `RS` domain was fixed.
  ([\#313](https://github.com/pharmaverse/admiralonco/issues/313))

- The wording in the “Bone Response Categories Based on PCWG3
  Guidelines” section of the “Creating ADRS with Prostate Cancer Working
  Group 3 (PCWG3) Criteria” vignette was improved.
  ([\#314](https://github.com/pharmaverse/admiralonco/issues/314))

- The “Ask AI” widget was added to the bottom right of each page. It
  enables users to ask questions about
  [admiralonco](https://pharmaverse.github.io/admiralonco/) and receive
  answers from an LLM. It is trained on the documentation of the admiral
  packages and provided by
  [kapa.ai](https://docs.kapa.ai/kapa-for-open-source).
  ([\#326](https://github.com/pharmaverse/admiralonco/issues/326))

- A link to the [{admiral}
  ecosystem](https://pharmaverse.org/e2eclinical/adam/) page was added
  to the home page sidebar and main text.
  ([\#318](https://github.com/pharmaverse/admiralonco/issues/318))

## admiralonco 1.3.0

CRAN release: 2025-09-01

### Documentation

- New vignette “Creating ADRS with Prostate Cancer Working Group 3
  (PCWG3) Criteria”.
  ([\#304](https://github.com/pharmaverse/admiralonco/issues/304))

- Display `ANL01FL` in the `NADIR` derivation in the `ADTR` vignette.
  ([\#305](https://github.com/pharmaverse/admiralonco/issues/305))

## admiralonco 1.2.0

CRAN release: 2025-01-27

### Documentation

- New vignette “Creating ADRS with GCIG Criteria”.
  ([\#294](https://github.com/pharmaverse/admiralonco/issues/294))

- The vignettes “Creating a Basic ADRS” and “Creating ADRS (Including
  Non-standard Endpoints)” were updated to describe how the case `CR`
  followed by `PR` is handled.
  ([\#276](https://github.com/pharmaverse/admiralonco/issues/276))

- The documentation of the time-to-event source objects was fixed.
  ([\#293](https://github.com/pharmaverse/admiralonco/issues/293))

## admiralonco 1.1.0

CRAN release: 2024-06-19

### Documentation

- New vignette “Creating ADRS with IMWG Criteria”.
  ([\#277](https://github.com/pharmaverse/admiralonco/issues/277))

### Updates of Existing Functions

- Replaced hard coding of `exprs(USUBJID, STUDYID)` and `exprs(USUBJID)`
  with `get_admiral_option("subject_keys")` so the argument is flexible
  and can be changed from the default by the user.
  ([\#281](https://github.com/pharmaverse/admiralonco/issues/281))

## admiralonco 1.0.0

CRAN release: 2023-12-20

### Updates of Existing Functions

- The `bor_ne` event object was updated such that `CR` and `PR` are
  additionally considered as event. This ensures that the confirmed best
  overall response for a patient with a single `CR` or `PR` assessment
  is derived as `"NE"` instead of `"MISSING"`.
  ([\#253](https://github.com/pharmaverse/admiralonco/issues/253))

### Documentation

- New vignette “Creating ADRS with iRECIST endpoints”.
  ([\#233](https://github.com/pharmaverse/admiralonco/issues/233))

- All vignettes and templates were updated to be in line with the
  changes in [admiral](https://pharmaverse.github.io/admiral/) (see
  [Breaking
  Changes](https://pharmaverse.github.io/admiral/news/index.html#breaking-changes-1-0-0)
  for
  details)([\#256](https://github.com/pharmaverse/admiralonco/issues/256)).

### Breaking Changes

- The `derive_*()` functions were superseded in favor of
  [`derive_extreme_event()`](https:/pharmaverse.github.io/admiral/v1.4.0/cran-release/reference/derive_extreme_event.html).
  Any superseded functions can continue to be used as demonstrated via
  the “Basic ADRS” vignette, as they will remain supported and won’t be
  deprecated in the near future.
  ([\#256](https://github.com/pharmaverse/admiralonco/issues/256))

- The
  [`filter_pd()`](https:/pharmaverse.github.io/admiralonco/main/reference/filter_pd.md)
  function was superseded in favor of
  [`filter_relative()`](https:/pharmaverse.github.io/admiral/v1.4.0/cran-release/reference/filter_relative.html).
  ([\#256](https://github.com/pharmaverse/admiralonco/issues/256))

- The function `call_aval_fun()`, which was deprecated in admiralonco
  0.4.0, has been removed.
  ([\#256](https://github.com/pharmaverse/admiralonco/issues/256))

### Various

- Website now has button/links to Slack channel and GitHub Issues.
  ([\#262](https://github.com/pharmaverse/admiralonco/issues/262))

## admiralonco 0.5.0

CRAN release: 2023-09-14

### New Features

- Oncology specific events are provided for deriving oncology parameters
  with
  [`admiral::derive_extreme_event()`](https:/pharmaverse.github.io/admiral/v1.4.0/cran-release/reference/derive_extreme_event.html).
  ([\#234](https://github.com/pharmaverse/admiralonco/issues/234))

### Documentation

- The `ADRS` vignette and template were split into two
  ([\#234](https://github.com/pharmaverse/admiralonco/issues/234)):

  - A basic version which uses the
    [admiralonco](https://pharmaverse.github.io/admiralonco/) functions
    to cover standard RECIST 1.1.
  - A more flexible version which uses
    [`admiral::derive_extreme_event()`](https:/pharmaverse.github.io/admiral/v1.4.0/cran-release/reference/derive_extreme_event.html).
    This should be used if non-standard `ADRS` parameters should be
    implemented or non-standard response values should be considered.
    This version can also be used as a starting point for implementing
    other response criteria than RECIST 1.1, e.g., iRECIST or
    International Myeloma Working Group (IMWG) criteria for the
    diagnosis of multiple myeloma.

- The `ADRS` vignette and template were updated such that test data from
  [pharmaversesdtm](https://pharmaverse.github.io/pharmaversesdtm/)
  instead of `{admiral.test}` is used.
  ([\#226](https://github.com/pharmaverse/admiralonco/issues/226))

- The “Create `ADTR`” vignette and the `ADTR` template were updated such
  that test data from
  [pharmaversesdtm](https://pharmaverse.github.io/pharmaversesdtm/)
  instead of `{admiral.test}` is used.
  ([\#247](https://github.com/pharmaverse/admiralonco/issues/247))

- The confirmed response derivation was fixed in the basic `ADRS`
  vignette and template. When calling
  [`derive_param_confirmed_resp()`](https:/pharmaverse.github.io/admiralonco/main/reference/derive_param_confirmed_resp.md)
  the `filter_source` argument has to be set to
  `PARAMCD == "OVR" & ANL01FL == "Y"` instead of
  `PARAMCD == "OVR" & AVALC %in% c("CR", "PR") & ANL01FL == "Y"`.
  Otherwise, responses like `CR`, `NE`, `NE`, `CR` are considered as
  confirmed response.
  ([\#245](https://github.com/pharmaverse/admiralonco/issues/245))

## admiralonco 0.4.0

CRAN release: 2023-06-12

### Documentation

- New vignette “Creating and Using New Anti-Cancer Start Date”
  ([\#17](https://github.com/pharmaverse/admiralonco/issues/17))
- “Create `ADRS`” and “Create `ADTTE`” vignette has been updated to
  include a link to “Creating and Using New Anti-Cancer Start Date”
  vignette

### New Features

- Added a “Report a bug” link to
  [admiralonco](https://pharmaverse.github.io/admiralonco/) website
  ([\#211](https://github.com/pharmaverse/admiralonco/issues/211))

- Added a section regarding dependencies to the
  [admiralonco](https://pharmaverse.github.io/admiralonco/) website
  ([\#223](https://github.com/pharmaverse/admiralonco/issues/223))

- Vignettes and templates were updated due to changes in admiral and
  admiralonco:
  ([\#216](https://github.com/pharmaverse/admiralonco/issues/216))

  - The `AVAL` variable is now populated via the `set_values_to`
    argument as the `aval_fun` argument was deprecated.
  - `admiral::derive_param_extreme_event()` calls were replaced by
    [`admiral::derive_extreme_records()`](https:/pharmaverse.github.io/admiral/v1.4.0/cran-release/reference/derive_extreme_records.html)
    calls due to deprecation of `admiral::derive_param_extreme_event()`.
  - [`admiral::derive_param_exist_flag()`](https:/pharmaverse.github.io/admiral/v1.4.0/cran-release/reference/derive_param_exist_flag.html)
    calls were updated as the `dataset_adsl` argument was renamed to
    `dataset_ref`.

### Breaking Changes

- Function `derive_param_lasta()`, which was deprecated in admiralonco
  0.2.0, has been removed.
  ([\#216](https://github.com/pharmaverse/admiralonco/issues/216))

- The `aval_fun` argument of
  [`derive_param_bor()`](https:/pharmaverse.github.io/admiralonco/main/reference/derive_param_bor.md),
  [`derive_param_clinbenefit()`](https:/pharmaverse.github.io/admiralonco/main/reference/derive_param_clinbenefit.md),
  [`derive_param_confirmed_bor()`](https:/pharmaverse.github.io/admiralonco/main/reference/derive_param_confirmed_bor.md),
  [`derive_param_confirmed_resp()`](https:/pharmaverse.github.io/admiralonco/main/reference/derive_param_confirmed_resp.md),
  [`derive_param_response()`](https:/pharmaverse.github.io/admiralonco/main/reference/derive_param_response.md)
  was deprecated in favor of the `set_values_to` argument.
  ([\#216](https://github.com/pharmaverse/admiralonco/issues/216))

- The `call_aval_fun()` function was deprecated in favor or
  [`admiraldev::process_set_values_to()`](https://pharmaverse.github.io/admiraldev/reference/process_set_values_to.html).
  ([\#216](https://github.com/pharmaverse/admiralonco/issues/216))

## admiralonco 0.3.0

CRAN release: 2023-03-14

### Breaking Changes

- All function arguments which expected a list of quosures created by
  [`vars()`](https://dplyr.tidyverse.org/reference/vars.html) are now
  expecting a list of expressions created by
  [`exprs()`](https://rlang.r-lib.org/reference/defusing-advanced.html).
  For example, instead of `by_vars = vars(STUDYID, USUBJID)`
  `by_vars = exprs(STUDYID, USUBJID)` must be used now.
  ([\#197](https://github.com/pharmaverse/admiralonco/issues/197))

### Documentation

- New vignette “Create `ADTR`”
  ([\#16](https://github.com/pharmaverse/admiralonco/issues/16))

### Various

- ADTTE template simplified for duration of response in line with
  [`admiral::derive_param_tte()`](https:/pharmaverse.github.io/admiral/v1.4.0/cran-release/reference/derive_param_tte.html)
  enhancement
  ([\#203](https://github.com/pharmaverse/admiralonco/issues/203))

## admiralonco 0.2.0

CRAN release: 2022-12-07

### Updates of Existing Functions

- Functions that use `subject_keys` as a function argument now default
  to `get_admiral_option("subject_keys")`instead of
  `vars(STUDYID, USUBJID)` to allow users to change the variables that
  uniquely identify a subject once using
  [`set_admiral_options()`](https:/pharmaverse.github.io/admiral/v1.4.0/cran-release/reference/set_admiral_options.html)
  to avoid several instances of find + replace in a script.
  ([\#175](https://github.com/pharmaverse/admiralonco/issues/175))

### Breaking Changes

- Function `derive_param_lasta()` has been deprecated. Please use
  `admiral::derive_param_extreme_event()` instead
  ([\#89](https://github.com/pharmaverse/admiralonco/issues/89)).

### Documentation

- Showed alternative approach in `ADRS` vignette for filtering out
  assessments after progressive disease compared to `source_pd` argument
  ([\#168](https://github.com/pharmaverse/admiralonco/issues/168))
- Extra terminology added for Independent Review Facility (IRF) as
  Blinded Independent Central Review (BICR) to make vignettes more
  accessible
  ([\#187](https://github.com/pharmaverse/admiralonco/issues/187))

### Various

- Templates now save datasets as .rds instead of .rda
  ([\#177](https://github.com/pharmaverse/admiralonco/issues/177))

## admiralonco 0.1.0

CRAN release: 2022-09-13

- Initial package release focused mainly on solid tumor / RECIST v1.1.
