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
