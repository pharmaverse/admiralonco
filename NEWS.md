# admiralonco 0.2.0

## New Features

## Updates of Existing Functions

- Functions that use `subject_keys` as a function argument now default to `get_admiral_option("subject_keys")`instead of `vars(STUDYID, USUBJID)` to allow users to change the variables that uniquely identify a subject once using `set_admiral_options()` to avoid several instances of find + replace in a script. (#175)

## Breaking Changes

## Documentation

## Various

- Templates now save datasets as .rds instead of .rda (#177)

# admiralonco 0.1.0

- Initial package release focused mainly on solid tumor / RECIST v1.1.
