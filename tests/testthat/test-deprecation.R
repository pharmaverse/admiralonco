## Test 1: A warning is thrown if `derive_param_lasta()` is called ----
test_that("deprecation Test 1: A warning is thrown if `derive_param_lasta()` is called", {
  rlang::with_options(lifecycle_verbosity = "warning", {
    expect_warning(
      derive_param_lasta(admiralonco::admiral_adrs, set_values_to = vars(PARAMCD = "LSTA")),
      class = "lifecycle_warning_deprecated"
    )
  })
})
