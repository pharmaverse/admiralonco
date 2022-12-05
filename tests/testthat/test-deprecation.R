## Test 1: An error is thrown if `derive_param_lasta()` is called ----
test_that("deprecation Test 1: An error is thrown if `derive_param_lasta()` is called", {
  expect_error(
    derive_param_lasta(admiralonco::admiral_adrs, set_values_to = vars(PARAMCD = "LSTA")),
    class = "lifecycle_error_deprecated"
  )
})
