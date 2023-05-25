library(tibble)

data <- tribble(
  ~AVALC,
  "YES",
  "NO"
)

# call_aval_fun ----

## Test 1: AVAL is created ----
test_that("call_aval_fun Test 1: AVAL is created", {
  yn_map <- function(x) {
    case_when(
      x == "YES" ~ 1,
      x == "NO" ~ 0
    )
  }

  expect_warning(
    actual <- call_aval_fun(
      data,
      yn_map
    ),
    class = "lifecycle_warning_deprecated"
  )

  expect_dfs_equal(
    actual,
    tribble(
      ~AVALC, ~AVAL,
      "YES",      1,
      "NO",       0
    ),
    keys = c("AVAL")
  )
})

## Test 2: Test error for invalid aval_fun ----
test_that("call_aval_fun Test 2: Test error for invalid aval_fun", {
  bad_fun <- function(x) {
    abort("Function bad_fun failed!")
  }

  expect_error(
    suppress_warning(
      call_aval_fun(
      data,
      aval_fun = bad_fun
    ),
    regexpr = "deprecated"
    ),
    regexp = "Assigning new AVAL records with aval_fun"
  )
})
