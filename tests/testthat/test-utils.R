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

  expect_dfs_equal(
    call_aval_fun(
      data,
      yn_map
    ),
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
  expect_error(
    call_aval_fun(
      data,
      aval_fun = tibble
    ),
    regexp = "Assigning new AVAL records with aval_fun"
  )
})
