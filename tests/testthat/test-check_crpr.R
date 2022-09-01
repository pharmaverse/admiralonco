library(tibble)
library(dplyr, warn.conflicts = FALSE)
library(lubridate)
library(admiraldev)

adrs <- tribble(
  ~USUBJID, ~ADTC,        ~AVALC,
  "1",      "2020-01-01", "PR",
  "1",      "2020-02-01", "CR",
  "1",      "2020-02-16", "NE",
  "1",      "2020-03-01", "CR",
  "1",      "2020-04-01", "SD",
  "2",      "2020-02-06", "PR",
  "2",      "2020-02-16", "CR",
  "2",      "2020-03-30", "PR"
) %>%
  mutate(
    ADT = ymd(ADTC),
    STUDYID = "XX1234"
  )

# signal_crpr ----
## Test 1: warning is issued ----
test_that("signal_crpr Test 1: warning is issued", {
  expect_warning(signal_crpr(
    adrs,
    order = vars(ADT)
  ),
  paste(
    "Dataset contains CR records followed by PR.",
    "Run `get_crpr_dataset()` to access the CR records records followed by PR",
    sep = "\n"
  ),
  fixed = TRUE
  )
})

## Test 2: error with custom message ----
test_that("signal_crpr Test 2: error with custom message", {
  expect_error(signal_crpr(
    mutate(adrs, SUBJID = USUBJID) %>%
      select(-USUBJID, -STUDYID),
    order = vars(ADT),
    subject_keys = vars(SUBJID),
    check_type = "error",
    msg = "The modified dataset contains CR records followed by PR."
  ),
  paste(
    "The modified dataset contains CR records followed by PR.",
    "Run `get_crpr_dataset()` to access the CR records records followed by PR",
    sep = "\n"
  ),
  fixed = TRUE
  )
})

# get_crpr_dataset ----
## Test 3: dataset if returned ----
test_that("get_crpr_dataset() Test 2: dataset if returned", {
  suppress_warning(
    signal_crpr(adrs,
      order = vars(ADT)
    ),
    "Dataset contains CR records followed by PR"
  )

  expect_dfs_equal(
    base = tribble(
      ~USUBJID, ~ADTC,        ~AVALC,
      "2",      "2020-02-16", "CR",
      "2",      "2020-03-30", "PR"
    ) %>%
      mutate(
        ADT = ymd(ADTC),
        STUDYID = "XX1234"
      ),
    compare = get_crpr_dataset(),
    keys = c("USUBJID", "ADT")
  )
})
