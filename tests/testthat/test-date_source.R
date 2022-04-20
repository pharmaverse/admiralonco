
dummy_data <- tibble::tribble(
    ~STUDYID, ~USUBJID, ~PARAMCD,  ~ADT,              ~AVAL,   ~DTC,
    "TEST01", "PAT01",  "PDDT",  as.Date("2021-04-27"),  "",   "2021-04-27",
    "TEST01", "PAT02",  "PDDT",  as.Date("2021-04-27"),  "",   "2021-04-27"
  )

testthat::test_that("an error is thrown if date format is incorrect", {
  expect_error(
    date_source(dataset_name = "dummy_data", date = DTC, filter = PARAMCD == "PDDT"),
    regexp = "date = needs to be DT or DTM format"
  )
})
