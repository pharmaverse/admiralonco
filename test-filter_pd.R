test_that("Input dataset if filtered including dates only up to first PD date
          from the adrs source data set`", {

            adrs <- tibble::tribble(
              ~STUDYID, ~USUBJID, ~PARAMCD, ~AVALC, ~ADT,
              "CDISCPILOT01", "01-701-1015", "OVR", "CR", "2016-01-25",
              "CDISCPILOT01", "01-701-1015", "OVR", "PD", "2016-01-25",
              "CDISCPILOT01", "01-701-1015", "BOR", "PD", "2016-02-21",
              "CDISCPILOT01", "01-701-1034", "OVR", "SD", "2015-07-12",
              "CDISCPILOT01", "01-701-1034", "OVR", "PD", "2016-04-25",
              "CDISCPILOT01", "01-701-1034", "OVR", "PD", "2016-06-25",
              "CDISCPILOT01", "01-701-1034", "BOR", "PD", "2016-05-25"
            ) %>% dplyr::mutate(
              ADT = lubridate::as_date(ADT)
            )

            adlb <- tibble::tribble(
              ~STUDYID, ~USUBJID, ~ADT,
              "CDISCPILOT01", "01-701-1015", "2014-02-13",
              "CDISCPILOT01", "01-701-1015", "2015-05-21",
              "CDISCPILOT01", "01-701-1015", "2015-12-21",
              "CDISCPILOT01", "01-701-1015", "2016-07-08",
              "CDISCPILOT01", "01-701-1015", "2016-11-25",
              "CDISCPILOT01", "01-701-1015", "2017-02-21",
              "CDISCPILOT01", "01-701-1034", "2015-04-03",
              "CDISCPILOT01", "01-701-1034", "2016-01-05",
              "CDISCPILOT01", "01-701-1034", "2016-04-02",
              "CDISCPILOT01", "01-701-1034", "2016-04-25",
              "CDISCPILOT01", "01-701-1034", "2016-06-25",
              "CDISCPILOT01", "01-701-1034", "2016-05-25"
            ) %>% dplyr::mutate(
              ADT = lubridate::as_date(ADT)
            )


  expected_output <- tibble::tribble(
    ~STUDYID, ~USUBJID, ~ADT,
     "CDISCPILOT01", "01-701-1015", "2014-02-13",
     "CDISCPILOT01"," 01-701-1015", "2015-05-21",
     "CDISCPILOT01", "01-701-1015", "2015-12-21",
     "CDISCPILOT01", "01-701-1034", "2015-04-03",
     "CDISCPILOT01", "01-701-1034", "2016-01-05",
     "CDISCPILOT01", "01-701-1034", "2016-04-02",
     "CDISCPILOT01", "01-701-1034", "2016-04-25"
  ) %>% dplyr::mutate(
    ADT = lubridate::as_date(ADT)
  )

  actual_output <- filter_pd(dataset = adlb,
                filter_source = PARAMCD ==  "OVR" & AVALC == "PD",
                source_pd = ADT,
                source_dataset = adrs)

  expect_dfs_equal(
    expected_output,
    actual_output,
    keys = c("STUDYID", "USUBJID", "ADT")
  )
})
