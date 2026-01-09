# derive_param_response Test 4: deprecation message if function is called

    Code
      adrs %>% derive_param_response(dataset_adsl = adsl, filter_source = PARAMCD ==
        "OVR" & AVALC %in% c("CR", "PR"), source_pd = NULL, source_datasets = NULL,
      set_values_to = exprs(AVAL = admiral::yn_to_numeric(AVALC), PARAMCD = "RSP",
      PARAM = "Response by investigator"), subject_keys = get_admiral_option(
        "subject_keys"))
    Message
      `derive_param_response()` was deprecated in admiralonco 1.4.
      i Please use `admiral::derive_extreme_event()` instead.
      x This message will turn into a warning at the beginning of 2027.
      i See admiral's deprecation guidance: https://pharmaverse.github.io/admiraldev/dev/articles/programming_strategy.html#deprecation
    Output
      # A tibble: 17 x 8
         USUBJID PARAMCD AVALC CHECKKEPTCOL STUDYID ADT         AVAL PARAM            
         <chr>   <chr>   <chr> <chr>        <chr>   <date>     <dbl> <chr>            
       1 1       OVR     PR    001          XX1234  2020-01-02    NA <NA>             
       2 1       OVR     CR    001          XX1234  2020-02-01    NA <NA>             
       3 1       OVR     CR    001          XX1234  2020-03-01    NA <NA>             
       4 1       OVR     SD    001          XX1234  2020-04-01    NA <NA>             
       5 1       PD      N     001          XX1234  NA            NA <NA>             
       6 2       OVR     SD    002          XX1234  2021-06-15    NA <NA>             
       7 2       OVR     PD    002          XX1234  2021-07-16    NA <NA>             
       8 2       OVR     PD    002          XX1234  2021-09-14    NA <NA>             
       9 2       PD      Y     002          XX1234  2021-09-14    NA <NA>             
      10 3       OVR     SD    003          XX1234  2021-09-14    NA <NA>             
      11 3       OVR     PD    003          XX1234  2021-10-30    NA <NA>             
      12 3       OVR     CR    003          XX1234  2021-12-25    NA <NA>             
      13 3       PD      Y     003          XX1234  2021-10-30    NA <NA>             
      14 1       RSP     Y     001          XX1234  2020-01-02     1 Response by inve~
      15 3       RSP     Y     003          XX1234  2021-12-25     1 Response by inve~
      16 2       RSP     N     002          XX1234  NA             0 Response by inve~
      17 4       RSP     N     004          XX1234  NA             0 Response by inve~

