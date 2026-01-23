# derive_param_bor Test 3: Error if missing records for filter_source

    Code
      derive_param_bor(dataset = adrs, dataset_adsl = adsl, filter_source = PARAMCD ==
        "MISSING RECORDS", source_pd = NULL, source_datasets = NULL, reference_date = TRTSDT,
      ref_start_window = 28, set_values_to = exprs(PARAMCD = "BOR", PARAM = "Best Overall Response"))
    Condition
      Error in `derive_param_bor()`:
      ! dataframe passed into dataset argument with the filter PARAMCD == "MISSING RECORDS" has 0 records

# derive_param_bor Test 5: deprecation message if function is called

    Code
      actual_01 <- derive_param_bor(dataset = adrs, dataset_adsl = adsl,
        filter_source = PARAMCD == "OVR", source_pd = NULL, source_datasets = NULL,
        reference_date = TRTSDT, ref_start_window = 28, set_values_to = exprs(AVAL = {{
          aval_fun_pass }}(AVALC), PARAMCD = "BOR", PARAM = "Best Overall Response"))
    Message
      `derive_param_bor()` was deprecated in admiralonco 1.4.
      i Please use `admiral::derive_extreme_event()` instead.
      x This message will turn into a warning at the beginning of 2027.
      i See admiral's deprecation guidance: https://pharmaverse.github.io/admiraldev/dev/articles/programming_strategy.html#deprecation

