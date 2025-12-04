# derive_param_bor Test 3: Error if missing records for filter_source

    Code
      derive_param_bor(dataset = adrs, dataset_adsl = adsl, filter_source = PARAMCD ==
        "MISSING RECORDS", source_pd = NULL, source_datasets = NULL, reference_date = TRTSDT,
      ref_start_window = 28, set_values_to = exprs(PARAMCD = "BOR", PARAM = "Best Overall Response"))
    Condition
      Error in `derive_param_bor()`:
      ! dataframe passed into dataset argument with the filter PARAMCD == "MISSING RECORDS" has 0 records

