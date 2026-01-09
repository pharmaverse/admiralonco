# derive_param_clinbenefit Test 4: deprecation message if function is called

    Code
      actual_output <- derive_param_clinbenefit(dataset = adrs, dataset_adsl = adsl,
        filter_source = PARAMCD == "OVR", source_resp = resp, source_pd = pd,
        source_datasets = list(adrs = adrs), reference_date = TRTSDT,
        ref_start_window = 28, clinben_vals = c("CR", "PR", "SD"), set_values_to = exprs(
          AVAL = yn_to_numeric(AVALC), PARAMCD = "CBR", ANL01FL = "Y"))
    Message
      `derive_param_clinbenefit()` was deprecated in admiralonco 1.4.
      i Please use `admiral::derive_extreme_event()` instead.
      x This message will turn into a warning at the beginning of 2027.
      i See admiral's deprecation guidance: https://pharmaverse.github.io/admiraldev/dev/articles/programming_strategy.html#deprecation

