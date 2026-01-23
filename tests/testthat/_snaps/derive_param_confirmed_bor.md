# derive_param_confirmed_bor Test 3: error if invalid response values

    Code
      derive_param_confirmed_bor(adrs, dataset_adsl = adsl, filter_source = PARAMCD ==
        "OVR", source_pd = pd_date, source_datasets = list(adrs = adrs),
      reference_date = TRTSDT, ref_start_window = 28, ref_confirm = 28,
      set_values_to = exprs(PARAMCD = "CBOR", PARAM = "Best Confirmed Overall Response by Investigator"))
    Condition
      Error in `derive_param_confirmed_bor()`:
      ! The function is considering only the following response values: "CR", "PR", "SD", "NON-CR/NON-PD", "PD", "NE", and "ND" The following invalid values were found: "iCR"

# derive_param_confirmed_bor Test 6: deprecation message if function is called

    Code
      suppress_warning(actual <- derive_param_confirmed_bor(adrs, dataset_adsl = adsl,
        filter_source = PARAMCD == "OVR", source_pd = pd_date, source_datasets = list(
          adrs = adrs), reference_date = TRTSDT, ref_start_window = 28, ref_confirm = 28,
        set_values_to = exprs(AVAL = aval_resp(AVALC), PARAMCD = "CBOR", PARAM = "Best Confirmed Overall Response by Investigator")),
      "Dataset contains CR records followed by PR")
    Message
      `derive_param_confirmed_bor()` was deprecated in admiralonco 1.4.
      i Please use `admiral::derive_extreme_event()` instead.
      x This message will turn into a warning at the beginning of 2027.
      i See admiral's deprecation guidance: https://pharmaverse.github.io/admiraldev/dev/articles/programming_strategy.html#deprecation

