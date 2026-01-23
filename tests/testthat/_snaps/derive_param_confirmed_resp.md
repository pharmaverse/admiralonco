# derive_param_confirmed_resp Test 3: error if invalid response values

    Code
      derive_param_confirmed_resp(adrs, dataset_adsl = adsl, filter_source = PARAMCD ==
        "OVR", source_pd = pd_date, source_datasets = list(adrs = adrs), ref_confirm = 28,
      set_values_to = exprs(PARAMCD = "CRSP", PARAM = "Confirmed Response by Investigator"))
    Condition
      Error in `derive_param_confirmed_resp()`:
      ! The function is considering only the following response values: "CR", "PR", "SD", "NON-CR/NON-PD", "PD", "NE", and "ND" The following invalid values were found: "iCR"

# derive_param_confirmed_resp Test 6: deprecation message if function is called

    Code
      suppress_warning(actual <- derive_param_confirmed_resp(adrs, dataset_adsl = adsl,
        filter_source = PARAMCD == "OVR", source_pd = pd_date, source_datasets = list(
          adrs = adrs), ref_confirm = 28, set_values_to = exprs(AVAL = yn_to_numeric(
          AVALC), PARAMCD = "CRSP", PARAM = "Confirmed Response by Investigator")),
      "Dataset contains CR records followed by PR")
    Message
      `derive_param_confirmed_resp()` was deprecated in admiralonco 1.4.
      i Please use `admiral::derive_extreme_event()` instead.
      x This message will turn into a warning at the beginning of 2027.
      i See admiral's deprecation guidance: https://pharmaverse.github.io/admiraldev/dev/articles/programming_strategy.html#deprecation

