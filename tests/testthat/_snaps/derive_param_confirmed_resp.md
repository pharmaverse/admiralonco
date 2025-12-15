# derive_param_confirmed_resp Test 3: error if invalid response values

    Code
      derive_param_confirmed_resp(adrs, dataset_adsl = adsl, filter_source = PARAMCD ==
        "OVR", source_pd = pd_date, source_datasets = list(adrs = adrs), ref_confirm = 28,
      set_values_to = exprs(PARAMCD = "CRSP", PARAM = "Confirmed Response by Investigator"))
    Condition
      Error in `derive_param_confirmed_resp()`:
      ! The function is considering only the following response values: "CR", "PR", "SD", "NON-CR/NON-PD", "PD", "NE", and "ND" The following invalid values were found: "iCR"

