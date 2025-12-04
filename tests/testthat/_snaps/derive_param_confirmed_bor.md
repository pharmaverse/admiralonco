# derive_param_confirmed_bor Test 3: error if invalid response values

    Code
      derive_param_confirmed_bor(adrs, dataset_adsl = adsl, filter_source = PARAMCD ==
        "OVR", source_pd = pd_date, source_datasets = list(adrs = adrs),
      reference_date = TRTSDT, ref_start_window = 28, ref_confirm = 28,
      set_values_to = exprs(PARAMCD = "CBOR", PARAM = "Best Confirmed Overall Response by Investigator"))
    Condition
      Error in `derive_param_confirmed_bor()`:
      ! The function is considering only the following response values: "CR", "PR", "SD", "NON-CR/NON-PD", "PD", "NE", and "ND" The following invalid values were found: "iCR"

