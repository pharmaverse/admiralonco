# filter_pd Test 5: error if invalid source_datasets

    Code
      filter_pd(dataset = adrs, filter = PARAMCD == "OVR", source_pd = date_source(
        dataset_name = "adrs", date = ADT, filter = PARAMCD == "OVR" & AVALC == "PD", ),
      source_datasets = list(ars = adrs))
    Message
      `date_source()` was deprecated in admiralonco 1.4.0.
      x This message will turn into a warning {at the beginning of 2027}.
      i See admiral's deprecation guidance: https://pharmaverse.github.io/admiraldev/dev/articles/programming_strategy.html#deprecation
    Condition
      Error in `filter_pd()`:
      ! The dataset name specified for `source_pd` must be included in the list specified for the `source_datasets` parameter. Following names were provided by `source_datasets`: "ars"

