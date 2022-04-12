#' Derive a Clinical Benefit Parameter
#'
#' Add a clinical benefit parameter to the input dataset.
derive_param_clinbenefit <- function(dataset,
                                     dataset_adsl,
                                     source_param,
                                     source_resp,
                                     source_pd,
                                     source_datasets,
                                     by_vars = NULL,
                                     reference_date = TRTSDT,
                                     ref_start_window = 28,
                                     set_values_to,
                                     subject_keys = vars(STUDYID, USUBJID)) {
  
  # Assertions and quotes
  
  assert_data_frame(
    dataset, 
    required_vars = quo_c(
      subject_keys,
      vars(PARAMCD, AVALC, ADT)
      )
  )
  assert_data_frame(
    dataset_adsl,
    required_vars = quo_c(
      subject_keys,
      reference_date
      )
  )
  params_available <- unique(dataset$PARAMCD)
  source_param <- assert_character_scalar(source_param, values = params_available)
  assert_vars(subject_keys)
  assert_list_of(source_resp, "tte_source")
  assert_list_of(source_pd, "tte_source")
  assert_list_of(source_datasets, "data.frame")
  source_names <- names(source_datasets)
  assert_vars(by_vars, optional = TRUE)
  assert_list_element(
    list = source_resp,
    element = "dataset_name",
    condition = dataset_name %in% source_names,
    source_names = source_names,
    message_text = paste0(
      "The dataset names must be included in the list specified for the ",
      "`source_datasets` parameter.\n",
      "Following names were provided by `source_datasets`:\n",
      enumerate(source_names, quote_fun = squote)
    )
  )
  assert_list_element(
    list = source_pd,
    element = "dataset_name",
    condition = dataset_name %in% source_names,
    source_names = source_names,
    message_text = paste0(
      "The dataset names must be included in the list specified for the ",
      "`source_datasets` parameter.\n",
      "Following names were provided by `source_datasets`:\n",
      enumerate(source_names, quote_fun = squote)
    )
  )
  
  reference_date <- assert_symbol(enquo(reference_date))
  ref_start_window <- assert_integer_scalar(ref_start_window)
  assert_varval_list(set_values_to, accept_expr = TRUE, optional = TRUE)
  assert_param_does_not_exist(dataset, quo_get_expr(set_values_to$PARAMCD))
  
  # ADSL
  
  adsl_vars <- vars(
    !!!subject_keys,
    !!reference_date
  )
  adsl <- dataset_adsl %>%
    select(!!!adsl_vars)
  
  # Find PDs
  
  pd_data <- filter_date_sources(
    sources = source_pd,
    source_datasets = source_datasets,
    by_vars = by_vars,
    subject_keys = subject_keys,
    mode = "first") 
  
  # Find responders
  
  resp_data <- filter_date_sources(
    sources = source_resp,
    source_datasets = source_datasets,
    by_vars = by_vars,
    subject_keys = subject_keys,
    mode = "first") 
  
  # Find those who are not responders but had a specific amount of time
  # between reference date and a valid non-PD measurement
  
  
  
}


#' Create a `date_source` Object
#'
#' The `date_source` object is used to dates of progressive disease (PD) and 
#' other responses as input for downstream `derive_param` functions in {admiralonco}

date_source <- function(dataset_name,
                        date,
                        filter = NULL) {
  out <- list(
    dataset_name = assert_character_scalar(dataset_name),
    date = assert_symbol(enquo(date)),
    filter = assert_filter_cond(enquo(filter), optional = TRUE)
    )
  
  class(out) <- c("tte_source", "source", "list")
  out
}