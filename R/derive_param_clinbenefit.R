#' Derive a Clinical Benefit Parameter
#'
#' Add a clinical benefit/disease control parameter to the input dataset. 
#' Clinical benefit/disease control is first identified for looking for subjects 
#' having objective response, and then derived for subjects that have at least 
#' one evaluable non-PD response assessment after a specified amount of time 
#' from a reference date.
#' 
#' @param dataset Input dataset. This is the dataset to which the clinical
#' benefit rate parameter will be added.
#' 
#'   The variables `PARAMCD`, `AVALC`, `ADT`, and those specified by the `by_vars` 
#'   parameter are expected. 
#' 
#' @param dataset_adsl ADSL dataset used as input for populating `subject_keys`
#' in input dataset. 
#' 
#'   The variables specified by the `subject_keys` parameter and the `reference_date`
#'   parameter are expected. 
#'   
#' @param source_param Character name of parameter code in `source_dataset` that
#' represents overall disease response assessment for a subject at a given timepoint,
#' e.g. `"OVR"` or `"OVRLRESP"`.
#' 
#' @param source_resp A `date_source` object specifying the dataset, date variable, 
#' and filter condition used to identify objective response status.
#' 
#' @param source_pd A `date_source` object specifying the dataset, date variable, 
#' and filter condition used to identify disease progression.
#' 
#' @param source_datasets Named list of length 1 specifying the dataset from 
#' which response information for deriving clinical benefit rate will be collected. 
#' 
#' @param by_vars A named list returned by `vars()` used together with `subject_keys` 
#' to identify groupings within which the earliest date of clinical benefit rate 
#' status will be chosen. 
#' 
#' @param reference_date Name of variable representing the index date for 
#' `ref_start_window`. A variable providing a date. An unquoted symbol is expected.
#' 
#' @param ref_start_window Integer representing number of days from `reference_date`
#' that must elapse before an evaluable non-PD assessment counts toward determining
#' clinical benefit.
#' 
#' @param set_values_to A named list returned by `vars()` containing new variables 
#' and their static value to be populated for the clinical benefit rate parameter 
#' records, e.g. `vars(PARAMCD = "CBR", PARAM = "Clinical Benefit Rate")`.
#' 
#' @param subject_keys A named list returned by `vars()` containing variables 
#' used to uniquely identify subjects.
#' 
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
  assert_vars(by_vars, optional = TRUE)
  reference_date <- assert_symbol(enquo(reference_date))
  assert_data_frame(
    dataset, 
    required_vars = vars(!!!by_vars, PARAMCD, AVALC, ADT)
  )
  assert_data_frame(
    dataset_adsl,
    required_vars = vars(!!!subject_keys, !!reference_date)
  )
  params_available <- unique(dataset$PARAMCD)
  source_param <- assert_character_scalar(source_param, values = params_available)
  assert_vars(subject_keys)
  assert_s3_class(source_resp, "date_source")
  assert_s3_class(source_pd, "date_source")
  assert_list_of(source_datasets, "data.frame")
  source_names <- names(source_datasets)
  
  assert_list_element(
    list = list(source_resp),
    element = "dataset_name",
    condition = dataset_name %in% source_names,
    source_names = source_names,
    message_text = paste0(
      "The dataset names must be included in the list specified for the ",
      "`source_datasets` parameter.\n",
      "Following names were provided by `source_datasets`:\n"
      #enumerate(source_names, quote_fun = squote)
    )
  )
  
  assert_list_element(
    list = list(source_pd),
    element = "dataset_name",
    condition = dataset_name %in% source_names,
    source_names = source_names,
    message_text = paste0(
      "The dataset names must be included in the list specified for the ",
      "`source_datasets` parameter.\n",
      "Following names were provided by `source_datasets`:\n"
      #enumerate(source_names, quote_fun = squote)
    )
  )
  
  ref_start_window <- assert_integer_scalar(ref_start_window)
  assert_varval_list(set_values_to, accept_expr = TRUE, optional = TRUE)
  assert_param_does_not_exist(dataset, quo_get_expr(set_values_to$PARAMCD))
  
  # ADSL variables
  
  adsl_vars <- vars(
    !!!subject_keys,
    !!reference_date)
  
  adsl <- dataset_adsl %>%
    select(!!!adsl_vars)
  
  # Get PD date and objective response date
  
  pd_data <- source_datasets[[source_pd$dataset_name]] %>%
    admiral::filter_if(source_pd$filter) %>%
    select(!!!subject_keys, !!!by_vars, !!source_pd$date) %>%
    rename(temp_pd = !!source_pd$date)
  
  rsp_data <- source_datasets[[source_resp$dataset_name]] %>%
    admiral::filter_if(source_resp$filter) %>%
    select(!!!subject_keys, !!!by_vars, !!source_resp$date) %>%
    rename(temp_rs = !!source_resp$date)
  
  # Look for valid non-PD measurements after window from reference date
  ovr_data <- source_datasets[[source_resp$dataset_name]] %>%
    left_join(
      adsl,
      by = vars2chr(subject_keys)
    ) %>%
    left_join(
      pd_data,
      by = vars2chr(subject_keys)
    ) %>%
    filter(PARAMCD == source_param & 
      !(AVALC %in% c("NA", "NE", "ND")) & !is.na(AVALC) &
      # This line will become filter_pd
      (is.na(temp_pd) | (!is.na(temp_pd) & ADT < temp_pd)) &
      # end filter_pd
      ADT >= !!reference_date + days(ref_start_window)
    ) %>% 
    anti_join(
      ., 
      rsp_data, 
      by = vars2chr(subject_keys) 
    ) %>%
    filter_extreme(
      order = vars(ADT),
      by_vars = vars(!!!subject_keys, !!!by_vars),
      mode = "first",
      check_type = "none"
    ) %>%
    select(!!!subject_keys, !!!by_vars, ADT)
  
  rsp_data <- rsp_data %>%
    rename(ADT=temp_rs)
  
  new_param <- bind_rows(ovr_data, rsp_data) %>%
    right_join(adsl, by=vars2chr(subject_keys)) %>%
    mutate(AVALC=if_else(!is.na(ADT), "Y", "N")) %>%
    select(-!!reference_date)
  
  tryCatch(
    new_param <- mutate(new_param, !!!set_values_to),
    error = function(cnd) {
      abort(
        paste0(
          "Assigning new variables failed!\n",
          "set_values_to = (\n",
          paste(
            " ",
            names(set_values_to),
            "=",
            lapply(set_values_to, quo_get_expr),
            collapse = "\n"
          ),
          "\n)\nError message:\n  ",
          cnd
        )
      )
    }
  )
  
  bind_rows(dataset, new_param)
  
}