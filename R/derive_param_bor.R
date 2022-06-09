#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
#' @description
#'     Derive Best Overall Response Parameter
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @details
#'    Calculates the best overall response (BOR) parameter for subjects.
#'    Records after PD can be removed using the source_pd and source_datasets
#'    arguments.
#'
#'   Note: 
#'   
#'   1. All records where `ADT` >= `reference_date` + `ref_start_window` are considered
#'      for Best Overall Response.
#'      
#'   2. All records where `ADT` < `reference_date` + `ref_start_window`  and the subject
#'      has a record in step 1, are considered for Best Overall Response.
#'      
#'   3. Records where `ADT` < `reference_date` + `ref_start_window`  and the subject does 
#'      not have a record in step 1 and AVALC %in% c("CR", "PR", "PD") are considered for
#'      Best Overall Response
#'      
#'   4  Records where `ADT` < `reference_date` + `ref_start_window` and the subject does 
#'      not have a record in step 1 and AVALC %in% c('SD', 'NON-CR/NON-PD') then
#'      Best Overall Response is set to NE.
#'      
#'   5. The `AVAL` variable is added and set using the `aval_fun(AVALC)` function.
#'
#'   6. The variables specified by the `set_values_to` parameter and records
#'      are added to the dataframe passed into the `dataset` argument
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
# Function Arguments:
#'
#' @param dataset The input dataframe from which the Best Overall Response will 
#'                be derived from and added to.
#'
#'   The columns `PARAMCD`, `ADT`, and `AVALC`and the columns specified in
#'   `subject_keys` and `reference_date` are expected.
#'
#' @param dataset_adsl The ADSL input dataframe
#'
#'   The columns specified in `subject_keys` are expected.
#'
#' @param filter_source Filter to be applied to `dataset` to derive the
#'                       Best Overall Response
#'
#' @param source_pd Date of first progressive disease (PD)
#'
#'   If the parameter is specified, the observations of the input `dataset` for
#'   deriving the new parameter are restricted to observations up to the
#'   specified date. Observations at the specified date are included. For
#'   subjects without first PD date all observations are take into account.
#'
#'   *Permitted Values:* a `date_source` object (see `admiral::date_source()`
#'   for details)
#'
#'   *Default:* `NULL`,
#'
#' @param source_datasets Source dataframe to be used to calculatethe first PD date
#'
#'   A named list of dataframes is expected (although for BoR) only one dataframe is
#'   needed. It links the `dataset_name` from `source_pd` with an existing dataframe.
#'
#'   For example if `source_pd = pd_date` with
#'   ```
#'   pd_date <- date_source(
#'     dataset_name = "adrs",
#'     date = ADT,
#'     filter = PARAMCD == PD
#'   )
#'   ```
#'   and the actual response dataframe in the script is `myadrs`, `source_datasets
#'   = list(adrs = myadrs)` should be specified.
#'
#' @param reference_date Reference date
#'
#'   The reference date is used along with `ref_start_window` to determine those 
#'   records that occur before and after `ADT` (see Details section for further
#'   information). Usually it is treatment start date (`TRTSDT`) or 
#'   randomization date (`RANDDT`).
#'   
#'   *Permitted Values:* a numeric date variable
#'
#' @param ref_start_window Stable disease time window
#'
#'   The ref_start_window is used along with `reference_date` to determine those 
#'   records that occur before and after `ADT` (see Details section for further
#'   information). 
#'
#'   *Permitted Values:* a non-negative numeric scalar
#'
#'   *Default:* 0
#'
#' @param missing_as_ne Consider no assessments as `"NE"`?
#'
#'   If the argument is set to `TRUE`, the response is set to `"NE"` for
#'   subjects in `dataset_adsl` without an assessment in the `dataset` after the 
#'   filter has been applied. Otherwise, the response is set to `"MISSING"` 
#'   for these subjects.
#'
#'   *Permitted Values:* a logical scalar
#'
#'   *Default:* `FALSE`
#'
#' @param aval_fun Function to map character analysis value (`AVALC`) to numeric
#'   analysis value (`AVAL`)
#'
#'   The (first) argument of the function must expect a character vector and the
#'   function must return a numeric vector.
#'
#'   *Default:* `aval_resp` (see `aval_resp()` for details)
#'
#' @param set_values_to Variables to set
#'
#'   A named list returned by `vars()` defining the variables to be set for the
#'   new parameter, e.g. `vars(PARAMCD = "CBOR", PARAM = "Best Overall
#'   Response")` is expected. The values must be symbols, character strings,
#'   numeric values, or `NA`.
#'
#' @param subject_keys Variables to uniquely identify a subject
#'
#'   A list of symbols created using `vars()` is expected.
#'
#' @examples
#'
#' # Create ADSL dataset
#' adsl <- tibble::tribble(
#'   ~USUBJID, ~TRTSDTC,
#'   "1",      "2020-01-01",
#'   "2",      "2019-12-12",
#'   "3",      "2019-11-11",
#'   "4",      "2019-12-30",
#'   "5",      "2020-01-01",
#'   "6",      "2020-02-02",
#'   "7",      "2020-02-02",
#'   "8",      "2020-04-01"
#'   ) %>%
#'   dplyr::mutate(TRTSDT = lubridate::ymd(TRTSDTC),
#'                 STUDYID = "XX1234")
#'
#' # Create ADRS dataset
#' ovr_obs <- tibble::tribble(
#'   ~USUBJID, ~ADTC,        ~AVALC,
#'   "1",      "2020-01-01", "PR",
#'   "1",      "2020-02-01", "CR",
#'   "1",      "2020-02-16", "NE",
#'   "1",      "2020-03-01", "CR",
#'   "1",      "2020-04-01", "SD",
#'   "2",      "2020-01-01", "SD",
#'   "2",      "2020-02-01", "PR",
#'   "2",      "2020-03-01", "SD",
#'   "2",      "2020-03-13", "CR",
#'   "3",      "2019-11-12", "CR",
#'   "3",      "2019-12-02", "CR",
#'   "3",      "2020-01-01", "SD",
#'   "4",      "2020-01-01", "PR",
#'   "4",      "2020-03-01", "SD",
#'   "4",      "2020-04-01", "SD",
#'   "4",      "2020-05-01", "PR",
#'   "4",      "2020-05-15", "NON-CR/NON-PD",
#'   "5",      "2020-01-01", "PR",
#'   "5",      "2020-01-10", "SD",
#'   "5",      "2020-01-20", "PR",
#'   "5",      "2020-05-15", "NON-CR/NON-PD",
#'   "6",      "2020-02-06", "PR",
#'   "6",      "2020-02-16", "CR",
#'   "6",      "2020-03-30", "PR",
#'   "6",      "2020-04-12", "PD",
#'   "6",      "2020-05-01", "CR",
#'   "6",      "2020-06-01", "CR",
#'   "7",      "2020-02-06", "PR",
#'   "7",      "2020-02-16", "CR",
#'   "7",      "2020-04-01", "NE"
#'   ) %>%
#'   dplyr::mutate(PARAMCD = "OVR")
#'
#' pd_obs <-
#'   dplyr::bind_rows(tibble::tribble(
#'     ~USUBJID, ~ADTC,        ~AVALC,
#'     "2",      "2020-03-01", "Y",
#'     "4",      "2020-02-01", "Y"
#'     ) %>%
#'     dplyr::mutate(PARAMCD = "PD"))
#'     
#' adrs <- dplyr::bind_rows(ovr_obs, pd_obs) %>%
#'   dplyr::mutate(ADT     = lubridate::ymd(ADTC),
#'                 STUDYID = "XX1234") %>%
#'   dplyr::select(-ADTC) %>%
#'   admiral::derive_vars_merged(
#'     dataset_add = adsl,
#'     by_vars     = dplyr::vars(STUDYID, USUBJID),
#'     new_vars    = dplyr::vars(TRTSDT) )
#'
#' pd_date <- admiral::date_source(
#'   dataset_name = "adrs",
#'   date         = ADT,
#'   filter       = PARAMCD == "PD")
#'
#' # Derive best overall response parameter
#' derive_param_bor(
#'   adrs,
#'   dataset_adsl     = adsl,
#'   filter_source    = PARAMCD == "OVR",
#'   source_pd        = pd_date,
#'   source_datasets  = list(adrs = adrs),
#'   reference_date   = TRTSDT,
#'   ref_start_window = 28,
#'   set_values_to    = dplyr::vars(PARAMCD = "BOR",
#'                                  PARAM   = "Best Overall Response")
#' ) %>%
#'   dplyr::filter(PARAMCD == "BOR")
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @export
#' @name   derive_param_bor
#' @title  derive_param_bor
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @author Stephen Gormley
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @keywords ADRS
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @return The dataframe passed in the `dataset` argument with additional columns
#'         and/or rows as set in the `set_values_to` argument.
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|

derive_param_bor <- function(dataset,
                             dataset_adsl,
                             filter_source,
                             source_pd        = NULL,
                             source_datasets  = NULL,
                             reference_date,
                             ref_start_window = 0,
                             missing_as_ne    = FALSE,
                             aval_fun         = admiralonco::aval_resp(),
                             set_values_to,
                             subject_keys     = admiral::vars(STUDYID, USUBJID)) {

  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Assert statements ----
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  filter_source  <- admiral::assert_filter_cond(arg = rlang::enquo(filter_source))
  reference_date <- admiral::assert_symbol(arg = rlang::enquo(reference_date))

  admiral::assert_integer_scalar(arg      = ref_start_window, 
                                 subset   = "non-negative",
                                 optional = TRUE)
  
  admiral::assert_logical_scalar(arg = missing_as_ne)
  
  admiral::assert_function(arg = aval_fun)
  
  admiral::assert_varval_list(arg               = set_values_to, 
                              required_elements = "PARAMCD")
  
  admiral::assert_vars(arg = subject_keys)
  
  admiral::assert_data_frame(arg           = dataset,
                             required_vars = admiral:::quo_c(subject_keys, 
                                                             reference_date, 
                                                             admiral::vars(PARAMCD, ADT, AVALC)))
  
  admiral::assert_data_frame(arg           = dataset_adsl, 
                             required_vars = subject_keys)
  
  admiral::assert_param_does_not_exist(dataset = dataset, 
                                       param   = rlang::quo_get_expr(set_values_to$PARAMCD))

  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # filter_pd and filter_source: Filter source dataset using filter_source----
  # argument and also filter data after progressive disease with filter_pd
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  if (!is.null(source_pd)) {

    # asserts on the pd data
    source_names <- names(source_datasets)
    
    admiral::assert_list_element(
      list         = list(source_pd),
      element      = "dataset_name",
      condition    = dataset_name %in% source_names,
      source_names = source_names,
      message_text = paste0(
        "The dataset names must be included in the list specified for the ",
        "`source_datasets` parameter.\n",
        "Following names were provided by `source_datasets`:\n",
        admiral:::enumerate(source_names, quote_fun = sQuote))
    )
    
    admiral::assert_s3_class(arg   = source_pd, 
                             class = "date_source")
    
    admiral::assert_data_frame(arg = eval(rlang::parse_expr(source_pd$dataset_name)))
    
    dataset_filter <- dataset %>%
      filter_pd(filter          = !!enquo(filter_source),
                source_pd       = source_pd,
                source_datasets = source_datasets,
                subject_keys    = subject_keys)
    
  } else {
    
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # filter_source: Filter using filter_source argument ----
    # This would also be used to filter out records from dataset that are greater
    # than e.g. ADSL.TRTSDT
    # Not filtering data after progressive disease with filter_pd
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    dataset_filter <- dataset %>%
      dplyr::filter(!!rlang::enquo(filter_source))
    
  }
                
  # Error if filter results in 0 records
  if (nrow(dataset_filter) == 0) {
    err_msg <- sprintf(
      "dataframe passed into %s argument with the filter %s has 0 records",
      "dataset",
      deparse(rlang::quo_get_expr(filter_source)))
    
    rlang::abort(err_msg)
  }
  
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Create the response dataframes ----
  #
  # Three: 
  #  1.  Records where ADT >= reference_date + ref_start_window
  #  2.  Records where ADT < reference_date + ref_start_window 
  #      but subject does have a subsequent read in dataframe 1.
  #  3.  Records where ADT < reference_date + ref_start_window
  #      but subject does not have a subsequent read in dataframe 1.
  #      (this is NE for subects with 'SD' or 'NON-CR/NON-PD')
  #
  #      Requirement: BOR is set to 'NE', in the case where the subject has only 
  #      AVALC = 'SD' or 'NON-CR/NON-PD' less than xxx days after the reference date 
  #      from ADSL.
  #  4.  Subjects in adsl and not in dataset_filter
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  # data where ADT >= reference_date + days(ref_start_window)
  after_ref_data <- dataset_filter %>% 
    dplyr::filter(ADT >= !!reference_date + lubridate::days(ref_start_window)) 
  
  after_ref_data <- after_ref_data[c(-14),]
  
  # create list of unique subject_keys with at least one record 
  # where ADT >= reference_date + days(ref_start_window)
  subjects_with_record_after_ref_data <- after_ref_data %>% 
    dplyr::distinct(!!!subject_keys)
  
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Data frame 1: bor_data_01, ADT >= reference_date + ref_start_window
  #               and assign sort order to select best (i.e. lowest) later
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  bor_data_01 <- after_ref_data %>%
    dplyr::mutate(tmp_order = dplyr::case_when(AVALC %in% c("CR") ~ 1,
                                               AVALC %in% c("PR") ~ 2,
                                               AVALC %in% c("SD") ~ 3,
                                               AVALC %in% c("NON-CR/NON-PD") ~ 4,
                                               AVALC %in% c("PD") ~ 5,
                                               AVALC %in% c("NE") ~ 6,
                                               is.null(AVALC) ~ 7)) %>%
    dplyr::select(!!!subject_keys, AVALC, tmp_order, ADT)
  
  # data where ADT < reference_date + days(ref_start_window)
  before_ref_data <- dataset_filter %>% 
    dplyr::filter(ADT < !!reference_date + lubridate::days(ref_start_window)) %>%
    dplyr::mutate(tmp_record_after_reference = FALSE) %>%
    dplyr::select(!!!subject_keys, AVALC, ADT)
  
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Data frame 2: bor_data_02, ADT < reference_date + ref_start_window 
  #               but subject does have a subsequent read in dataframe 1.
  #               and assign sort order to select best (i.e. lowest) later
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  bor_data_02 <- subjects_with_record_after_ref_data %>% 
    dplyr::left_join(before_ref_data) %>%
    dplyr::mutate(tmp_order = dplyr::case_when(AVALC %in% c("CR") ~ 1,
                                               AVALC %in% c("PR") ~ 2,
                                               AVALC %in% c("SD") ~ 3,
                                               AVALC %in% c("NON-CR/NON-PD") ~ 4,
                                               AVALC %in% c("PD") ~ 5,
                                               AVALC %in% c("NE") ~ 6,
                                               is.null(AVALC) ~ 7))
  
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Data frame 3: bor_data_03, ADT < reference_date + ref_start_window
  #               but subject does not have a subsequent read in dataframe 1.
  #               and assign sort order to select best (i.e. lowest) later.
  #               (Note: this is NE for subects with 'SD' or 'NON-CR/NON-PD')
  # 
  # Note: dplyr::anti_join(before_ref_data, subjects_with_record_after_ref_data)
  #       All rows in before_ref_data that do not have a match in
  #       subjects_with_record_after_ref_data.
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  bor_data_03 <- before_ref_data  %>% 
    dplyr::anti_join(subjects_with_record_after_ref_data) %>%
    dplyr::mutate(tmp_order = dplyr::case_when(AVALC %in% c("CR") ~ 1,
                                               AVALC %in% c("PR") ~ 2,
                                               AVALC %in% c("PD") ~ 5,
                                               TRUE ~ 6))
  
  # check nothing strange has gone on with joins
  assertthat::are_equal(nrow(bor_data_03) + nrow(bor_data_02), 
                        nrow(before_ref_data))
  
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Data frame 4: bor_data_04, Subjects in adsl and not in dataset_filter
  #               tmp_order set to 999 so last in order when binded later.
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  bor_data_04 <- dataset_adsl %>%
    dplyr::select(!!!subject_keys) %>%
    dplyr::mutate(AVALC     = dplyr::case_when(isTRUE(missing_as_ne) ~ "NE",
                                               TRUE ~"MISSING"),
                  tmp_order = 999) %>%
    dplyr::select(!!!subject_keys, AVALC, tmp_order)
  
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Bind three types of dataframes and select lowest value as BOR
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  param_bor <- dplyr::bind_rows(bor_data_01, 
                                bor_data_02, 
                                bor_data_03,
                                bor_data_04) %>%
    admiral::filter_extreme(by_vars = subject_keys,
                            order   = admiral::vars(tmp_order, ADT),
                            mode    = "first") %>%
    dplyr::select(-tmp_order)

  
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # set_values_to: Execute set_values_to ----
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  tryCatch(
    
    param_bor_values_set <- param_bor %>%
      dplyr::mutate(AVAL = aval_fun(AVALC),
                    !!!set_values_to),
    
    error = function(cnd) {
      rlang::abort(
        paste0(
          "Assigning new columns with set_values_to has failed:\n",
          "set_values_to = (\n",
          paste(
            " ",
            names(set_values_to),
            "=",
            lapply(set_values_to, rlang::quo_get_expr),
            collapse = "\n"
          ),
          "\n)\nError message:\n  ",
          cnd
        )
      )
    })
  
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Bind back to passed dataset and return ----
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  return_dataframe <- dplyr::bind_rows(dataset,
                                       param_bor_values_set)
  
  # check nothing strange has gone on with joins
  assertthat::are_equal(nrow(return_dataframe),
                        nrow(dataset) +
                          nrow(param_bor))
  
  return(return_dataframe)
}



#' Map Character Response Values to Numeric Values
#'
#' Map character response values like `"PR"` or `"SD"` to numeric values.
#'
#' @param arg Character vector
#'
#' @author Stefan Bundfuss
#'
#' @keywords user_utility
#'
#' @export
#'
#' @return
#'
#' - `1` if `arg` equals `"CR"`,
#' - `2` if `arg` equals `"PR"`,
#' - `3` if `arg` equals `"SD"`,
#' - `4` if `arg` equals `"NON-CR/NON-PD"`,
#' - `5` if `arg` equals `"PD"`,
#' - `6` if `arg` equals `"NE"`,
#' - `7` if `arg` equals `"MISSING"`,
#' - `NA_real_` otherwise
#'
#' @examples
#'
#' aval_resp(c("CR", "PR", "SD", "NON-CR/NON-PD", "PD", "NE", "MISSING", "ND", NA_character_))
aval_resp <- function(arg) {
  assert_character_vector(arg)
  case_when(
    arg == "CR" ~ 1,
    arg == "PR" ~ 2,
    arg == "SD" ~ 3,
    arg == "NON-CR/NON-PD" ~ 4,
    arg == "PD" ~ 5,
    arg == "NE" ~ 6,
    arg == "MISSING" ~ 7,
    TRUE ~ NA_real_
  )
}