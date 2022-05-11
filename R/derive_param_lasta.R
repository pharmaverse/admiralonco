#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
#' @description
#'     Add a Last Disease Assessment parameter to the input dataset.
#'     
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @details
#'    Calculates the last disease assessment for subjects censored (if required) using argument source_pd
#'    which will remove all records in the dataset being passed that occur after the date in source_pd
#'    
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
# Function Arguments: 
#
#' @param dataset Input dataframe to which the Last clinical assesment parameter will be added.
#'
#'   The variables `PARAMCD`, `AVAL`, `AVALC`, and those specified by the `order`
#'   argument are expected.
#'
#' @param order Sort order in which the last record shall be taken by USUBJID to calculate
#'              Last Disease Assessment
#'                        
#' @param by_vars Grouping variables, the last of which (as ordered by order) shall be taken as the 
#'                 Last Disease Assessment record
#'                  
#' @param dataset_adsl ADSL dataset used as input for populating `subject_keys`
#'                     in input dataset.
#'
#'   The variables specified by the `subject_keys` parameter and the `reference_date`
#'   parameter are expected.
#'
#' @param filter_source Filter to be applied on dataset from which to derive the Last Disease Assessment
#'
#' @param source_pd A `date_source` object specifying the dataset, date variable,
#'                  and filter condition used to identify disease progression.
#'
#' @param source_datasets A named `list` containing datasets in which to search for the
#'                        progressive disease as defined in source_pd.

#' 
#' @param set_values_to A named list returned by `vars()` containing new variables
#'                      and their static value to be populated for the Last Disease Assessment
#'                      records, e.g. `vars(PARAMCD = "LSTAC", PARAM = "Last Disease Assessment Censored at First PD by Investigator")`.
#'
#' @param subject_keys   TBC [IF Needed]
#' 
#' @param keep_adsl_vars TBC [IF Needed]
#' 
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @examples
#'
#' \dontrun{
#' pd <- date_source(
#'   dataset_name = “adrs”,
#'   date = ADT,
#'   filter = PARAMCD == “PD” & AVALC == “Y”
#' )
#' 
#' derive_param_lasta(
#'   dataset,
#'   dataset_adsl = adsl,
#'   filter_source = PARAMCD == "OVR" & ANL01FL == "Y",
#'   source_pd = list(pd),
#'   source_datasets = list(adrs = adrs),
#'   set_values_to = vars(
#'     PARAMCD = "LSTAC",
#'     PARAM = "Last Disease Assessment Censored at First PD by Investigator",
#'     PARCAT1 = "Tumor Response",
#'     PARCAT2 = "Investigator",
#'     PARCAT3 = "Recist 1.1",
#'     ANL01FL = "Y")
#' )
#'}
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @export
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @author Stephen Gormley
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @keyword ADRS Last Disease Assesment
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @return The dataframe passed in the dataset argument with additona columns as in set_values_to argument
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|

derive_param_lasta <- function(dataset,
                               order           = admiral::vars(USUBJID, ADT),
                               by_vars         = admiral::vars(USUBJID),
                               dataset_adsl,
                               filter_source   = PARAMCD == "OVR" & ANL01FL == "Y",
                               source_pd       = NULL,
                               source_datasets = list(adrs = adrs),
                               keep_adsl_vars  = NULL,
                               set_values_to,
                               subject_keys    = admiral::vars(USUBJID)) {
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Assert statements ----
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  filter_source <- admiral::assert_filter_cond(enquo(filter_source), optional = TRUE)
  admiral::assert_vars(by_vars)
  admiral::assert_data_frame(dataset_adsl)
  admiral::assert_data_frame(dataset_adsl, required_vars = vars(USUBJID))
  admiral::assert_data_frame(dataset, 
                             required_vars = admiral::vars(!!!by_vars, PARAMCD, AVAL, AVALC, ADT))
  admiral::assert_varval_list(set_values_to, 
                              accept_expr = TRUE, 
                              optional    = TRUE)
  admiral::assert_param_does_not_exist(dataset, rlang::quo_get_expr(set_values_to$PARAMCD))
  
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
        admiral:::enumerate(source_names, quote_fun = sQuote)
      )
    )
  dataset <<- dataset
  dataset_adsl <<- dataset_adsl
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Select adsl_cols ---- 
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  adsl_cols <- dataset_adsl %>% 
    dplyr::select(USUBJID, !!!keep_adsl_vars)
  
  warning("USER WARNING: Double check this, not sure this is necessary")
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Get PD date from PD_SOURCE dataset if passed ---- 
  # Is this either ADSL or ADRS PD? I think so, in which case why do we have dataset_ADSL?
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  if(!is.null(source_pd)) {
    admiral::assert_s3_class(source_pd, "date_source")
    admiral::assert_data_frame(eval(rlang::parse_expr(source_pd$dataset_name)))
    
    pd_data <- eval(rlang::parse_expr(source_pd$dataset_name)) %>%
      admiral::filter_if(source_pd$filter) %>%
      dplyr::select(!!!subject_keys, !!!by_vars, !!source_pd$date) %>%
      dplyr::rename(temp_pd_date = !!source_pd$date)
    
    dataset <- dataset %>% 
      dplyr::left_join(pd_data, by = "USUBJID") %>%
      dplyr::filter(is.na(temp_pd_date) | (!is.na(temp_pd_date) & ADT <= temp_pd_date))
  }
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Filter using filter_source argument ----
  # This would also be used to filter out records from dataset that are greater than
  # e.g. ADSL.TRTSDT
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  filter_source <<-filter_source
  dataset_filter <- dataset %>%
    dplyr::filter(!!filter_source)  # what about NE's and others like UNDEFIINED, NON-CR? In here or argument?
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Filter last assessment using filter_extreme ----
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  param_lasta <- dataset_filter %>%
      admiral::filter_extreme(mode       = "last",
                              order      = vars(!!!order),
                              by_vars    = vars(!!!by_vars, ADT),
                              check_type = "warning")
  
  warning("USER WARNING: Ask how I should provide a more informative error?")
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Join with ADSL such that all subjects in ADSL have a PARAM even with no assessment ----
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  param_lasta_with_adsl <- param_lasta %>%
    dplyr::right_join(adsl_cols, by = "USUBJID") 
  
  warning("USER WARNING: Double check this, as not 100% sure")

  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Execute set_values_to ----
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  tryCatch(
    
    param_lasta_with_adsl_values_set <- param_lasta_with_adsl %>% 
      dplyr::mutate(!!!set_values_to),
    
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
    })
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # bind back to passed dataset and return ----
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  return_dataframe <- dplyr::bind_rows(dataset, 
                                       param_lasta_with_adsl_values_set)
  
  
  # check nothing strange has gone on with joins
  assertthat::are_equal(nrow(return_dataframe), 
                        nrow(dataset) + nrow(param_lasta_with_adsl %>% dplyr::filter(is.null(ADT))) +  nrow(param_lasta_with_adsl %>% dplyr::filter(!is.null(ADT))))

  
  return(return_dataframe)

}