#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
#' @description
#'     Add a Last Disease Assessment parameter for each unique `by_vars` to the 
#'     input dataframe passed into the dataset argument.
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @details
#'    Calculates the last disease assessment for subjects (censored if
#'    required using argument source_pd which will remove all records in the
#'    dataframe being passed that occur after the date in source_pd)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
# Function Arguments:
#
#' @param dataset Input dataframe to which the Last Disease Assessment
#'                parameter will be added. The variables `PARAMCD`, `AVAL`,
#'                `AVALC`, `ADT`, and those specified in the `order` and
#'                `by_vars` argument are expected.
#'
#' @param order Sort order, after which the last record shall be taken by
#'              the `by_vars` to determine Last Disease Assessment
#'
#' @param by_vars Grouping variables, the last of which (ordered by `order`)
#'                shall be taken as the Last Disease Assessment record
#'
#' @param filter_source Filter to be applied to the input dataframe to derive
#'                      the Last Disease Assessment
#'
#' @param source_pd A `date_source` object specifying the dataset_name, date
#'                  variable, and filter condition used to identify disease
#'                  progression and subsequently used (if provided) to remove
#'                  all records after disease progression from `dataset`
#'
#' @param source_datasets A named `list` containing the name of the dataframe
#'                        in which to search for the progressive disease as
#'                        defined in `source_pd`
#'
#' @param subject_keys Variables to uniquely identify a subject, used by
#'                     `source_pd` to specify a date
#'
#' @param set_values_to A named `list` returned by `vars()` containing new
#'                      variables and their static value to be populated for
#'                      the Last Disease Assessment records, e.g.
#'                      vars(PARAMCD = "LSTAC", PARAM = "Last Disease
#'                      Assessment Censored at First PD by Investigator")`
#'
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @examples
#'
#' \dontrun{
#' pd <- date_source(
#'   dataset_name = “adrs”,
#'   date         = ADT,
#'   filter       = PARAMCD == “PD” & AVALC == “Y”
#' )
#'
#' derive_param_lasta(
#'   dataset,
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
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @export
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @author Stephen Gormley
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @keyword ADRS Last Disease Assesment
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @return The dataframe passed in the `dataset` argument with additonal columns
#'         and/or rows as set in the `set_values_to` argument.
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|

derive_param_lasta <- function(dataset,
                               order           = admiral::vars(USUBJID, ADT),
                               by_vars         = admiral::vars(USUBJID),
                               filter_source   = PARAMCD == "OVR" & ANL01FL == "Y",
                               source_pd       = NULL,
                               source_datasets = list(adrs = adrs),
                               subject_keys    = admiral::vars(USUBJID),
                               set_values_to) {

  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Assert statements ----
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  filter_source <- admiral::assert_filter_cond(enquo(filter_source),
                                               optional = TRUE)
  admiral::assert_vars(by_vars)
  admiral::assert_data_frame(dataset,
                             required_vars = admiral::vars(!!!order,
                                                           !!!by_vars,
                                                           PARAMCD, AVAL,
                                                           AVALC, ADT))
  admiral::assert_varval_list(set_values_to,
                              accept_expr = TRUE)
  admiral::assert_param_does_not_exist(dataset,
                                       rlang::quo_get_expr(set_values_to$PARAMCD))

  if (!is.null(source_pd)) {
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
  }

  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # source_pd: Get PD date from source_pd dataframe, if passed ----
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  if (!is.null(source_pd)) {

    admiral::assert_s3_class(source_pd, "date_source")
    admiral::assert_data_frame(eval(rlang::parse_expr(source_pd$dataset_name)))

    warning("USER WARNING: The following should be replaced by FILTER_PD .")

    pd_data <- eval(rlang::parse_expr(source_pd$dataset_name)) %>%
      admiral::filter_if(source_pd$filter) %>%
      dplyr::select(!!!subject_keys, !!source_pd$date) %>%
      dplyr::rename(temp_pd_date = !!source_pd$date)

    # select first PD Date
    pd_data_first <- pd_data %>%
      dplyr::arrange(!!!subject_keys, temp_pd_date) %>%
      dplyr::group_by(!!!subject_keys) %>%
      dplyr::filter(row_number() == 1)

    # check nothing strange has gone on with above
    assertthat::are_equal(nrow(pd_data_first),
                          nrow(pd_data %>% dplyr::distinct(!!!subject_keys)))

    dataset_censor <- dataset %>%
      dplyr::left_join(pd_data_first,
                       by = admiral::vars2chr(subject_keys)) %>%
      dplyr::filter(is.na(temp_pd_date) |
                      (!is.na(temp_pd_date) & ADT < temp_pd_date))

  } else {

    dataset_censor <- dataset

  }

  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # filter_source: Filter using filter_source argument ----
  # This would also be used to filter out records from dataset that are greater
  # than e.g. ADSL.TRTSDT
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  dataset_filter <- dataset_censor %>%
    dplyr::filter(!!!filter_source)

  # Error if filter results in 0 records
  if (nrow(dataset_filter) == 0) {
    err_msg <- sprintf(
      "dataframe passed into %s argument with the filter %s has 0 records",
      "dataset",
      deparse(rlang::quo_get_expr(filter_source)))

    rlang::abort(err_msg)
  }

  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # filter_extreme: Filter last assessment using filter_extreme ----
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  param_lasta <- dataset_filter %>%
      admiral::filter_extreme(mode       = "last",
                              order      = admiral::vars(!!!order),
                              by_vars    = admiral::vars(!!!by_vars),
                              check_type = "warning")

  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # set_values_to: Execute set_values_to ----
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  tryCatch(

    param_lasta_values_set <- param_lasta %>%
      dplyr::mutate(!!!set_values_to),

    error = function(cnd) {
      abort(
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
                                       param_lasta_values_set)

  # check nothing strange has gone on with joins
  assertthat::are_equal(nrow(return_dataframe),
                        nrow(dataset) +
                          nrow(param_lasta))

  return(return_dataframe)

}
