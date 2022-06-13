#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
#' @description
#'     Add a Last Disease Assessment parameter for each unique `by_vars` to the
#'     input dataframe passed into the dataset argument.
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @details
#'    Calculates the last disease assessment.
#'    Records after PD can be removed using the source_pd and source_datasets
#'    arguments.
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
# Function Arguments:
#
#' @param dataset Input dataframe from which the Last Disease Assessment will be
#'                be derived from and added to.
#'
#'   The columns `PARAMCD`, `ADT`, and `AVALC`and the columns specified in
#'   `subject_keys` and `reference_date` are expected.
#'
#'    *Permitted Values:* a `data.frame()` object
#'
#'    *Required or Optional:* Required
#'
#' @param dataset_adsl The ADSL input dataframe
#'
#'   The columns specified in `subject_keys` are expected.
#'
#'    *Permitted Values:* a `data.frame()` object
#'
#'    *Required or Optional:* Required
#'
#' @param filter_source Filter to be applied to `dataset` to derive the
#'                      Last Disease Assessment
#'
#'    *Required or Optional:* Required
#'
#' @param order Sort order, after which the last record shall be taken by
#'              the `by_vars` to determine Last Disease Assessment. Created
#'              using `admiral::vars()`.
#'
#'    *Permitted Values:* an `admiral::vars` object
#'
#'    *Default:* `admiral::vars(STUDYID, USUBJID, ADT)`
#'
#'    *Required or Optional:* Required
#'
#' @param by_vars Grouping variables, the last of which (ordered by `order`)
#'                shall be taken as the Last Disease Assessment record. Created
#'                using `admiral::vars()`.
#'
#'    *Permitted Values:* an `admiral::vars` object
#'
#'    *Default:* `admiral::vars(STUDYID, USUBJID)`
#'
#'    *Required or Optional:* Required
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
#'   *Default:* `NULL`
#'
#'   *Required or Optional:* Optional
#'
#' @param source_datasets Source dataframe to be used to calculate the
#'                        first PD date
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
#'    *Required or Optional:* Optional
#'
#' @param subject_keys Variables to uniquely identify a subject
#'
#'   A list of symbols created using `admiral::vars()`.
#'
#'   *Default:* `admiral::vars(STUDYID, USUBJID)`
#'
#'   *Required or Optional:* Required
#'
#' @param set_values_to Variables to set
#'
#'   A named list returned by `vars()` defining the variables to be set for the
#'   new parameter, e.g. `vars(PARAMCD = "LSTAC", PARAM = "Last Disease
#'    Assessment Censored at First PD by Investigator")` is expected. The values
#'    must be symbols, character strings, numeric values, or `NA`.
#'
#'    *Required or Optional:* Required
#'
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @examples
#'
#' \dontrun{
#'
#' library(magrittr)
#'
#' adsl <- tibble::tribble(
#'   ~USUBJID, ~TRTSDT,                      ~EOSDT,
#'   "01",     lubridate::ymd("2020-12-06"), lubridate::ymd("2022-03-06"),
#'   "02",     lubridate::ymd("2021-01-16"), lubridate::ymd("2022-02-03"),
#'   "03",     lubridate::ymd("2021-01-09"), lubridate::ymd("2021-02-24"),
#'   "04",     lubridate::ymd("2021-04-21"), lubridate::ymd("2021-09-15")
#' ) %>%
#'   dplyr::mutate(STUDYID = "a_study_id")
#'
#' adrs <- tibble::tribble(
#'   ~USUBJID, ~PARAMCD, ~AVAL, ~AVALC, ~ASEQ, ~ADT,
#'   "01", "RSP", NA, "Y", 1, lubridate::ymd("2021-04-08"),
#'   "02", "RSP", NA, "N", 1, lubridate::ymd("2021-05-07"),
#'   "03", "RSP", NA, "N", 1, NA,
#'   "04", "RSP", NA, "N", 1, NA,
#'   "01", "PD",  NA, "N", 1, NA,
#'   "02", "PD",  NA, "Y", 1, lubridate::ymd("2021-05-07"),
#'   "03", "PD",  NA, "N", 1, NA,
#'   "04", "PD",  NA, "N", 1, NA,
#'   "01", "OVR", 3, "SD", 1, lubridate::ymd("2021-03-07"),
#'   "01", "OVR", 2, "PR", 1, lubridate::ymd("2021-04-08"),
#'   "02", "OVR", 3, "SD", 1, lubridate::ymd("2021-03-07"),
#'   "02", "OVR", NA, NA, 1, lubridate::ymd("2021-04-07"),
#'   "02", "OVR", 6, "PD", 1, lubridate::ymd("2021-05-07"),
#'   "03", "OVR", 3, "SD", 1, lubridate::ymd("2021-01-30"),
#'   "03", "OVR", 3, "SD", 2, lubridate::ymd("2021-01-30"),
#'   "04", "OVR", NA, "NE", 1, lubridate::ymd("2021-05-21"),
#'   "04", "OVR", 5, "NON-PD", 1, lubridate::ymd("2021-06-30"),
#'   "04", "OVR", NA, "NE", 1, lubridate::ymd("2021-07-24"),
#'   "04", "OVR", NA, "ND", 1, lubridate::ymd("2021-09-30"),
#' ) %>%
#'   dplyr::mutate(STUDYID = "a_study_id")
#'
#' pd <- date_source(
#'   dataset_name = adrs,
#'   date         = ADT,
#'   filter       = PARAMCD == 'PD' & AVALC == 'Y'
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
#' @name   derive_param_lasta
#' @title  derive_param_lasta
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @author Stephen Gormley
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @keywords ADRS
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @importFrom  magrittr `%>%`
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @return The dataframe passed in the `dataset` argument with additional
#'         columns and/or rows as set in the `set_values_to` argument.
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|

derive_param_lasta <- function(dataset,
                               dataset_adsl,
                               filter_source,
                               order           = admiral::vars(STUDYID, USUBJID, ADT),
                               by_vars         = admiral::vars(STUDYID, USUBJID),
                               source_pd       = NULL,
                               source_datasets = NULL,
                               subject_keys    = admiral::vars(STUDYID, USUBJID),
                               set_values_to) {

  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Assert statements (checked in order of signature) ----
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  admiral::assert_data_frame(arg           = dataset,
                             required_vars = admiral:::quo_c(subject_keys,
                                                            order,
                                                            by_vars,
                                                            admiral::vars(PARAMCD, ADT, AVALC)))

  admiral::assert_data_frame(arg           = dataset_adsl,
                             required_vars = admiral:::quo_c(subject_keys))

  filter_source <- admiral::assert_filter_cond(arg      = enquo(filter_source),
                                               optional = TRUE)

  admiral::assert_vars(arg = order)

  admiral::assert_vars(arg = by_vars)

  admiral::assert_vars(arg = subject_keys)

  admiral::assert_varval_list(arg         = set_values_to,
                              required_elements = c("PARAMCD", "PARAM"))

  admiral::assert_param_does_not_exist(dataset = dataset,
                                       param   = rlang::quo_get_expr(set_values_to$PARAMCD))

#   dataset <<- dataset
#   dataset_adsl <<- dataset_adsl
#   filter_source <<- filter_source
#   order_db  <<- order
#   order <- order_db
#   by_vars <<- by_vars
#   source_pd <<- source_pd
#   source_datasets <<- source_datasets
#   subject_keys    <<- subject_keys
#   set_values_to <<- set_values_to
# stop("yep derive me")
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
        admiral::enumerate(source_names, quote_fun = sQuote))
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
      dplyr::filter(!!enquo(filter_source))

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
  # filter_extreme: Filter last assessment using filter_extreme ----
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  param_lasta_01 <- dataset_filter %>%
      admiral::filter_extreme(mode       = "last",
                              order      = admiral::vars(!!!order),
                              by_vars    = admiral::vars(!!!by_vars),
                              check_type = "warning")

  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # adsl: Append subjects in ADSL not in dataset_filter ----
  #
  # Note Requirement: For subjects without observations in the input dataset
  # after the filter is applied, we keep all variables from ADSL which
  # are also in the input dataset.
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  adsl_cols_to_keep  <- names(dataset_adsl)[names(dataset_adsl) %in% names(dataset)]

  # create list of unique subject_keys with at least one record
  subjects_with_data <-  dataset_filter %>%
    dplyr::distinct(!!!subject_keys)

  # All records in dataset_adsl that do not have a match in subjects_with_data are
  # removed with anti_join.
  adsl_subjects_only <- dataset_adsl %>%
    dplyr::anti_join(subjects_with_data) %>%
    dplyr::select(adsl_cols_to_keep)

  # bind back with dataframe with data
  param_lasta_02 <- param_lasta_01 %>%
    dplyr::bind_rows(adsl_subjects_only)

  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # set_values_to: Execute set_values_to ----
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  tryCatch(

    param_lasta_values_set <- param_lasta_02 %>%
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
