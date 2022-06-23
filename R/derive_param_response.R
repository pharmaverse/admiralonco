#' Add a Parameter Indicating If a Subject Had a Response before Progressive
#' Disease
#'
#' Add a new parameter indicating if a response has been observed.
#' If a response has been observed, `AVALC` is set to "Y", `AVAL` to 1 and `ADT`
#'  is set to the
#' first date when a response has been observed.
#' If a response has not been observed, `AVALC` is set to "N", AVAL to 0 and
#' `ADT` is set NA.
#'
#' @param dataset Input dataset
#'
#'   The variables specified by the `subject_keys`and `ADT` are expected.
#'
#' @param dataset_adsl Input dataset
#'
#'   + The variables specified for `subject_keys` are expected.
#'   + For each observation of the specified dataset a new observation is added
#'   to the input dataset. This is to capture those patients that may never have
#'   had a tumor assessment.
#'
#' @param source_datasets Source dataset(s)
#'
#'   A named list of datasets with 1 or 2 elements is expected (e.g. `list(adrs
#'   = adrs, pdds=pdds)`).
#'
#'   At least `dataset` must be defined as a named list.
#'
#'   + If 1 element is defined, it implies that the response data and the data
#'   used in `pd_source` are in the same dataset `dataset`.
#'
#'   + If 2 elements are defined, then 1 dataset contains the response data
#'   (`dataset`) and the other the data used in `pd_source` (e.g.
#'   `pd_source$dataset_name`)
#'
#'   The variables specified by the `subject_keys` and `ADT` are expected in
#'   the input `dataset`.
#'
#' @param filter_source Source filter
#'
#'   All observations in the `dataset` data fulfilling the specified condition
#'   are selected.
#'
#' @param source_pd Sources and conditions defining the end of the assessment
#' period for the responses.
#'
#'   An object of type `date_source` is expected
#'
#'   All observations in `dataset` defining the response data fulfilling the
#'   `filter_source` condition are considered as response if they fall before
#'   the end of the assessment period as defined by `source_pd`.
#'
#'   + For subjects with at least one response before the end of the assessment
#'   period, `AVALC` is set to `"Y"`, `AVAL` to `1`, and `ADT` to the first
#'   date when the response occurred.
#'
#'   + For all other subjects `AVALC` is set to `"N"`, `AVAL` to `0`, and
#'    `ADT` to `NA`.
#'
#' @param set_values_to Variables to set
#'
#'   A named list returned by `vars()` defining the variables to be set for the
#'   new parameter, e.g. `vars(PARAMCD = "RSP", PARAM = "Response by
#'   investigator")` is expected.
#'
#'   The values must be symbols, character strings, numeric values or `NA`.
#'
#' @param subject_keys Variables to uniquely identify a subject
#'
#'   A list of symbols created using `vars()` is expected.
#'
#' @details
#'
#'   1. The Date of the end of the assessment period (e.g. Progressive disease,
#'    as defined by `pd_source`) is added to the response dataset.
#'
#'   1. The response dataset is restricted to observations occurring before
#'   ** or on ** the date of progressive disease.
#'
#'   1. For each subject (with respect to the variables specified for the
#'   `subject_keys` parameter), the first observation (with respect to
#'   `ADT`) where the response condition (`filter_source` parameter) is
#'   fulfilled is selected.
#'
#'   1. For each observation in `dataset_adsl` a new observation is created.
#'   + For subjects with a response `AVALC` is set to `"Y"`, `AVAL` to `1`,
#'   and `ADT` to  the first date (`ADT`) where the response condition is
#'   fulfilled.
#'   + For all other subjects `AVALC` is set to `"N"`, `AVAL` to `0`
#'   and `ADT` to `NA`.
#'
#'   1. The variables specified by the `set_values_to` parameter are added to
#'   the new observations.
#'
#'   1. The new observations are added to input dataset.
#'
#' @author Samia Kabi
#'
#' @return The input dataset with a new parameter indicating if and when a
#' response occurred
#'
#' @keywords derivation bds adrs
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' library(admiral)
#' library(lubridate)
#' library(rlang)
#'
#' adsl <- tibble::tribble(
#'   ~USUBJID,
#'   "1",
#'   "2",
#'   "3",
#'   "4"
#' ) %>%
#'   mutate(STUDYID = "XX1234")
#'
#' adrs <- tibble::tribble(
#'   ~USUBJID, ~PARAMCD, ~ADTC,         ~AVALC, ~ANL01FL,
#'   "1",      "OVR",    "2020-01-02",  "PR",   "Y",
#'   "1",      "OVR",    "2020-02-01",  "CR",   "Y",
#'   "1",      "OVR",    "2020-03-01",  "CR",   "Y",
#'   "1",      "OVR",    "2020-04-01",  "SD",   "Y",
#'   "1",      "PD",     NA_character_, "N",    "Y",
#'   "2",      "OVR",    "2021-06-15",  "SD",   "Y",
#'   "2",      "OVR",    "2021-07-16",  "PD",   "Y",
#'   "2",      "OVR",    "2021-09-14",  "PD",   "Y",
#'   "2",      "PD",     "2021-09-14",  "Y",    "Y",
#'   "3",      "OVR",    "2021-09-14",  "SD",   "Y",
#'   "3",      "OVR",    "2021-10-30",  "PD",   "Y",
#'   "3",      "OVR",    "2021-12-25",  "CR",   "Y",
#'   "3",      "PD",     "2021-10-30",  "Y",    "Y"
#' ) %>%
#'   mutate(
#'     STUDYID = "XX1234",
#'     ADT = ymd(ADTC),
#'     ANL01FL = "Y"
#'   ) %>%
#'   select(-ADTC)
#'
#' # Define the end of the assessment period for responses:
#' # all responses before or on the first PD will be used.
#' pd <- date_source(
#'   dataset_name = "adrs",
#'   date = ADT,
#'   filter = PARAMCD == "PD" & AVALC == "Y"
#' )
#' # Derive the response parameter
#' derive_param_response(
#'   dataset = adrs,
#'   dataset_adsl = adsl,
#'   filter_source = PARAMCD == "OVR" & AVALC %in% c("CR", "PR") & ANL01FL == "Y",
#'   source_pd = pd,
#'   source_datasets = list(adrs = adrs),
#'   set_values_to = vars(
#'     PARAMCD = "RSP",
#'     PARAM = "Response by investigator"
#'   ),
#'   subject_keys = vars(STUDYID, USUBJID)
#' ) %>%
#'   arrange(USUBJID, PARAMCD, ADT)
derive_param_response <- function(dataset,
                                  dataset_adsl,
                                  filter_source,
                                  source_pd,
                                  source_datasets,
                                  set_values_to,
                                  subject_keys = vars(STUDYID, USUBJID)) {

  # ---- checking and quoting ----
  assert_data_frame(dataset)
  assert_data_frame(dataset_adsl)
  filter_s <- assert_filter_cond(enquo(filter_source), optional = TRUE)
  assert_list_of(source_datasets, "data.frame")
  source_names <- names(source_datasets)

  assert_s3_class(source_pd, "date_source")
  if (!(source_pd$dataset_name %in% source_names)) {
    abort(paste(
      "The dataset names must be included in the list specified for the ",
      "`source_datasets` parameter.\n",
      "Following names were provided by `source_datasets`:\n",
      admiral::enumerate(source_names, quote_fun = admiral::squote)
    ))
  }

  assert_varval_list(set_values_to, accept_expr = TRUE, optional = TRUE)
  if (!is.null(set_values_to$PARAMCD) & !is.null(dataset)) {
    assert_param_does_not_exist(dataset, quo_get_expr(set_values_to$PARAMCD))
  }

  #----  Only records from `dataset` where `filter_source` before PD ----
  resp_before_pd <- dataset %>%
    filter_pd(
      # Need to specify a filter otherwise:
      # ERROR ! Argument `filter_source` is missing, with no default
      filter = !!filter_s,
      source_pd = source_pd,
      source_datasets = source_datasets,
      subject_keys = vars(!!!subject_keys)
    )
  # ---- Select the 1st response and add a new PARAMCD to the input dataset ----
  dataset %>%
    derive_param_first_event(
      dataset_adsl = dataset_adsl,
      dataset_source = resp_before_pd,
      # Need to specify a filter otherwise:
      # ERROR ! Argument `filter_source` is missing, with no default
      filter_source = !!filter_s,
      date_var = ADT,
      set_values_to = set_values_to
    )
}
