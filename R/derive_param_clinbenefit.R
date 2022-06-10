#' Derive a Clinical Benefit Parameter
#'
#' Add a clinical benefit/disease control parameter to the input dataset.
#'
#' @details
#' Clinical benefit/disease control is derived for subjects that have at least
#' one evaluable non-PD response assessment prior to first PD (Progressive Disease)
#' (i.e., inclusive of `CR`, `PR`, `SD` and exclusive of `NA`, `NE`, `ND`, and `PD`)
#' and after a specified amount of time from a reference date (`ref_start_window`).
#'
#' \enumerate{
#'   \item The input dataset (`dataset`) is restricted to the observations matching
#'   `filter_source` and to observations before or at the date specified by
#'   `source_pd`.
#'
#'   \item Variables `AVALC`, `AVAL`, and `ADT` will be populated for each unique subject
#'   in the input `dataset_adsl` as identified by `subject_keys`.
#'
#'   \item `AVALC` is set to `Y` for those subjects in the `dataset` meeting the criteria
#'   for clinical benefit above, and `N` for subjects either not meeting the criteria
#'   in `dataset` or present in `dataset_adsl` but not present in `dataset`.
#'
#'   \item `AVAL` is derived using `AVALC` as input to the function specified in
#'   `aval_fun`.
#'
#'   \item `ADT` is set to the earliest possible date representing an evaluable
#'   non-PD response assessment prior to first PD.
#'
#'   \item The variables specified by `set_values_to` are added to the new observations
#'   with values equal to the values specified in the same.
#'
#'   \item The new observations are added to `dataset`.
#'  }
#'
#' @param dataset Input dataset. This is the dataset to which the clinical
#' benefit rate parameter will be added.
#'
#'   The variables `PARAMCD`, `AVALC`, `ADT`, and those specified by the `subject_keys`
#'   parameter are expected.
#'
#' @param dataset_adsl ADSL input dataset.
#'
#'   The variables specified for `subject_keys` parameter and the `reference_date`
#'   parameter are expected. For each subject of the specified dataset a new
#'   observation is added to the input dataset.
#'
#' @param filter_source Filter condition in `dataset` that represents records
#' for overall disease response assessment for a subject at a given timepoint,
#' e.g. `PARAMCD == "OVR"` or `PARAMCD == "OVRLRESP"`.
#'
#' @param source_resp A `date_source` object specifying the dataset, date variable,
#' and filter condition used to identify response status.
#'
#' @param source_pd A `date_source` object specifying the dataset, date variable,
#' and filter condition used to identify disease progression.
#'
#' @param source_datasets  A named list of data sets is expected.
#'
#'  The list must contain the names provided by the `dataset_name` field of the
#'  `date_source()` objects specified for `source_pd` and `source_resp`.
#'
#' @param reference_date Name of variable representing the index date for
#' `ref_start_window`. A variable providing a date. An unquoted symbol is expected.
#'
#' @param ref_start_window Integer representing number of days from `reference_date`
#' that must elapse before an evaluable non-PD assessment counts toward determining
#' clinical benefit.
#'
#' @param aval_fun Function to map character analysis value (`AVALC`) to numeric
#' analysis value (`AVAL`)
#'
#' The (first) argument of the function must expect a character vector and the
#' function must return a numeric vector.
#'
#' *Default:* `yn_to_numeric` (see `admiral::yn_to_numeric()` for details)
#'
#' @param set_values_to A named list returned by `vars()` containing new variables
#' and their static value to be populated for the clinical benefit rate parameter
#' records, e.g. `vars(PARAMCD = "CBR", PARAM = "Clinical Benefit Rate")`.
#'
#' @param subject_keys A named list returned by `vars()` containing variables
#' used to uniquely identify subjects.
#'
#' @return The input dataset with a new parameter for clinical benefit
#'
#' @keywords derivation adrs
#'
#' @export
#'
#' @author Andrew Smith
#' @examples
#' library(lubridate)
#' library(dplyr)
#' library(admiral)
#'
#' adsl <- tibble::tribble(
#'   ~USUBJID, ~TRTSDT, ~EOSDT,
#'   "01", ymd("2020-01-14"), ymd("2020-05-06"),
#'   "02", ymd("2021-02-16"), ymd("2021-08-03"),
#'   "03", ymd("2021-03-09"), ymd("2021-04-24"),
#'   "04", ymd("2021-04-21"), ymd("2021-09-15")
#' ) %>%
#'   mutate(STUDYID = "AB42")
#'
#' adrs <- tibble::tribble(
#'   ~USUBJID, ~PARAMCD, ~AVALC, ~ADT,
#'   "01", "RSP", "Y", ymd("2021-03-14"),
#'   "02", "RSP", "N", ymd("2021-05-07"),
#'   "03", "RSP", "N", NA,
#'   "04", "RSP", "N", NA,
#'   "01", "PD", "N", NA,
#'   "02", "PD", "Y", ymd("2021-05-07"),
#'   "03", "PD", "N", NA,
#'   "04", "PD", "N", NA,
#'   "01", "OVR", "SD", ymd("2020-03-14"),
#'   "01", "OVR", "PR", ymd("2021-04-13"),
#'   "02", "OVR", "PR", ymd("2021-04-08"),
#'   "02", "OVR", "PD", ymd("2021-05-07"),
#'   "02", "OVR", "CR", ymd("2021-06-20"),
#'   "03", "OVR", "SD", ymd("2021-03-30"),
#'   "04", "OVR", "NE", ymd("2021-05-21"),
#'   "04", "OVR", "NA", ymd("2021-06-30"),
#'   "04", "OVR", "NE", ymd("2021-07-24"),
#'   "04", "OVR", "ND", ymd("2021-09-04"),
#' ) %>%
#'   mutate(STUDYID = "AB42", ANL01FL = "Y")
#'
#' pd <- date_source(
#'   dataset_name = "adrs",
#'   date = ADT,
#'   filter = PARAMCD == "PD" & AVALC == "Y" & ANL01FL == "Y"
#' )
#'
#' resp <- date_source(
#'   dataset_name = "adrs",
#'   date = ADT,
#'   filter = PARAMCD == "RSP" & AVALC == "Y" & ANL01FL == "Y"
#' )
#'
#' derive_param_clinbenefit(
#'   dataset = adrs,
#'   dataset_adsl = adsl,
#'   filter_source = PARAMCD == "OVR",
#'   source_resp = resp,
#'   source_pd = pd,
#'   source_datasets = list(adrs = adrs),
#'   reference_date = TRTSDT,
#'   ref_start_window = 28,
#'   set_values_to = vars(
#'     PARAMCD = "CBR"
#'   )
#' )
derive_param_clinbenefit <- function(dataset,
                                     dataset_adsl,
                                     filter_source,
                                     source_resp,
                                     source_pd,
                                     source_datasets,
                                     reference_date,
                                     ref_start_window,
                                     aval_fun = yn_to_numeric,
                                     set_values_to,
                                     subject_keys = vars(STUDYID, USUBJID)) {

  # Assertions and quotes
  reference_date <- assert_symbol(enquo(reference_date))
  assert_vars(subject_keys)
  assert_data_frame(
    dataset_adsl,
    required_vars = vars(!!!subject_keys, !!reference_date)
  )
  assert_data_frame(
    dataset,
    required_vars = vars(!!!subject_keys, PARAMCD, AVALC, ADT)
  )

  filter_source <- assert_filter_cond(
    enquo(filter_source),
    optional = FALSE
  )
  assert_function(aval_fun)
  assert_s3_class(source_resp, "date_source")
  assert_s3_class(source_pd, "date_source")
  assert_list_of(source_datasets, "data.frame")

  source_names <- names(source_datasets)
  if (!all(c(source_pd$dataset_name, source_resp$dataset_name) %in% source_names)) {
    abort(
      paste0(
        "The dataset names specified for `source_pd` and `source_resp` must be \n",
        "included in the list specified for the `source_datasets` parameter.\n",
        "Following names were provided by `source_datasets`:\n",
        enumerate(source_names, quote_fun = squote)
      )
    )
  }

  ref_start_window <- assert_integer_scalar(ref_start_window)
  assert_varval_list(set_values_to, optional = TRUE)
  assert_param_does_not_exist(dataset, quo_get_expr(set_values_to$PARAMCD))

  # ADSL variables

  adsl_vars <- vars(
    !!!subject_keys,
    !!reference_date
  )

  adsl <- dataset_adsl %>%
    select(!!!adsl_vars)

  # Get response date

  rsp_data <- source_datasets[[source_resp$dataset_name]] %>%
    filter_if(source_resp$filter) %>%
    select(!!!subject_keys, !!source_resp$date) %>%
    rename(temp_rs = !!source_resp$date)

  # Look for valid non-PD measurements after window from reference date
  ovr_data <- filter_pd(
    dataset = dataset,
    filter = !!filter_source,
    source_pd = source_pd,
    source_datasets = source_datasets,
    subject_keys = vars(!!!subject_keys)
  )

  ovr_data <- ovr_data %>%
    left_join(
      adsl,
      by = vars2chr(subject_keys)
    ) %>%
    filter(
      !(AVALC %in% c("NA", "NE", "ND", "PD")) & !is.na(AVALC) &
        ADT >= !!reference_date + days(ref_start_window)
    ) %>%
    # Use this approach only for patients that are not already responders
    anti_join(
      .,
      rsp_data,
      by = vars2chr(subject_keys)
    ) %>%
    filter_extreme(
      order = vars(ADT),
      by_vars = vars(!!!subject_keys),
      mode = "first",
      check_type = "none"
    ) %>%
    select(!!!subject_keys, ADT)

  rsp_data <- rsp_data %>%
    rename(ADT = temp_rs)

  new_param <- bind_rows(ovr_data, rsp_data) %>%
    right_join(adsl, by = vars2chr(subject_keys)) %>%
    mutate(AVALC = if_else(!is.na(ADT), "Y", "N")) %>%
    select(-!!reference_date) %>%
    mutate(
      AVAL = aval_fun(AVALC),
      !!!set_values_to
    )


  bind_rows(dataset, new_param)
}
