#' Derive Confirmed Best Overall Response Parameter
#'
#' Derive confirmed best overall response (BOR) parameter
#'
#' @author Stefan Bundfuss
#'
#' @export
#'
derive_param_confirmed_bor <- function(
  dataset,
  dataset_adsl,
  filter_source,
  source_pd = NULL,
  source_datasets,
  reference_date,
  ref_start_window,
  ref_confirm,
  max_nr_ne = 1,
  accept_sd = FALSE,
  set_values_to,
  subject_keys = vars(STUDYID, USUBJID)
) {
  # Check input parameters
  filter_source <- assert_filter_cond(enquo(filter_source))
  assert_s3_class(source_pd, "date_source", optional = TRUE)
  assert_list_of(source_datasets, "data.frame")
  reference_date <- assert_symbol(enquo(reference_date))
  assert_integer_scalar(ref_start_window, subset = "non-negative")
  assert_integer_scalar(ref_confirm, subset = "non-negative")
  assert_integer_scalar(max_nr_ne, subset = "non-negative")
  assert_logical_scalar(accept_sd)
  assert_varval_list(set_values_to, required_elements = "PARAMCD")
  assert_vars(subject_keys)
  assert_data_frame(dataset, required_vars = quo_c(subject_keys, vars(PARAMCD, ADT, AVALC)))
  assert_data_frame(dataset_adsl, required_vars = quo_c(subject_keys, reference_date))
  if (!is.null(dataset)) {
    assert_param_does_not_exist(dataset, quo_get_expr(set_values_to$PARAMCD))
  }

}
