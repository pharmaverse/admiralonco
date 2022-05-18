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
  assert_data_frame(dataset, required_vars = admiral:::quo_c(subject_keys, vars(PARAMCD, ADT, AVALC)))
  assert_data_frame(dataset_adsl, required_vars = admiral:::quo_c(subject_keys, reference_date))
  if (!is.null(dataset)) {
    assert_param_does_not_exist(dataset, quo_get_expr(set_values_to$PARAMCD))
  }

  # Restrict input dataset
  source_data <- dataset %>%
    filter(!!filter_source)
    # filter_pd(source_pd = source_pd,
    #    source_datasets = source_datasets)

  # Create observations for potential responses
  cr_data <- filter_confirmation(
    source_data,
    by_vars = subject_keys,
    join_vars = vars(AVALC),
    order = vars(ADT),
    first_cond = AVALC.join == "CR",
    filter = AVALC == "CR" & all(AVALC.join %in% c("CR", "NE")) & count_vals(var = AVALC.join, val = "NE") < max_nr_ne
  ) %>%
    mutate(AVAL = 1)

  missing_data <- dataset_adsl %>%
    select(subject_keys) %>%
    mutate(
      AVALC = "MISSING",
      AVAL = 7)

  # Select best response
  bind_rows(cr_data, missing_data) %>%
    filter_extreme(
      by_vars = subject_keys,
      order = vars(AVAL, ADT),
      mode = "first"
    ) %>%
    mutate(!!!set_values_to)
}
