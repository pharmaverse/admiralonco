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
  assert_data_frame(dataset,
                    required_vars = admiral:::quo_c(subject_keys, reference_date, vars(PARAMCD, ADT, AVALC)))
  assert_data_frame(dataset_adsl, required_vars = subject_keys)
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
    join_vars = vars(AVALC, ADT),
    order = vars(ADT),
    first_cond = AVALC.join == "CR" &
      ADT.join >= ADT + days(ref_confirm),
    filter = AVALC == "CR" &
      all(AVALC.join %in% c("CR", "NE")) &
      count_vals(var = AVALC.join, val = "NE") < max_nr_ne
  ) %>%
    mutate(AVAL = 1)

  if (accept_sd) {
    max_nr_sd = 1
  } else {
    max_nr_sd = 0
  }
  pr_data <- filter_confirmation(
    source_data,
    by_vars = subject_keys,
    join_vars = vars(AVALC, ADT),
    order = vars(ADT),
    first_cond = AVALC.join %in% c("CR", "PR") &
      ADT.join >= ADT + days(ref_confirm),
    filter = AVALC == "PR" &
      all(AVALC.join %in% c("CR", "PR", "SD", "NE")) &
      count_vals(var = AVALC.join, val = "NE") < max_nr_ne &
      count_vals(var = AVALC.join, val = "SD") < max_nr_sd &
      (
        min_cond(var = ADT.join, cond = AVALC.join == "CR") > max_cond(var = ADT.join, cond = AVALC.join == "PR") |
          count_vals(var = AVALC.join, val = "CR") == 0
      )
  ) %>%
    mutate(AVAL = 2)

  sd_data <- filter(
    source_data,
    AVALC == "SD" & ADT >= !!reference_date + days(ref_start_window)
  ) %>%
    mutate(
      AVAL = 3
    )
  non_data <- filter(
    source_data,
    AVALC == "NON-CR/NON-PD" & ADT >= !!reference_date + days(ref_start_window)
  ) %>%
    mutate(
      AVAL = 4
    )

  pd_data <- filter(source_data, AVALC == "PD") %>%
    mutate(AVAL = 5)

  ne_data <- filter(
    source_data,
    AVALC == "NE" | AVALC %in% c("CR", "PR", "SD", "NON-CR/NON-PD") & ADT < !!reference_date + days(ref_start_window)
  ) %>%
    mutate(AVALC = "NE",
           AVAL = 6)

  missing_data <- dataset_adsl %>%
    select(!!!subject_keys) %>%
    mutate(
      AVALC = "MISSING",
      AVAL = 7)

  # Select best response
  bind_rows(cr_data, pr_data, sd_data, pd_data, non_data, ne_data, missing_data) %>%
    filter_extreme(
      by_vars = subject_keys,
      order = vars(AVAL, ADT),
      mode = "first"
    ) %>%
    mutate(!!!set_values_to)
}

min_cond <- function(var, cond) {
  assert_filter_cond(enquo(cond))
  if (length(var[cond]) == 0) {
    NA
  } else {
    min(var[cond])
  }
}

max_cond <- function(var, cond) {
  assert_filter_cond(enquo(cond))
  if (length(var[cond]) == 0) {
    NA
  } else {
    min(var[cond])
  }
}
