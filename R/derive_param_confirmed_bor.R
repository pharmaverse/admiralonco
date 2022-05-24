#' Derive Confirmed Best Overall Response Parameter
#'
#' Derive confirmed best overall response (BOR) parameter
#'
#' @author Stefan Bundfuss
#'
#' @export
#'
derive_param_confirmed_bor <- function(dataset,
                                       dataset_adsl,
                                       filter_source,
                                       source_pd = NULL,
                                       source_datasets,
                                       reference_date,
                                       ref_start_window,
                                       ref_confirm,
                                       max_nr_ne = 1,
                                       accept_sd = FALSE,
                                       missing_as_ne = FALSE,
                                       aval_fun = aval_resp,
                                       set_values_to,
                                       subject_keys = vars(STUDYID, USUBJID)) {
  # Check input parameters
  filter_source <- assert_filter_cond(enquo(filter_source))
  assert_s3_class(source_pd, "date_source", optional = TRUE)
  assert_list_of(source_datasets, "data.frame")
  reference_date <- assert_symbol(enquo(reference_date))
  assert_integer_scalar(ref_start_window, subset = "non-negative")
  assert_integer_scalar(ref_confirm, subset = "non-negative")
  assert_integer_scalar(max_nr_ne, subset = "non-negative")
  assert_logical_scalar(accept_sd)
  assert_logical_scalar(missing_as_ne)
  assert_function(aval_fun)
  assert_varval_list(set_values_to, required_elements = "PARAMCD")
  assert_vars(subject_keys)
  assert_data_frame(dataset,
    required_vars = admiral:::quo_c(subject_keys, reference_date, vars(PARAMCD, ADT, AVALC))
  )
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
      count_vals(var = AVALC.join, val = "NE") <= max_nr_ne
  ) %>%
    mutate(tmp_order = 1) %>%
    select(!!!subject_keys, AVALC, tmp_order, ADT)

  if (accept_sd) {
    max_nr_sd <- 1
  } else {
    max_nr_sd <- 0
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
      count_vals(var = AVALC.join, val = "NE") <= max_nr_ne &
      count_vals(var = AVALC.join, val = "SD") <= max_nr_sd &
      (
        min_cond(var = ADT.join, cond = AVALC.join == "CR") > max_cond(var = ADT.join, cond = AVALC.join == "PR") |
          count_vals(var = AVALC.join, val = "CR") == 0
      )
  ) %>%
    mutate(tmp_order = 2) %>%
    select(!!!subject_keys, AVALC, tmp_order, ADT)

  sd_data <- filter(
    source_data,
    AVALC %in% c("CR", "PR", "SD") & ADT >= !!reference_date + days(ref_start_window)
  ) %>%
    mutate(
      AVALC = "SD",
      tmp_order = 3
    ) %>%
    select(!!!subject_keys, AVALC, tmp_order, ADT)

  non_data <- filter(
    source_data,
    AVALC == "NON-CR/NON-PD" & ADT >= !!reference_date + days(ref_start_window)
  ) %>%
    mutate(
      tmp_order = 4
    ) %>%
    select(!!!subject_keys, AVALC, tmp_order, ADT)

  pd_data <- filter(source_data, AVALC == "PD") %>%
    mutate(tmp_order = 5) %>%
    select(!!!subject_keys, AVALC, tmp_order, ADT)

  ne_data <- filter(
    source_data,
    AVALC == "NE" | AVALC %in% c("CR", "PR", "SD", "NON-CR/NON-PD") & ADT < !!reference_date + days(ref_start_window)
  ) %>%
    mutate(
      AVALC = "NE",
      tmp_order = 6
    ) %>%
    select(!!!subject_keys, AVALC, tmp_order, ADT)

  nd_data <- filter(
    source_data,
    AVALC == "ND"
  ) %>%
    mutate(
      AVALC = "ND",
      tmp_order = 7
    ) %>%
    select(!!!subject_keys, AVALC, tmp_order, ADT)

  if (missing_as_ne) {
    missing_val <- "NE"
  } else {
    missing_val <- "MISSING"
  }

  missing_data <- dataset_adsl %>%
    select(!!!subject_keys) %>%
    mutate(
      AVALC = missing_val,
      tmp_order = 8
    ) %>%
    select(!!!subject_keys, AVALC, tmp_order)

  # Select best response
  bor <- bind_rows(cr_data, pr_data, sd_data, pd_data, non_data, ne_data, nd_data, missing_data) %>%
    filter_extreme(
      by_vars = subject_keys,
      order = vars(tmp_order, ADT),
      mode = "first"
    ) %>%
    select(-tmp_order) %>%
    mutate(
      AVAL = aval_fun(AVALC),
      !!!set_values_to
    )

  # Add to input dataset
  bind_rows(dataset, bor)
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
#' @return `1` if `arg` equals `"Y"`, `0` if `arg` equals `"N"`, `NA_real_` otherwise
#'
#' @examples
#'
#' aval_resp(c("Y", "N", NA_character_))
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
