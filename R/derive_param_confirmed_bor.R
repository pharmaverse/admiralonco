#' Adds a Parameter for Confirmed Best Overall Response
#'
#' Adds a parameter for confirmed best overall response (BOR)
#'
#' @param dataset Input dataset
#'
#'   The `PARAMCD`, `ADT`, and `AVALC` variables and the variables specified by
#'   `subject_keys` and `reference_date` are expected.
#'
#'   After applying `filter_source` and `source_pd` the variable `ADT` and the
#'   variables specified by `subject_keys` must be a unique key of the dataset.
#'
#' @param dataset_adsl ADSL input dataset
#'
#'   The variables specified for `subject_keys` are expected. For each subject
#'   of the specified dataset a new observation is added to the input dataset.
#'
#' @param filter_source Source filter
#'
#'   All observations in `dataset_source` fulfilling the specified condition are
#'   considered for deriving the confirmed best overall response.
#'
#' @param source_pd Date of first progressive disease (PD)
#'
#'   If the parameter is specified, the observations of the input dataset for
#'   deriving the new parameter are restricted to observations up to the
#'   specified date. Observations at the specified date are included. For
#'   subjects without first PD date all observations are take into account.
#'
#'   *Permitted Values:* a `date_source` object (see `admiral::date_source()`
#'   for details)
#'
#' @param source_datasets Source dataset for the first PD date
#'
#'   A named list of datasets is expected. It links the `dataset_name` from
#'   `source_pd` with an existing dataset.
#'
#'   For example if `source_pd = pd_date` with
#'   ```{r, eval=FALSE}
#'   pd_date <- date_source(
#'     dataset_name = "adrs",
#'     date = ADT,
#'     filter = PARAMCD == PD
#'   )
#'   ```
#'   and the actual response dataset in the script is `myadrs`, `source_datasets
#'   = list(adrs = myadrs)` should be specified.
#'
#' @param reference_date Reference date
#'
#'   The reference date is used for the derivation of `"SD"` and
#'   `"NON-CR/NON-PD"` response (see "Details" section). Usually it is treatment
#'   start date (`TRTSDT`) or randomization date (`RANDDT`).
#'
#'   *Permitted Values:* a numeric date variable
#'
#' @param ref_start_window Stable disease time window
#'
#'   Assessments at least the specified number of days after the reference date
#'   (i.e. where `ADT` >= `reference_date` + `ref_start_window`)
#'   with response `"CR"`, `"PR"`, `"SD"`, or `"NON-CR/NON-PD"` are considered
#'   for `"SD"` or `"NON-CR/NON-PD"` response.
#'
#'   *Permitted Values:* a non-negative numeric scalar
#'
#' @param ref_confirm Minimum time period for confirmation
#'
#'   The assessment and the confirmatory assessment for `"CR"` and `"PR"` have
#'   to be at least the specified number of days apart.
#'
#' @param max_nr_ne
#'
#'   The specified number of `"NE"` assessments between the assessment and the
#'   confirmatory assessment for `"CR"` and `"PR"` response is accepted.
#'
#'   *Permitted Values:* a non-negative numeric scalar
#'
#' @param accept_sd Accept `"SD"` for `"PR"`?
#'
#'   If the argument is set to `TRUE`, one `"SD"` assessment between the
#'   assessment and the confirmatory assessment for `"PR"` response is accepted.
#'   Otherwise, no `"SD"` assessment must occur between the two assessments.
#'
#'   *Permitted Values:* a logical scalar
#'
#' @param missing_as_ne Consider no assessments as `"NE"`?
#'
#'   If the argument is set to `TRUE`, the response is set to `"NE"` for
#'   subjects without an assessment in the input dataset. Otherwise, the
#'   response is set to `"MISSING"` for these subjects.
#'
#'   *Permitted Values:* a logical scalar
#'
#' @param aval_fun Function to map character analysis value (`AVALC`) to numeric
#'   analysis value (`AVAL`)
#'
#'   The (first) argument of the function must expect a character vector and the
#'   function must return a numeric vector.
#'
#' @param set_values_to Variables to set
#'
#'   A named list returned by `vars()` defining the variables to be set for the
#'   new parameter, e.g. `vars(PARAMCD = "CBOR", PARAM = "Confirmed Best Overall
#'   Response")` is expected. The values must be symbols, character strings,
#'   numeric values, or `NA`.
#'
#' @param subject_keys Variables to uniquely identify a subject
#'
#'   A list of symbols created using `vars()` is expected.
#'
#' @details
#'
#'   1. The input dataset (`dataset`) is restricted to the observations matching
#'   `filter_source` and to observations before or at the date specified by
#'   `source_pd`.
#'
#'   1. The following potential confirmed responses are selected from the
#'   restricted input dataset:
#'
#'       - `"CR"`: An assessment is considered as complete response (CR) if
#'           - `AVALC == "CR"`,
#'           - there is a confirmatory assessment with `AVALC == "CR"` at least
#'           `ref_confirm` days after the assessment,
#'           - all assessments between the assessment and the confirmatory
#'           assessment are `"CR"` or `"NE"`, and
#'           - there are at most `max_nr_ne` `"NE"` assessments between the
#'           assessment and the confirmatory assessment.
#'
#'       - `"PR"`: An assessment is considered as partial response (PR) if
#'           - `AVALC == "PR"`,
#'           - there is a confirmatory assessment with `AVALC %in% c("CR",
#'           "PR")` at least `ref_confirm` days after the assessment,
#'           - all assessments between the assessment and the confirmatory
#'           assessment are `"CR"`, `"PR"`, `"SD"`, or `"NE"`,
#'           - there is no `"PR"` assessment after a `"CR"` assessment in the
#'           confirmation period,
#'           - there are at most `max_nr_ne` `"NE"` assessments between the
#'           assessment and the confirmatory assessment, and
#'           - if the `accept_sd` argument is set to `TRUE`, one `"SD"`
#'           assessment in the confirmation period is accepted. Otherwise, no
#'           `"SD"` assessment must occur within the confirmation period.
#'
#'        - `"SD"`: An assessment is considered as stable disease (SD) if
#'          - `AVALC %in% c("CR", "PR", "SD")` and
#'          - the assessment is at least `ref_start_window` days after
#'          `reference_date`.
#'
#'        - `"NON-CR/NON-PD"`: An assessment is considered as NON-CR/NON-PD if
#'          - `AVALC = "NON-CR/NON-PD"` and
#'          - the assessment is at least `ref_start_window` days after
#'          `reference_date`.
#'
#'        - `"PD"`: An assessment is considered as progressive disease (PD) if
#'        `AVALC == "PD"`.
#'
#'        - `"NE"`: An assessment is considered as not estimable (NE) if
#'            - `AVALC == "NE"` or
#'            - `AVALC %in% c("CR", "PR", "SD", "NON-CR/NON-PD")` and the
#'            assessment is less than `ref_start_window` days after
#'            `reference_date`.
#'
#'        - `"ND"`: An assessment is considered as not done (ND) if `AVALC ==
#'        "ND"`.
#'
#'        - `"MISSING"`: An assessment is considered as missing (MISSING) if a
#'        subject has no observation in the input dataset.
#'
#'            If the `missing_as_ne` argument is set to `TRUE`, `AVALC` is set to
#'            `"NE"` for these subjects.
#'
#'   1. For each subject the best response as derived in the previous step is
#'   selected, where `"CR"` is best and `"MISSING"` is worst in the order above.
#'   If the best response is not unique, the first one (with respect to `ADT`)
#'   is selected. If the selected record is from the input dataset, all
#'   variables are kept. If the selected record is from `dataset_adsl`, all
#'   variables which are in both `dataset` and `dataset_adsl` are kept.
#'
#'   1. The `AVAL` variable is added and set to `aval_fun(AVALC)`.
#'
#'   1. The variables specified by the `set_values_to` parameter are added to
#'   the new observations.
#'
#'   1. The new observations are added to input dataset.
#'
#' @return The input dataset with a new parameter for confirmed best overall
#'   response
#'
#' @family der_prm_adrs
#'
#' @keywords der_prm_adrs
#'
#' @author Stefan Bundfuss
#'
#' @export
#'
#' @examples
#'
#' library(dplyr)
#' library(lubridate)
#' library(admiral)
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
#'   "8",      "2020-04-01",
#'   "9",      "2020-03-01"
#' ) %>%
#'   mutate(
#'     TRTSDT = ymd(TRTSDTC),
#'     STUDYID = "XX1234"
#'   )
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
#'   "7",      "2020-04-01", "NE",
#'   "9",      "2020-03-16", "CR",
#'   "9",      "2020-04-01", "NE",
#'   "9",      "2020-04-16", "NE",
#'   "9",      "2020-05-01", "CR"
#' ) %>%
#'   mutate(PARAMCD = "OVR", ANL01FL = "Y")
#'
#' pd_obs <-
#'   bind_rows(tibble::tribble(
#'     ~USUBJID, ~ADTC,        ~AVALC,
#'     "6",      "2020-04-12", "Y"
#'   ) %>%
#'     mutate(PARAMCD = "PD", ANL01FL = "Y"))
#'
#' adrs <- bind_rows(ovr_obs, pd_obs) %>%
#'   mutate(
#'     ADT = ymd(ADTC),
#'     STUDYID = "XX1234"
#'   ) %>%
#'   select(-ADTC) %>%
#'   derive_vars_merged(
#'     dataset_add = adsl,
#'     by_vars = vars(STUDYID, USUBJID),
#'     new_vars = vars(TRTSDT)
#'   )
#'
#' pd_date <- date_source(
#'   dataset_name = "adrs",
#'   date = ADT,
#'   filter = PARAMCD == "PD" & ANL01FL == "Y"
#' )
#'
#' # Derive confirmed best overall response parameter
#' derive_param_confirmed_bor(
#'   adrs,
#'   dataset_adsl = adsl,
#'   filter_source = PARAMCD == "OVR" & ANL01FL == "Y",
#'   source_pd = pd_date,
#'   source_datasets = list(adrs = adrs),
#'   reference_date = TRTSDT,
#'   ref_start_window = 28,
#'   ref_confirm = 28,
#'   set_values_to = vars(
#'     PARAMCD = "CBOR",
#'     PARAM = "Best Confirmed Overall Response by Investigator"
#'   )
#' ) %>%
#'   filter(PARAMCD == "CBOR")
#'
#' # Derive confirmed best overall response parameter (accepting SD for PR,
#' # accept two NEs, and considering missings as NE)
#' derive_param_confirmed_bor(
#'   adrs,
#'   dataset_adsl = adsl,
#'   filter_source = PARAMCD == "OVR" & ANL01FL == "Y",
#'   source_pd = pd_date,
#'   source_datasets = list(adrs = adrs),
#'   reference_date = TRTSDT,
#'   ref_start_window = 28,
#'   ref_confirm = 28,
#'   max_nr_ne = 2,
#'   accept_sd = TRUE,
#'   missing_as_ne = TRUE,
#'   set_values_to = vars(
#'     PARAMCD = "CBOR",
#'     PARAM = "Best Confirmed Overall Response by Investigator"
#'   )
#' ) %>%
#'   filter(PARAMCD == "CBOR")
derive_param_confirmed_bor <- function(dataset,
                                       dataset_adsl,
                                       filter_source,
                                       source_pd = NULL,
                                       source_datasets = NULL,
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
    required_vars = quo_c(subject_keys, reference_date, vars(PARAMCD, ADT, AVALC))
  )
  assert_data_frame(dataset_adsl, required_vars = subject_keys)
  assert_param_does_not_exist(dataset, quo_get_expr(set_values_to$PARAMCD))

  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # filter_pd and filter_source: Filter source dataset using filter_source----
  # argument and also filter data after progressive disease with filter_pd
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  if (!is.null(source_pd)) {
    # Restrict input dataset
    source_data <- dataset %>%
      filter_pd(
        filter = !!filter_source,
        source_pd = source_pd,
        source_datasets = source_datasets,
        subject_keys = subject_keys
      )
  } else {

    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # filter_source: Filter using filter_source argument ----
    # This would also be used to filter out records from dataset that are greater
    # than e.g. ADSL.TRTSDT
    # Not filtering data after progressive disease with filter_pd
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    source_data <- dataset %>%
      filter(!!enquo(filter_source))
  }

  # Check for invalid AVALC values
  resp_vals <- source_data$AVALC
  valid_vals <- c("CR", "PR", "SD", "NON-CR/NON-PD", "PD", "NE", "ND")
  invalid_vals <- unique(resp_vals[!resp_vals %in% valid_vals])
  if (length(invalid_vals) > 0) {
    abort(
      paste0(
        "The function is considering only the following response values:\n",
        enumerate(valid_vals),
        "\nThe following invalid values were found:\n",
        enumerate(invalid_vals)
      )
    )
  }

  # Check for CR followed by PR (this should not occur in clean data)
  signal_crpr(
    source_data,
    order = vars(ADT),
    subject_keys = subject_keys
  )

  # Create observations for potential responses
  cr_data <- filter_confirmation(
    source_data,
    by_vars = subject_keys,
    join_vars = vars(AVALC, ADT),
    join_type = "after",
    order = vars(ADT),
    first_cond = AVALC.join == "CR" &
      ADT.join >= ADT + days(ref_confirm),
    filter = AVALC == "CR" &
      all(AVALC.join %in% c("CR", "NE")) &
      count_vals(var = AVALC.join, val = "NE") <= max_nr_ne
  ) %>%
    mutate(tmp_order = 1)

  if (accept_sd) {
    max_nr_sd <- 1
  } else {
    max_nr_sd <- 0
  }
  pr_data <- filter_confirmation(
    source_data,
    by_vars = subject_keys,
    join_vars = vars(AVALC, ADT),
    join_type = "after",
    order = vars(ADT),
    first_cond = AVALC.join %in% c("CR", "PR") &
      ADT.join >= ADT + days(ref_confirm),
    filter = AVALC == "PR" &
      all(AVALC.join %in% c("CR", "PR", "SD", "NE")) &
      count_vals(var = AVALC.join, val = "NE") <= max_nr_ne &
      count_vals(var = AVALC.join, val = "SD") <= max_nr_sd &
      (
        min_cond(
          var = ADT.join,
          cond = AVALC.join == "CR"
        ) > max_cond(var = ADT.join, cond = AVALC.join == "PR") |
          count_vals(var = AVALC.join, val = "CR") == 0 |
          count_vals(var = AVALC.join, val = "PR") == 0
      )
  ) %>%
    mutate(tmp_order = 2)

  sd_data <- filter(
    source_data,
    AVALC %in% c("CR", "PR", "SD") & ADT >= !!reference_date + days(ref_start_window)
  ) %>%
    mutate(
      AVALC = "SD",
      tmp_order = 3
    )

  non_data <- filter(
    source_data,
    AVALC == "NON-CR/NON-PD" & ADT >= !!reference_date + days(ref_start_window)
  ) %>%
    mutate(
      tmp_order = 4
    )

  pd_data <- filter(source_data, AVALC == "PD") %>%
    mutate(tmp_order = 5)

  ne_data <- filter(
    source_data,
    AVALC == "NE" | AVALC %in% c("CR", "PR", "SD", "NON-CR/NON-PD") &
      ADT < !!reference_date + days(ref_start_window)
  ) %>%
    mutate(
      AVALC = "NE",
      tmp_order = 6
    )

  nd_data <- filter(
    source_data,
    AVALC == "ND"
  ) %>%
    mutate(
      AVALC = "ND",
      tmp_order = 7
    )

  if (missing_as_ne) {
    missing_val <- "NE"
  } else {
    missing_val <- "MISSING"
  }

  source_vars <- colnames(source_data)
  adsl_vars <- colnames(dataset_adsl)

  missing_data <- dataset_adsl %>%
    select(!!!subject_keys, intersect(source_vars, adsl_vars)) %>%
    mutate(
      AVALC = missing_val,
      tmp_order = 8
    )

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
#' @family utils_fmt
#' @keywords utils_fmt
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
