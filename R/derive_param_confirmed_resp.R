#' Adds a Parameter for Confirmed Response
#'
#' Adds a parameter for confirmed response
#'
#' @param dataset Input dataset
#'
#'   The `PARAMCD`, `ADT`, and `AVALC` variables and the variables specified by
#'   `subject_keys` and `reference_date` are expected.
#'
#'   After applying `filter_source` and/or `source_pd` the variable `ADT` and the
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
#'   considered for deriving the confirmed response.
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
#'   ```{r eval=FALSE}
#'   pd_date <- date_source(
#'     dataset_name = "adrs",
#'     date = ADT,
#'     filter = PARAMCD == PD
#'   )
#'   ```
#'   and the actual response dataset in the script is `myadrs`, `source_datasets
#'   = list(adrs = myadrs)` should be specified.
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
#' @param aval_fun Function to map character analysis value (`AVALC`) to numeric
#'   analysis value (`AVAL`)
#'
#'   The (first) argument of the function must expect a character vector and the
#'   function must return a numeric vector.
#'
#'   *Default:* `yn_to_numeric` (see `admiral::yn_to_numeric()` for details)
#'
#' @param set_values_to Variables to set
#'
#'   A named list returned by `vars()` defining the variables to be set for the
#'   new parameter, e.g. `vars(PARAMCD = "CRSP", PARAM = "Confirmed Response")`
#'   is expected. The values must be symbols, character strings, numeric values,
#'   or `NA`.
#'
#' @param subject_keys Variables to uniquely identify a subject
#'
#'   A list of symbols created using `vars()` is expected.
#'
#' @details
#'
#'   \enumerate{
#'   \item The input dataset (`dataset`) is restricted to the observations matching
#'   `filter_source` and to observations before or at the date specified by
#'   `source_pd`.
#'
#'   \item A subject is considered as responder if there is at least one
#'   observation in the restricted dataset with
#'
#'   \itemize{
#'   \item `AVALC == "CR"`,
#'   \item there is a confirmatory assessment with `AVALC == "CR"` at least
#'   `ref_confirm` days after the assessment,
#'   \item all assessments between the assessment and the confirmatory
#'   assessment are `"CR"` or `"NE"`, and
#'   \item there are at most `max_nr_ne` `"NE"` assessments between the
#'   assessment and the confirmatory assessment.}
#'
#'   or at least one observation with
#'
#'   \itemize{
#'   \item `AVALC == "PR"`,
#'   \item there is a confirmatory assessment with `AVALC %in% c("CR", "PR")` at
#'   least `ref_confirm` days after the assessment,
#'   \item all assessments between the assessment and the confirmatory
#'   assessment are `"CR"`, `"PR"`, `"SD"`, or `"NE"`,
#'   \item there is no `"PR"` assessment after a `"CR"` assessment in the
#'   confirmation period,
#'   \item there are at most `max_nr_ne` `"NE"` assessments between the
#'   assessment and the confirmatory assessment,
#'   \item if the `accept_sd` argument is set to `TRUE`, one `"SD"` assessment
#'   in the confirmation period is accepted. Otherwise, no `"SD"` assessment
#'   must occur within the confirmation period.
#'   }
#'
#'   \item For responders `AVALC` is set to `"Y"` and `ADT` to the first date where
#'   the response criteria are fulfilled. For all other subjects in
#'   `dataset_adsl` `AVALC` is set to `"N"` and `ADT` to `NA`.
#'
#'   \item The `AVAL` variable is added and set to `aval_fun(AVALC)`.
#'
#'   \item The variables specified by the `set_values_to` parameter are added to
#'   the new observations.
#'
#'   \item The new observations are added to input dataset.
#'   }
#'
#' @return The input dataset with a new parameter for confirmed response
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
#'     ADT = lubridate::ymd(ADTC),
#'     STUDYID = "XX1234"
#'   ) %>%
#'   select(-ADTC)
#'
#' pd_date <- date_source(
#'   dataset_name = "adrs",
#'   date = ADT,
#'   filter = PARAMCD == "PD" & ANL01FL == "Y"
#' )
#'
#' # Derive confirmed response parameter
#' derive_param_confirmed_resp(
#'   adrs,
#'   dataset_adsl = adsl,
#'   filter_source = PARAMCD == "OVR" & ANL01FL == "Y",
#'   source_pd = pd_date,
#'   source_datasets = list(adrs = adrs),
#'   ref_confirm = 28,
#'   set_values_to = vars(
#'     PARAMCD = "CRSP",
#'     PARAM = "Confirmed Response by Investigator"
#'   )
#' ) %>%
#'   filter(PARAMCD == "CRSP")
#'
#' # Derive confirmed response parameter (accepting SD for PR and two NEs)
#' derive_param_confirmed_resp(
#'   adrs,
#'   dataset_adsl = adsl,
#'   filter_source = PARAMCD == "OVR" & ANL01FL == "Y",
#'   source_pd = pd_date,
#'   source_datasets = list(adrs = adrs),
#'   ref_confirm = 28,
#'   max_nr_ne = 2,
#'   accept_sd = TRUE,
#'   set_values_to = vars(
#'     PARAMCD = "CRSP",
#'     PARAM = "Confirmed Response by Investigator"
#'   )
#' ) %>%
#'   filter(PARAMCD == "CRSP")
derive_param_confirmed_resp <- function(dataset,
                                        dataset_adsl,
                                        filter_source,
                                        source_pd = NULL,
                                        source_datasets = NULL,
                                        ref_confirm,
                                        max_nr_ne = 1,
                                        accept_sd = FALSE,
                                        aval_fun = yn_to_numeric,
                                        set_values_to,
                                        subject_keys = vars(STUDYID, USUBJID)) {
  # Check input parameters
  filter_source <- assert_filter_cond(enquo(filter_source))
  assert_integer_scalar(ref_confirm, subset = "non-negative")
  assert_integer_scalar(max_nr_ne, subset = "non-negative")
  assert_logical_scalar(accept_sd)
  assert_function(aval_fun)
  assert_varval_list(set_values_to, required_elements = "PARAMCD")
  assert_vars(subject_keys)
  assert_data_frame(
    dataset,
    required_vars = quo_c(subject_keys, vars(PARAMCD, ADT, AVALC))
  )
  assert_data_frame(dataset_adsl, required_vars = subject_keys)
  if (!is.null(dataset)) {
    assert_param_does_not_exist(dataset, quo_get_expr(set_values_to$PARAMCD))
  }

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
      filter(!!filter_source)
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
    mutate(
      AVALC = "Y"
    )

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
    mutate(
      AVALC = "Y"
    )

  source_vars <- colnames(source_data)
  adsl_vars <- colnames(dataset_adsl)

  missing_data <- dataset_adsl %>%
    select(!!!subject_keys, intersect(source_vars, adsl_vars)) %>%
    mutate(
      AVALC = "N"
    )

  # Select response (for missing_data ADT is NA and will be sorted to the end,
  # i.e., an observation from missing_data is selected only if there is no
  # observation in cr_data or pr_data)
  rsp <- bind_rows(cr_data, pr_data, missing_data) %>%
    filter_extreme(
      by_vars = subject_keys,
      order = vars(ADT),
      mode = "first"
    ) %>%
    mutate(
      !!!set_values_to
    ) %>%
    call_aval_fun(
      aval_fun
    )


  # Add to input dataset
  bind_rows(dataset, rsp)
}
