#' Filter Confirmed Assessments
#'
#' The function filters observation using a condition taking subsequent
#' observations into account. For example, it could select all observations with
#' `AVALC == "Y"` and `AVALC == "Y"` for at least one subsequent observation.
#' The input dataset is joined with itself to enable conditions taking variables
#' from both the current observation and the subsequent observations into
#' account. The suffix ".join" is added to the variables from the subsequent
#' observations.
#'
#' @param dataset Input dataset
#'
#'   The variables specified for `by_vars`, `join_vars`, and `order` are
#'   expected.
#'
#' @param by_vars By variables
#'
#'   The specified variables are used as by variables for joining the input
#'   dataset with itself.
#'
#' @param join_vars Variables to keep from joined dataset
#'
#'   The variables needed from the subsequent observations should be specified
#'   for this parameter. The specified variables are added to the joined dataset
#'   with suffix ".join". For example to select all observations with `AVALC ==
#'   "Y"` and `AVALC == "Y"` for at least one subsequent visit `join_vars =
#'   vars(AVALC, AVISITN)` and `filter = AVALC == "Y" & AVALC.join == "Y" &
#'   AVISITN < AVISITN.join` could be specified.
#'
#' @param first_cond Condition for selecting range of data
#'
#'   If this argument is specified, the subsequent observations are restricted
#'   up to the first observation where the specified condition is fulfilled. If
#'   the condition is not fulfilled for any of the subsequent observations, all
#'   observations are removed.
#'
#' @param order Order
#'
#'   The observations are ordered by the specified order.
#'
#' @param filter Condition for selecting observations
#'
#'   The filter is applied to the joined dataset for selecting the confirmed
#'   observations. The condition can include summary functions. The joined
#'   dataset is grouped by the original observations. I.e., the summary function
#'   are applied to all observations up to the confirmation observation. For
#'   example, `filter = AVALC == "CR" & all(AVALC.join %in% c("CR", "NE")) &
#'   count_vals(var = AVALC.join, val = "NE") <= 1` selects observations with
#'   response "CR" and for all observations up to the confirmation observation
#'   the response is "CR" or "NE" and there is at most one "NE".
#'
#' @param check_type Check uniqueness?
#'
#'   If `"warning"` or `"error"` is specified, the specified message is issued
#'   if the observations of the input dataset are not unique with respect to the
#'   by variables and the order.
#'
#'   *Default:* `"none"`
#'
#'   *Permitted Values:* `"none"`, `"warning"`, `"error"`
#'
#' @author Stefan Bundfuss
#'
#' @keywords adam user_utility
#'
#' @export
#'
#' @examples
#'
#' library(dplyr)
#' library(admiralonco)
#'
#' # filter observations with AVALC == "Y" and AVALC == "Y" at one subsequent visit
#' data <- tibble::tribble(
#'   ~USUBJID, ~AVISITN, ~AVALC,
#'   "1",      1,        "Y",
#'   "1",      2,        "N",
#'   "1",      3,        "Y",
#'   "1",      4,        "N",
#'   "2",      1,        "Y",
#'   "2",      2,        "N",
#'   "3",      1,        "Y",
#'   "4",      1,        "N",
#'   "4",      2,        "N",
#' )
#'
#' filter_confirmation(
#'   data,
#'   by_vars = vars(USUBJID),
#'   join_vars = vars(AVALC, AVISITN),
#'   order = vars(AVISITN),
#'   filter = AVALC == "Y" & AVALC.join == "Y" & AVISITN < AVISITN.join
#' )
#'
#' # select observations with AVALC == "CR", AVALC == "CR" at a subsequent visit,
#' # only "CR" or "NE" in between, and at most one "NE" in between
#' data <- tibble::tribble(
#'   ~USUBJID, ~AVISITN, ~AVALC,
#'   "1",      1,        "PR",
#'   "1",      2,        "CR",
#'   "1",      3,        "NE",
#'   "1",      4,        "CR",
#'   "1",      5,        "NE",
#'   "2",      1,        "CR",
#'   "2",      2,        "PR",
#'   "2",      3,        "CR",
#'   "3",      1,        "CR",
#'   "4",      1,        "CR",
#'   "4",      2,        "NE",
#'   "4",      3,        "NE",
#'   "4",      4,        "CR",
#'   "4",      5,        "PR"
#' )
#'
#' filter_confirmation(
#'   data,
#'   by_vars = vars(USUBJID),
#'   join_vars = vars(AVALC),
#'   order = vars(AVISITN),
#'   first_cond = AVALC.join == "CR",
#'   filter = AVALC == "CR" & all(AVALC.join %in% c("CR", "NE")) &
#'     count_vals(var = AVALC.join, val = "NE") <= 1
#' )
filter_confirmation <- function(dataset,
                                by_vars,
                                join_vars,
                                first_cond = NULL,
                                order,
                                filter,
                                check_type = "warning") {
  # Check input parameters
  assert_vars(by_vars)
  assert_vars(join_vars)
  first_cond <- assert_filter_cond(enquo(first_cond), optional = TRUE)
  assert_order_vars(order)
  filter <- assert_filter_cond(enquo(filter))
  check_type <-
    assert_character_scalar(
      check_type,
      values = c("none", "warning", "error"),
      case_sensitive = FALSE
    )
  assert_data_frame(
    dataset,
    required_vars = admiral:::quo_c(by_vars, join_vars, admiral:::extract_vars(order))
  )

  # number observations of the input dataset to get a unique key
  # (by_vars and tmp_obs_nr_filter_confirmation)
  data <- dataset %>%
    derive_var_obs_number(
      new_var = tmp_obs_nr_filter_confirmation,
      by_vars = by_vars,
      order = order,
      check_type = check_type
    )
  # join the input dataset with itself such that to each observation of the
  # input dataset all following observations are joined
  data_joined <- data %>%
    left_join(
      select(data, !!!by_vars, !!!join_vars, tmp_obs_nr_filter_confirmation),
      by = vars2chr(by_vars),
      suffix = c("", ".join")
    ) %>%
    filter(tmp_obs_nr_filter_confirmation <= tmp_obs_nr_filter_confirmation.join)

  if (!quo_is_null(first_cond)) {
    # select all observations up to the first confirmation observation
    data_joined <- filter_relative(
      data_joined,
      by_vars = vars(!!!by_vars, tmp_obs_nr_filter_confirmation),
      condition = !!first_cond & tmp_obs_nr_filter_confirmation < tmp_obs_nr_filter_confirmation.join,
      order = vars(tmp_obs_nr_filter_confirmation.join),
      mode = "first",
      selection = "before",
      inclusive = TRUE,
      keep_no_ref_groups = FALSE
    )
  }

  # apply confirmation condition, which may include summary functions
  data_joined %>%
    group_by(!!!by_vars, tmp_obs_nr_filter_confirmation) %>%
    filter(!!filter) %>%
    # select one observation of each group, as the joined variables are removed
    # it doesn't matter which one, so we take just the first one
    slice(1L) %>%
    ungroup() %>%
    select(colnames(dataset))
}


#' Count Number of Observations Where a Variable Equals a Value
#'
#' Count number of observations where a variable equals a value.
#'
#' @param var A vector
#'
#' @param val A value
#'
#' @author Stefan Bundfuss
#'
#' @export
#'
#' @keywords user_utility
#'
#' @examples
#'
#' library(dplyr)
#' library(admiralonco)
#' data <- tibble::tribble(
#'   ~USUBJID, ~AVISITN, ~AVALC,
#'   "1",      1,        "PR",
#'   "1",      2,        "CR",
#'   "1",      3,        "NE",
#'   "1",      4,        "CR",
#'   "1",      5,        "NE",
#'   "2",      1,        "CR",
#'   "2",      2,        "PR",
#'   "2",      3,        "CR",
#'   "3",      1,        "CR",
#'   "4",      1,        "CR",
#'   "4",      2,        "NE",
#'   "4",      3,        "NE",
#'   "4",      4,        "CR",
#'   "4",      5,        "PR"
#' )
#'
#' group_by(data, USUBJID) %>%
#'   mutate(nr_nes = count_vals(var = AVALC, val = "NE"))
count_vals <- function(var, val) {
  length(var[var == val])
}
