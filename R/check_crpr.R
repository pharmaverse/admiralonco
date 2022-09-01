.datasets <- new.env(parent = emptyenv())

#' Signal CR Records Followed by PR
#'
#' @param dataset A data frame
#'
#' @param order A list of variables created using `vars()` determining the order
#'   or the records
#'
#' @param msg The condition message
#'
#' @param subject_keys Variables to uniquely identify a subject
#'
#'   A list of symbols created using `vars()` is expected.
#'
#' @param check_type Type of message to issue when detecting PR after CR.
#'
#'   *Permitted Values*: `"message"`, `"warning"` or `"error"`
#'
#' @return No return value, called for side effects
#'
#' @seealso [get_crpr_dataset()]
#'
#' @export
#'
#' @family utils_ds_chk
#'
#' @keywords utils_ds_chk
#'
#' @author Stefan Bundfuss
#'
#' @examples
#' library(tibble)
#' library(dplyr)
#' library(lubridate)
#' library(admiralonco)
#'
#' adrs <- tribble(
#'   ~USUBJID, ~ADTC,        ~AVALC,
#'   "1",      "2020-01-01", "PR",
#'   "1",      "2020-02-01", "CR",
#'   "1",      "2020-02-16", "NE",
#'   "1",      "2020-03-01", "CR",
#'   "2",      "2020-02-06", "PR",
#'   "2",      "2020-02-16", "CR",
#'   "2",      "2020-03-30", "PR",
#' ) %>%
#'   mutate(
#'     ADT = ymd(ADTC),
#'     STUDYID = "XX1234"
#'   )
#'
#' signal_crpr(adrs, order = vars(ADT))
signal_crpr <- function(dataset,
                        order,
                        msg = "Dataset contains CR records followed by PR.",
                        subject_keys = vars(STUDYID, USUBJID),
                        check_type = "warning") {
  assert_character_scalar(msg)
  assert_vars(subject_keys)
  assert_character_scalar(check_type, values = c("message", "warning", "error"))

  crpr_data <- filter_confirmation(
    dataset,
    by_vars = subject_keys,
    order = order,
    join_vars = vars(AVALC),
    join_type = "after",
    filter = AVALC == "CR" & AVALC.join == "PR"
  )

  if (nrow(crpr_data) > 0) {
    pr_data <- filter_confirmation(
      dataset,
      by_vars = subject_keys,
      order = order,
      join_vars = vars(AVALC),
      join_type = "before",
      filter = AVALC == "PR" & AVALC.join == "CR"
    )

    crpr_data <- bind_rows(crpr_data, pr_data) %>%
      arrange(!!!subject_keys, !!!order)

    .datasets$crpr <- crpr_data
    full_msg <- paste0(
      msg,
      "\nRun `get_crpr_dataset()` to access the CR records records followed by PR"
    )
    msg_funs <- list(message = inform, warning = warn, error = abort)
    msg_funs[[check_type]](full_msg)
  }
}

#' Get CR Records Followed by PR That Lead to a Prior Error
#'
#' @export
#'
#' @author Stefan Bundfuss
#'
#' @details
#' Some {admiralonco} function check that the in the source records CR is not
#' followed by PR and throw an error otherwise. The `get_crpr_dataset()`
#' function allows one to retrieve the duplicate records that lead to an error.
#'
#' Note that the function always returns the dataset of duplicates from the last
#' error that has been thrown in the current R session. Thus, after restarting
#' the R sessions `get_crpr_dataset()` will return `NULL` and after a second
#' error has been thrown, the dataset of the first error can no longer be
#' accessed (unless it has been saved in a variable).
#'
#' @return A `data.frame` or `NULL`
#'
#' @seealso [signal_crpr()]
#'
#' @family utils_ds_chk
#'
#' @keywords utils_ds_chk
#'
#' @examples
#' library(tibble)
#' library(dplyr)
#' library(lubridate)
#' library(admiralonco)
#'
#' adrs <- tribble(
#'   ~USUBJID, ~ADTC,        ~AVALC,
#'   "1",      "2020-01-01", "PR",
#'   "1",      "2020-02-01", "CR",
#'   "1",      "2020-02-16", "NE",
#'   "1",      "2020-03-01", "CR",
#'   "2",      "2020-02-06", "PR",
#'   "2",      "2020-02-16", "CR",
#'   "2",      "2020-03-30", "PR",
#' ) %>%
#'   mutate(
#'     ADT = ymd(ADTC),
#'     STUDYID = "XX1234"
#'   )
#'
#' signal_crpr(adrs, order = vars(ADT))
#'
#' get_crpr_dataset()
get_crpr_dataset <- function() {
  .datasets$crpr
}
