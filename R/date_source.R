#' Pre-Defined Date Source Objects
#'
#' These pre-defined `date_source` objects can be used as input to `derive_param_CBOR()`
#'
#' @param dataset_name Input dataset
#'
#'   The columns specified by the `filter` and the `date` parameter are
#'   expected.
#'
#' @param filter Filtering condition applied to input dataset.
#'
#'   For example, it can be used to filter for death.
#'
#'   Defaults to NULL.
#'
#' @param date The date column for which the response/PD/death date is
#'
#'   A date or date-time object column is expected.
#'
#' @details This function only works correctly for date or date-time where any partial dates are already imputed.
#'
#' @author Cloris Xue
#'
#' @return The object of class date_source
#'
#' @keywords date_source
#'
#' @examples
#' "dummy_data <- tibble::tribble(
#'   ~STUDYID, ~USUBJID, ~PARAMCD,  ~ADT,              ~AVAL,   ~DTC,
#'   "TEST01", "PAT01",  "PDDT",  as.Date("2021-04-27"),  "",   "2021-04-27",
#'   "TEST01", "PAT02",  "PDDT",  as.Date("2021-04-27"),  "",   "2021-04",
#'   "TEST01", "PAT01",  "RSP",  as.Date("2021-04-27"),  "Y",   "2021-04-27"
#' )
#'
#' death <- date_source(dateset_name = "dummy_data", date = ADT, filter = PARAMCD == "DEATH")
#' resp <- date_source(dataset_name = "dummy_data", date = ADT, filter = PARAMCD == "RSP" & AVALC == "Y")
#' pd <- date_source(dataset_name = "dummy_data", date = PDDT, filter = NULL)
#'
date_source <- function(dataset_name, date, filter = NULL, set_values_to = NULL) {
  assert_that((!sapply(get(dataset_name) %>% select({{date}}), class) == "character"),
              msg = "date = needs to be DT or DTM format")
  out <- list(dataset_name = assert_character_scalar(dataset_name),
              filter = assert_filter_cond(enquo(filter), optional = TRUE),
              date = assert_symbol(enquo(date)),
              set_values_to = set_values_to)
  class(out) <- c("date_source", "list")
  out
}
