#' Filter up to First PD (Progressive Disease) Date
#'
#' @description
#' `r lifecycle::badge("deprecated")` The `filter_pd()` function has been
#' deprecated in favor of `filter_relative()`.
#'
#' Filter a dataset to only include the source parameter records up to and
#' including the first PD (progressive disease). These records are passed to
#' downstream derivations regarding responses such as BOR (best overall
#' response).
#'
#' @param dataset Input dataset
#'
#'   The variables `ADT` and those specified by `subject_keys` are expected.
#'
#' @param filter Filter condition for restricting the input dataset
#'
#' @param source_pd A `admiral::date_source()` object providing the date of first PD
#'
#'   For each subject the first date (`date` field) in the provided dataset
#'   (`dataset_name` field) restricted by `filter` field is considered as first
#'   PD date.
#'
#' @param source_datasets A named list of data sets is expected.
#'
#'   The name must match the name provided by the `dataset_name` field of the
#'   `admiral::date_source()` object specified for `source_pd`.
#'
#' @param subject_keys Variables to uniquely identify a subject
#'
#'   A list of symbols created using `exprs()` is expected.
#'
#' @return A subset of the input dataset
#'
#' @details
#'
#' 1. The input dataset (`dataset`) is restricted by `filter`.
#'
#' 1. For each subject the first PD date is derived as the first date
#' (`source_pd$date`) in the source pd dataset
#' (`source_datasets[[source_pd$dataset_name]]`) restricted by
#' `source_pd$filter`.
#'
#' 1. The restricted input dataset is restricted to records up to first PD date.
#' Records matching first PD date are included. For subject without any first PD
#' date, all records are included.
#'
#' @author Teckla Akinyi, Stefan Bundfuss
#'
#' @export
#'
#' @family deprecated
#' @keywords deprecated
#'
#' @examples
#'
#' library(dplyr)
#' library(lubridate)
#' library(admiral)
#' # ensure that `date_source()` from admiralonco is used to avoid deprecation
#' # warning
#' unloadNamespace("admiralonco")
#' library(admiralonco)
#'
#' # Filter OVR records up to first PD, first PD date provided in separate BDS dataset (adevent)
#' adrs <- tibble::tribble(
#'   ~STUDYID,       ~USUBJID,      ~PARAMCD, ~AVALC, ~ADT,         ~ANL01FL,
#'   "CDISCPILOT01", "01-701-1015", "OVR",    "CR",   "2016-01-25", "Y",
#'   "CDISCPILOT01", "01-701-1015", "OVR",    "SD",   "2016-02-22", NA_character_,
#'   "CDISCPILOT01", "01-701-1015", "OVR",    "PD",   "2016-02-22", "Y",
#'   "CDISCPILOT01", "01-701-1015", "BOR",    "CR",   "2016-01-25", "Y",
#'   "CDISCPILOT01", "01-701-1034", "OVR",    "SD",   "2015-12-07", "Y",
#'   "CDISCPILOT01", "01-701-1034", "OVR",    "PD",   "2016-04-25", "Y",
#'   "CDISCPILOT01", "01-701-1034", "OVR",    "PD",   "2016-06-25", "Y",
#'   "CDISCPILOT01", "01-701-1034", "BOR",    "SD",   "2015-12-07", "Y",
#'   "CDISCPILOT01", "01-701-1035", "OVR",    "SD",   "2016-04-25", "Y",
#'   "CDISCPILOT01", "01-701-1035", "OVR",    "PR",   "2016-06-25", "Y",
#'   "CDISCPILOT01", "01-701-1035", "BOR",    "PR",   "2016-06-25", "Y"
#' ) %>% mutate(
#'   ADT = as_date(ADT)
#' )
#'
#' adevent <- tibble::tribble(
#'   ~STUDYID,       ~USUBJID,      ~PARAMCD, ~AVALC, ~ADT,
#'   "CDISCPILOT01", "01-701-1015", "PD",     "Y",    "2016-02-22",
#'   "CDISCPILOT01", "01-701-1034", "PD",     "Y",    "2016-04-25"
#' ) %>% mutate(
#'   ADT = as_date(ADT)
#' )
#'
#' pd <- date_source(
#'   dataset_name = "adevent",
#'   date = ADT,
#'   filter = PARAMCD == "PD"
#' )
#'
#' filter_pd(
#'   dataset = adrs,
#'   filter = PARAMCD == "OVR" & ANL01FL == "Y",
#'   source_pd = pd,
#'   source_datasets = list(adevent = adevent)
#' )
#'
#' # Filter OVR records up to first PD, first PD date provided in ADSL dataset
#' adsl <- tibble::tribble(
#'   ~STUDYID,       ~USUBJID,      ~PDDT,
#'   "CDISCPILOT01", "01-701-1015", "2016-02-22",
#'   "CDISCPILOT01", "01-701-1034", "2016-04-25",
#'   "CDISCPILOT01", "01-701-1035", ""
#' ) %>% mutate(
#'   PDDT = as_date(PDDT)
#' )
#'
#' pd <- date_source(
#'   dataset_name = "adsl",
#'   date = PDDT
#' )
#'
#' filter_pd(
#'   dataset = adrs,
#'   filter = PARAMCD == "OVR" & ANL01FL == "Y",
#'   source_pd = pd,
#'   source_datasets = list(adsl = adsl)
#' )
#'
#' # Filter OVR records up to first PD, first PD date provided in input dataset (PD parameter)
#' adrs <- tibble::tribble(
#'   ~STUDYID,       ~USUBJID,      ~PARAMCD, ~AVALC, ~ADT,         ~ANL01FL,
#'   "CDISCPILOT01", "01-701-1015", "OVR",    "CR",   "2016-01-25", "Y",
#'   "CDISCPILOT01", "01-701-1015", "OVR",    "SD",   "2016-02-22", NA_character_,
#'   "CDISCPILOT01", "01-701-1015", "OVR",    "PD",   "2016-02-22", "Y",
#'   "CDISCPILOT01", "01-701-1015", "BOR",    "CR",   "2016-01-25", "Y",
#'   "CDISCPILOT01", "01-701-1034", "OVR",    "SD",   "2015-12-07", "Y",
#'   "CDISCPILOT01", "01-701-1034", "OVR",    "PD",   "2016-04-25", "Y",
#'   "CDISCPILOT01", "01-701-1034", "OVR",    "PD",   "2016-06-25", "Y",
#'   "CDISCPILOT01", "01-701-1034", "BOR",    "SD",   "2015-12-07", "Y",
#'   "CDISCPILOT01", "01-701-1035", "OVR",    "SD",   "2016-04-25", "Y",
#'   "CDISCPILOT01", "01-701-1035", "OVR",    "PR",   "2016-06-25", "Y",
#'   "CDISCPILOT01", "01-701-1035", "BOR",    "PR",   "2016-06-25", "Y",
#'   "CDISCPILOT01", "01-701-1015", "PD",     "Y",    "2016-02-22", "Y",
#'   "CDISCPILOT01", "01-701-1034", "PD",     "Y",    "2016-04-25", "Y"
#' ) %>% mutate(
#'   ADT = as_date(ADT)
#' )
#'
#' pd <- date_source(
#'   dataset_name = "adrs",
#'   date = ADT,
#'   filter = PARAMCD == "PD"
#' )
#'
#' filter_pd(
#'   dataset = adrs,
#'   filter = PARAMCD == "OVR" & ANL01FL == "Y",
#'   source_pd = pd,
#'   source_datasets = list(adrs = adrs)
#' )
#'
#' # Filter OVR records up to first PD, first PD date derived from OVR records
#' adrs <- tibble::tribble(
#'   ~STUDYID,       ~USUBJID,      ~PARAMCD, ~AVALC, ~ADT,         ~ANL01FL,
#'   "CDISCPILOT01", "01-701-1015", "OVR",    "CR",   "2016-01-25", "Y",
#'   "CDISCPILOT01", "01-701-1015", "OVR",    "SD",   "2016-02-22", NA_character_,
#'   "CDISCPILOT01", "01-701-1015", "OVR",    "PD",   "2016-02-22", "Y",
#'   "CDISCPILOT01", "01-701-1015", "BOR",    "CR",   "2016-01-25", "Y",
#'   "CDISCPILOT01", "01-701-1034", "OVR",    "SD",   "2015-12-07", "Y",
#'   "CDISCPILOT01", "01-701-1034", "OVR",    "PD",   "2016-04-25", "Y",
#'   "CDISCPILOT01", "01-701-1034", "OVR",    "PD",   "2016-06-25", "Y",
#'   "CDISCPILOT01", "01-701-1034", "BOR",    "SD",   "2015-12-07", "Y",
#'   "CDISCPILOT01", "01-701-1035", "OVR",    "SD",   "2016-04-25", "Y",
#'   "CDISCPILOT01", "01-701-1035", "OVR",    "PR",   "2016-06-25", "Y",
#'   "CDISCPILOT01", "01-701-1035", "BOR",    "PR",   "2016-06-25", "Y"
#' ) %>% mutate(
#'   ADT = as_date(ADT)
#' )
#'
#' pd <- date_source(
#'   dataset_name = "adrs",
#'   date = ADT,
#'   filter = PARAMCD == "OVR" & ANL01FL == "Y" & AVALC == "PD"
#' )
#'
#' filter_pd(
#'   dataset = adrs,
#'   filter = PARAMCD == "OVR" & ANL01FL == "Y",
#'   source_pd = pd,
#'   source_datasets = list(adrs = adrs)
#' )
filter_pd <- function(dataset,
                      filter,
                      source_pd,
                      source_datasets,
                      subject_keys = get_admiral_option("subject_keys")) {
  deprecate_inform(
    when = "1.4",
    what = "filter_pd()",
    with = "admiral::filter_relative()",
    details = c(
      x = "This message will turn into a warning at the beginning of 2027.",
      i = "See admiral's deprecation guidance:
      https://pharmaverse.github.io/admiraldev/dev/articles/programming_strategy.html#deprecation"
    )
  )

  # Check input arguments
  assert_vars(subject_keys)
  assert_data_frame(dataset,
    required_vars = exprs(!!!subject_keys, ADT),
    optional = FALSE
  )
  filter <- assert_filter_cond(
    enexpr(filter),
    optional = FALSE
  )
  assert_s3_class(source_pd, "date_source")
  assert_list_of(source_datasets, "data.frame")
  source_names <- names(source_datasets)
  if (!source_pd$dataset_name %in% source_names) {
    cli_abort(
      paste0(
        "The dataset name specified for `source_pd` must be included in the list\n",
        " specified for the `source_datasets` parameter.\n",
        "Following names were provided by `source_datasets`:\n",
        "{.val {source_names}}"
      )
    )
  }

  # Merge filtered source data and compare dates to keep only those up to source date
  dataset %>%
    filter_if(filter) %>%
    derive_vars_merged(
      dataset_add = source_datasets[[source_pd$dataset_name]],
      filter_add = !!source_pd$filter,
      by_vars = subject_keys,
      order = exprs(!!source_pd$date),
      new_vars = exprs(temp_date = !!source_pd$date),
      mode = "first"
    ) %>%
    filter(is.na(temp_date) | ADT <= temp_date) %>%
    select(-temp_date)
}

#' Create a `date_source` object
#'
#' @description
#' `r lifecycle::badge("deprecated")` The `date_source()` function has been
#' deprecated without replacement as all functions using the `source_pd`
#' argument are deprecated as well.
#'
#' Create a `date_source` object as input for `source_pd` argument in
#' `{admiralonco}` functions.
#'
#' @param dataset_name The name of the dataset, i.e. a string, used to search for
#'   the date.
#'
#' @param filter An unquoted condition for filtering `dataset`.
#'
#' @param date A variable or an expression providing a date. A date or a
#'   datetime can be specified. An unquoted symbol or expression is expected.
#'
#' @param set_values_to Variables to be set
#'
#' @family deprecated
#' @keywords deprecated
#'
#' @export
#'
#' @return An object of class `date_source`.
#'
#' @examples
#'
#' pd <- date_source(
#'   dataset_name = "adrs",
#'   date = ADT,
#'   filter = PARAMCD == "PD"
#' )
date_source <- function(dataset_name,
                        filter = NULL,
                        date,
                        set_values_to = NULL) {
  deprecate_inform(
    when = "1.4.0",
    what = "date_source()",
    details = c(
      x = "This message will turn into a warning {at the beginning of 2027}.",
      i = paste0(
        "See admiral's deprecation guidance: https://pharmaverse.github.io/",
        "admiraldev/dev/articles/programming_strategy.html#deprecation"
      )
    )
  )

  out <- list(
    dataset_name = assert_character_scalar(dataset_name),
    filter = assert_filter_cond(enexpr(filter), optional = TRUE),
    date = assert_expr(enexpr(date)),
    set_values_to = assert_expr_list(set_values_to, named = TRUE, optional = TRUE)
  )
  class(out) <- c("date_source", "source", "list")
  out
}
