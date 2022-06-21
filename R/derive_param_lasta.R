#' @title  Adds a Parameter for the Last Disease Assessment
#
#' @description
#'     Derives Last Disease Assessment (optionally) up to first Progressive Disease
#
#' @details
#'    Calculates the last disease assessment by accessing the last record
#'    defined in `by_vars` after it has been arranged using the `order` argument.
#'
#'    Creates the new parameter record with AVAL/AVALC/ADT taken from the last source
#'    record (i.e. the last record defined in `by_vars` after it has been arranged
#'    using the `order` argument). One new record for each subject in the filtered
#'    input `dataset` is added to the input `dataset`.
#'
#'    Records after PD can be removed using the source_pd and source_datasets
#'    arguments.
#
# Function Arguments:
#
#' @param dataset Input dataframe from which the Last Disease Assessment will be
#'                be derived from and added to.
#'
#'   The columns `PARAMCD`, `ADT`, and `AVALC`and the columns specified in
#'   `subject_keys` are expected.
#'
#'    *Permitted Values:* a `data.frame()` object
#'
#'    *Required or Optional:* Required
#'
#' @param filter_source Filter to be applied to `dataset` to derive the
#'                      Last Disease Assessment
#'
#'    *Required or Optional:* Required
#'
#' @param order Sort order, after which the last record shall be taken by
#'              the `by_vars` to determine Last Disease Assessment. Created
#'              using `vars()`.
#'
#'    *Permitted Values:* an `vars` object
#'
#'    *Default:* `vars(STUDYID, USUBJID, ADT)`
#'
#'    *Required or Optional:* Required
#'
#' @param by_vars Grouping columns, the last of which (ordered by `order`)
#'                shall be taken as the Last Disease Assessment record. Created
#'                using `vars()`.
#'
#'    *Permitted Values:* an `vars` object
#'
#'    *Default:* `vars(STUDYID, USUBJID)`
#'
#'    *Required or Optional:* Required
#'
#' @param source_pd Date of first progressive disease (PD)
#'
#'   If the parameter is specified, the observations of the input `dataset` for
#'   deriving the new parameter are restricted to observations up to the
#'   specified date. Observations at the specified date are included. For
#'   subjects without first PD date all observations are take into account.
#'
#'   *Permitted Values:* a `date_source` object (see `date_source()`
#'   for details)
#'
#'   *Default:* `NULL`
#'
#'   *Required or Optional:* Optional
#'
#' @param source_datasets Source dataframe to be used to calculate the
#'                        first PD date
#'
#'   A named list of dataframes is expected (although for BoR) only one dataframe is
#'   needed. It links the `dataset_name` from `source_pd` with an existing dataframe.
#'
#'   For example if `source_pd = pd_date` with
#'   ```{r, eval=FALSE}
#'   pd_date <- date_source(
#'     dataset_name = "adrs",
#'     date = ADT,
#'     filter = PARAMCD == PD
#'   )
#'   ```
#'   and the actual response dataframe in the script is `myadrs`, `source_datasets
#'   = list(adrs = myadrs)` should be specified.
#'
#'    *Required or Optional:* Optional
#'
#' @param subject_keys Columns to uniquely identify a subject
#'
#'   A list of symbols created using `vars()`.
#'
#'   *Default:* `vars(STUDYID, USUBJID)`
#'
#'   *Required or Optional:* Required
#'
#' @param set_values_to Columns to set
#'
#'   A named list returned by `vars()` defining the columns to be set for the
#'   new parameter, e.g. `vars(PARAMCD = "LSTAC", PARAM = "Last Disease
#'    Assessment Censored at First PD by Investigator")` is expected. The values
#'    must be symbols, character strings, numeric values, or `NA`.
#'
#'    *Required or Optional:* Required
#'
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @examples
#'
#' library(dplyr)
#' library(lubridate)
#' library(admiral)
#' library(tibble)
#'
#' adsl <- tribble(
#'   ~USUBJID, ~TRTSDT,                      ~EOSDT,
#'   "01",     lubridate::ymd("2020-12-06"), lubridate::ymd("2022-03-06"),
#'   "02",     lubridate::ymd("2021-01-16"), lubridate::ymd("2022-02-03"),
#'   "03",     lubridate::ymd("2021-01-09"), lubridate::ymd("2021-02-24"),
#'   "04",     lubridate::ymd("2021-04-21"), lubridate::ymd("2021-09-15")
#' ) %>%
#'   mutate(STUDYID = "a_study_id")
#'
#' adrs <- tribble(
#'   ~USUBJID, ~PARAMCD, ~AVAL, ~AVALC, ~ASEQ, ~ADT,
#'   "01", "RSP", NA, "Y", 1, lubridate::ymd("2021-04-08"),
#'   "02", "RSP", NA, "N", 1, lubridate::ymd("2021-05-07"),
#'   "03", "RSP", NA, "N", 1, NA,
#'   "04", "RSP", NA, "N", 1, NA,
#'   "01", "PD", NA, "N", 1, NA,
#'   "02", "PD", NA, "Y", 1, lubridate::ymd("2021-05-07"),
#'   "03", "PD", NA, "N", 1, NA,
#'   "04", "PD", NA, "N", 1, NA,
#'   "01", "OVR", 3, "SD", 1, lubridate::ymd("2021-03-07"),
#'   "01", "OVR", 2, "PR", 1, lubridate::ymd("2021-04-08"),
#'   "02", "OVR", 3, "SD", 1, lubridate::ymd("2021-03-07"),
#'   "02", "OVR", NA, NA, 1, lubridate::ymd("2021-04-07"),
#'   "02", "OVR", 6, "PD", 1, lubridate::ymd("2021-05-07"),
#'   "03", "OVR", 3, "SD", 1, lubridate::ymd("2021-01-30"),
#'   "03", "OVR", 3, "SD", 2, lubridate::ymd("2021-01-30"),
#'   "04", "OVR", NA, "NE", 1, lubridate::ymd("2021-05-21"),
#'   "04", "OVR", 5, "NON-PD", 1, lubridate::ymd("2021-06-30"),
#'   "04", "OVR", NA, "NE", 1, lubridate::ymd("2021-07-24"),
#'   "04", "OVR", NA, "ND", 1, lubridate::ymd("2021-09-30"),
#' ) %>%
#'   mutate(STUDYID = "a_study_id")
#'
#' pd <- date_source(
#'   dataset_name = adrs,
#'   date         = ADT,
#'   filter       = PARAMCD == "PD" & AVALC == "Y"
#' )
#'
#' derive_param_lasta(
#'   dataset,
#'   filter_source = PARAMCD == "OVR" & ANL01FL == "Y",
#'   source_pd = pd,
#'   source_datasets = list(adrs = adrs),
#'   set_values_to = vars(
#'     PARAMCD = "LSTAC",
#'     PARAM = "Last Disease Assessment Censored at First PD by Investigator",
#'     PARCAT1 = "Tumor Response",
#'     PARCAT2 = "Investigator",
#'     PARCAT3 = "Recist 1.1",
#'     ANL01FL = "Y"
#'   )
#' )
#
#' @export
#
#' @author Stephen Gormley
#
#' @keywords ADRS
#
#' @return The dataframe passed in the `dataset` argument with additional
#'         columns and/or rows as set in the `set_values_to` argument.ß

derive_param_lasta <- function(dataset,
                               filter_source,
                               order = vars(STUDYID, USUBJID, ADT),
                               by_vars = vars(STUDYID, USUBJID),
                               source_pd = NULL,
                               source_datasets = NULL,
                               subject_keys = vars(STUDYID, USUBJID),
                               set_values_to) {

  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Assert statements (checked in order of signature) ----
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  assert_data_frame(
    arg = dataset,
    required_vars = quo_c(
      subject_keys,
      order,
      by_vars,
      vars(
        PARAMCD, ADT,
        AVAL, AVALC
      )
    )
  )

  filter_source <- assert_filter_cond(
    arg      = enquo(filter_source),
    optional = TRUE
  )

  assert_vars(arg = order)

  assert_vars(arg = by_vars)

  assert_vars(arg = subject_keys)

  assert_varval_list(
    arg               = set_values_to,
    required_elements = c("PARAMCD")
  )

  assert_param_does_not_exist(
    dataset = dataset,
    param   = rlang::quo_get_expr(set_values_to$PARAMCD)
  )

  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # filter_pd and filter_source: Filter source dataset using filter_source----
  # argument and also filter data after progressive disease with filter_pd
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  if (!is.null(source_pd)) {
    dataset_filter <- dataset %>%
      filter_pd(
        filter          = !!filter_source,
        source_pd       = source_pd,
        source_datasets = source_datasets,
        subject_keys    = subject_keys
      )
  } else {

    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # filter_source: Filter using filter_source argument ----
    # This would also be used to filter out records from dataset that are greater
    # than e.g. ADSL.TRTSDT
    # Not filtering data after progressive disease with filter_pd
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    dataset_filter <- dataset %>%
      filter(!!filter_source)
  }

  # Error if filter results in 0 records
  if (nrow(dataset_filter) == 0) {
    err_msg <- sprintf(
      "dataframe passed into %s argument with the filter %s has 0 records",
      "dataset",
      deparse(rlang::quo_get_expr(filter_source))
    )

    rlang::abort(err_msg)
  }

  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # filter_extreme: Filter last assessment using filter_extreme ----
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  param_lasta <- dataset_filter %>%
    filter_extreme(
      mode       = "last",
      order      = vars(!!!order),
      by_vars    = vars(!!!by_vars),
      check_type = "warning"
    )

  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # set_values_to: Execute set_values_to ----
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  tryCatch(

    param_lasta_values_set <- param_lasta %>%
      mutate(!!!set_values_to),
    error = function(cnd) {
      abort(
        paste0(
          "Assigning new columns with set_values_to has failed:\n",
          "set_values_to = (\n",
          paste(
            " ",
            names(set_values_to),
            "=",
            lapply(set_values_to, rlang::quo_get_expr),
            collapse = "\n"
          ),
          "\n)\nError message:\n  ",
          cnd
        )
      )
    }
  )

  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Bind back to passed dataset ----
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  return_this_dataframe <- bind_rows(
    dataset,
    param_lasta_values_set
  )
}
