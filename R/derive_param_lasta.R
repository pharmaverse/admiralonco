#' Adds a Parameter for the Last Disease Assessment
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function is *deprecated*, please use `admiral::derive_param_extreme_event()` instead.
#'
#' @details
#'    Calculates the last disease assessment by accessing the last record
#'    defined in `subject_keys` after it has been arranged using the `order` argument.
#'
#'    Creates a new parameter record (keeping all columns passed) from the last source
#'    record (i.e. the last record defined in `subject_keys` after it has been arranged
#'    using the `order` argument). One new record for each subject in the filtered
#'    input `dataset` is added to the input `dataset`.
#'
#'    Records after PD can be removed using the `source_pd` and `source_datasets`
#'    arguments.
#
# Function Arguments:
#
#' @param dataset Input dataframe from which the Last Disease Assessment will be
#'                be derived from and added to.
#'
#'   The column `PARAMCD` and the columns specified in `subject_keys` and
#'   `order` are expected.
#'
#'   After applying `filter_source` and/or `source_pd` the columns specified by
#'   `subject_keys` and `order` must be a unique key of the dataframe
#'
#'    *Permitted Values:* a `data.frame()` object
#'
#' @param filter_source Filter to be applied to `dataset` to derive the
#'                      Last Disease Assessment
#'
#' @param order Sort order, after which the last record shall be taken by
#'              the `subject_keys` to determine Last Disease Assessment. Created
#'              using `exprs()`.
#'
#'    *Permitted Values:* list of variables or `desc(<variable>)` function calls
#'    created by `exprs()`, e.g., `exprs(ADT, desc(AVAL))`
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
#' @param source_datasets Source dataframe to be used to calculate the
#'                        first PD date
#'
#'   A named list of dataframes is expected (although for BOR) only one dataframe is
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
#' @param subject_keys Columns to uniquely identify a subject
#'
#'   A list of symbols created using `exprs()`.
#'
#' @param set_values_to Columns to set
#'
#'   A named list returned by `exprs()` defining the columns to be set for the
#'   new parameter, e.g. `exprs(PARAMCD = "LSTAC", PARAM = "Last Disease
#'    Assessment Censored at First PD by Investigator")` is expected. The values
#'    must be symbols, character strings, numeric values, or `NA`.
#
#' @export
#'
#' @author Stephen Gormley
#'
#' @family deprecated
#'
#' @keywords deprecated
#'
#' @return The dataframe passed in the `dataset` argument with additional
#'         columns and/or rows as set in the `set_values_to` argument.

derive_param_lasta <- function(dataset,
                               filter_source,
                               order = exprs(ADT),
                               source_pd = NULL,
                               source_datasets = NULL,
                               subject_keys = get_admiral_option("subject_keys"),
                               set_values_to) {
  ### DEPRECATION
  deprecate_stop("0.2.0", "derive_param_lasta()", "admiral::derive_param_extreme_event()")
}
