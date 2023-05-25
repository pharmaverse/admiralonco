#' Adds a Parameter for Clinical Benefit
#'
#' Adds a parameter for clinical benefit/disease control
#'
#' @details
#' Clinical benefit/disease control is first identified by looking for subjects
#' having response status, and then derived for subjects that have at least one
#' evaluable non-PD response assessment prior to first PD (Progressive Disease)
#' (i.e., responses inclusive of `CR`, `PR`, `SD`, and `NON-CR/NON-PD`) and after a specified
#' amount of time from a reference date (`ref_start_window`).
#'
#' Note: The user input values they wish to include when determining
#' clinical benefit using the argument `clinben_vals`. The default values for this are
#' `CR`, `PR`, `SD`, and `NON-CR/NON-PD`, as listed above. In the below example,
#' eligible values be limited to `CR` and `PR`.
#'
#' Example: `clinben_vals <- c("CR", "PR")`
#'
#' \enumerate{
#'   \item The input dataset (`dataset`) is restricted to the observations matching
#'   `filter_source` and to observations before or at the date specified by `source_pd`.
#'
#'   \item This dataset is further restricted to include user-generated response
#'   assessments from `clinben_vals` or include response assessments of `CR`,
#'   `PR`, `SD`, and `NON-CR/NON-PD`, exclude missing response assessments, and
#'   exclude those less than `ref_start_window` after `reference_date`. The earliest
#'   assessment by `ADT` is then selected.
#'
#'   \item The dataset identified by `dataset` in `source_resp` is restricted
#'   according to its `filter` argument. The variable corresponding to the `date`
#'   parameter of `source_resp` is considered together with `ADT` from the
#'   previous step.
#'
#'   \item For the observations being added to `dataset`, `ADT` is set to the earlier
#'   of the first assessment date representing an evaluable non-PD assessment prior
#'   to first PD, or the date representing the start of response.
#'
#'   \item For the observations being added to `dataset`, `AVALC` is set to
#'   + `Y` for those subjects in the `dataset` meeting the criteria for clinical
#'   benefit above
#'
#'   + `N` for subjects not meeting the clinical benefit criteria in `dataset`
#'   or the dataset identified in `source_resp`
#'
#'   + `N` for subjects present in `dataset_adsl` but not present in `dataset`
#'   or the dataset identified in `source_resp`.
#'
#'   \item `AVAL` is derived using `AVALC` as input to the function specified in
#'   `aval_fun`.
#'
#'   \item The variables specified by `set_values_to` are added to the new observations
#'   with values equal to the values specified in the same.
#'
#'   \item The new observations are added to `dataset`. Variables held in common
#'   between `dataset` and `dataset_adsl` are kept for the new observations, and
#'   are populated with their values from `dataset_adsl`.
#'  }
#'
#' @param dataset Input dataset. This is the dataset to which the clinical
#' benefit rate parameter will be added.
#'
#'   The variables `PARAMCD`, `AVALC`, `ADT`, and those specified by the `subject_keys`
#'   parameter and the `reference_date` parameter are expected.
#'
#'   After applying `filter_source` and/or `source_pd` the variable `ADT` and the
#'   variables specified by `subject_keys` must be a unique key of the dataset.
#'
#' @param dataset_adsl ADSL input dataset.
#'
#'   The variables specified for `subject_keys`is expected. For each subject of
#'   the specified dataset a new observation is added to the input dataset. Variables
#'   in `dataset_adsl` that also appear in `dataset` will be populated with the
#'   appropriate subject-specific value for these new observations.
#'
#' @param filter_source Filter condition in `dataset` that represents records
#' for overall disease response assessment for a subject at a given timepoint,
#' e.g. `PARAMCD == "OVR"` or `PARAMCD == "OVRLRESP"`.
#'
#' @param source_resp A `date_source` object specifying the dataset, date variable,
#' and filter condition used to identify response status.
#'
#' @param source_pd A `date_source` object specifying the dataset, date variable,
#' and filter condition used to identify disease progression.
#'
#' @param source_datasets  A named list of data sets is expected.
#'
#'  The list must contain the names provided by the `dataset_name` field of the
#'  `date_source()` objects specified for `source_pd` and `source_resp`.
#'
#' @param reference_date Name of variable representing the index date for
#' `ref_start_window`. A variable providing a date. An unquoted symbol is expected.
#'
#' @param ref_start_window Integer representing number of days from `reference_date`
#' that must elapse before an evaluable non-PD assessment counts toward determining
#' clinical benefit.
#'
#' @param aval_fun *Deprecated*, please use `set_values_to` instead.
#'
#' Function to map character analysis value (`AVALC`) to numeric analysis value
#' (`AVAL`)
#'
#' The (first) argument of the function must expect a character vector and the
#' function must return a numeric vector.
#'
#' @param clinben_vals A vector of response values to be considered when determining
#' clinical benefit.
#'
#' @param set_values_to A named list returned by `exprs()` containing new variables
#' and their static value to be populated for the clinical benefit rate parameter
#' records, e.g. `exprs(PARAMCD = "CBR", PARAM = "Clinical Benefit Rate")`.
#'
#' @param subject_keys A named list returned by `exprs()` containing variables
#' used to uniquely identify subjects.
#'
#' @return The input dataset with a new parameter for clinical benefit
#'
#' @family der_prm_adrs
#'
#' @keywords der_prm_adrs
#'
#' @export
#'
#' @author Andrew Smith
#' @examples
#' library(lubridate)
#' library(dplyr)
#' library(admiral)
#'
#' adsl <- tibble::tribble(
#'   ~USUBJID, ~TRTSDT,
#'   "01",     ymd("2020-01-14"),
#'   "02",     ymd("2021-02-16"),
#'   "03",     ymd("2021-03-09"),
#'   "04",     ymd("2021-04-21")
#' ) %>%
#'   mutate(STUDYID = "AB42")
#'
#' adrs <- tibble::tribble(
#'   ~USUBJID, ~PARAMCD, ~AVALC, ~ADT,
#'   "01",     "RSP",    "Y",    ymd("2021-03-14"),
#'   "02",     "RSP",    "N",    ymd("2021-05-07"),
#'   "03",     "RSP",    "N",    NA,
#'   "04",     "RSP",    "N",    NA,
#'   "01",     "PD",     "N",    NA,
#'   "02",     "PD",     "Y",    ymd("2021-05-07"),
#'   "03",     "PD",     "N",    NA,
#'   "04",     "PD",     "N",    NA,
#'   "01",     "OVR",    "SD",   ymd("2020-03-14"),
#'   "01",     "OVR",    "PR",   ymd("2021-04-13"),
#'   "02",     "OVR",    "PR",   ymd("2021-04-08"),
#'   "02",     "OVR",    "PD",   ymd("2021-05-07"),
#'   "02",     "OVR",    "CR",   ymd("2021-06-20"),
#'   "03",     "OVR",    "SD",   ymd("2021-03-30"),
#'   "04",     "OVR",    "NE",   ymd("2021-05-21"),
#'   "04",     "OVR",    "NA",   ymd("2021-06-30"),
#'   "04",     "OVR",    "NE",   ymd("2021-07-24"),
#'   "04",     "OVR",    "ND",   ymd("2021-09-04"),
#' ) %>%
#'   mutate(STUDYID = "AB42", ANL01FL = "Y") %>%
#'   derive_vars_merged(
#'     dataset_add = adsl,
#'     by_vars = exprs(STUDYID, USUBJID),
#'     new_vars = exprs(TRTSDT)
#'   )
#'
#' pd <- date_source(
#'   dataset_name = "adrs",
#'   date = ADT,
#'   filter = PARAMCD == "PD" & AVALC == "Y" & ANL01FL == "Y"
#' )
#'
#' resp <- date_source(
#'   dataset_name = "adrs",
#'   date = ADT,
#'   filter = PARAMCD == "RSP" & AVALC == "Y" & ANL01FL == "Y"
#' )
#'
#' derive_param_clinbenefit(
#'   dataset = adrs,
#'   dataset_adsl = adsl,
#'   filter_source = PARAMCD == "OVR" & ANL01FL == "Y",
#'   source_resp = resp,
#'   source_pd = pd,
#'   source_datasets = list(adrs = adrs),
#'   reference_date = TRTSDT,
#'   ref_start_window = 28,
#'   set_values_to = exprs(
#'     PARAMCD = "CBR"
#'   )
#' ) %>%
#'   filter(PARAMCD == "CBR")
derive_param_clinbenefit <- function(dataset,
                                     dataset_adsl,
                                     filter_source,
                                     source_resp,
                                     source_pd = NULL,
                                     source_datasets,
                                     reference_date,
                                     ref_start_window,
                                     aval_fun,
                                     clinben_vals = c("CR", "PR", "SD", "NON-CR/NON-PD"),
                                     set_values_to,
                                     subject_keys = get_admiral_option("subject_keys")) {
  # Assertions and quotes
  reference_date <- assert_symbol(enexpr(reference_date))
  assert_vars(subject_keys)
  assert_data_frame(
    dataset_adsl,
    required_vars = subject_keys
  )
  assert_data_frame(
    dataset,
    required_vars = expr_c(subject_keys, reference_date, exprs(PARAMCD, AVALC, ADT))
  )

  filter_source <- assert_filter_cond(
    enexpr(filter_source),
    optional = FALSE
  )

  assert_s3_class(source_resp, "date_source")
  assert_s3_class(source_pd, "date_source", optional = TRUE)
  assert_list_of(source_datasets, "data.frame")
  assert_character_vector(clinben_vals)

  if (!missing(aval_fun)) {
    deprecate_warn("0.4.0", "derive_param_clinbenefit(aval_fun = )", "derive_param_clinbenefit(set_values_to = )")
    assert_function(aval_fun)
    set_values_to <- exprs(!!!set_values_to, AVAL = {{aval_fun}}(AVALC))
  }

  source_names <- names(source_datasets)

  if (!is.null(source_pd)) {
    if (!all(c(source_pd$dataset_name, source_resp$dataset_name) %in% source_names)) {
      abort(
        paste0(
          "The dataset names specified for `source_pd` and `source_resp` must be \n",
          "included in the list specified for the `source_datasets` parameter.\n",
          "Following names were provided by `source_datasets`:\n",
          enumerate(source_names, quote_fun = squote)
        )
      )
    }
  } else {
    if (!all(source_resp$dataset_name %in% source_names)) {
      abort(
        paste0(
          "The dataset names specified for `source_resp` must be \n",
          "included in the list specified for the `source_datasets` parameter.\n",
          "Following names were provided by `source_datasets`:\n",
          enumerate(source_names, quote_fun = squote)
        )
      )
    }
  }

  ref_start_window <- assert_integer_scalar(ref_start_window, subset = "non-negative")
  assert_varval_list(set_values_to, optional = TRUE)
  assert_param_does_not_exist(dataset, set_values_to$PARAMCD)

  # ADSL variables
  adsl_vars <- intersect(colnames(dataset_adsl), colnames(dataset))
  adsl <- dataset_adsl %>%
    select(all_of(adsl_vars))

  # Get response date

  rsp_data <- source_datasets[[source_resp$dataset_name]] %>%
    filter_if(source_resp$filter) %>%
    rename(ADT = !!source_resp$date)

  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # filter_pd and filter_source: Filter source dataset using filter_source----
  # argument and also filter data after progressive disease with filter_pd
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  if (!is.null(source_pd)) {
    # Look for valid non-PD measurements after window from reference date
    ovr_data <- filter_pd(
      dataset = dataset,
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

    ovr_data <- dataset %>%
      filter(!!filter_source)
  }

  ovr_data <- ovr_data %>%
    filter(
      AVALC %in% clinben_vals &
        ADT >= !!reference_date + days(ref_start_window)
    ) %>%
    filter_extreme(
      order = exprs(ADT),
      by_vars = subject_keys,
      mode = "first",
      check_type = "none"
    )

  # NAs are sorted to the end, i.e., adsl records are selected only for patients
  # without records in ovr_data and rsp_data
  new_param <- bind_rows(ovr_data, rsp_data, adsl) %>%
    filter_extreme(
      order = exprs(ADT),
      by_vars = subject_keys,
      mode = "first",
      check_type = "none"
    ) %>%
    mutate(
      AVALC = if_else(!is.na(ADT), "Y", "N"),
      !!!set_values_to
    )

  bind_rows(dataset, new_param)
}
