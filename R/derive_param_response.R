#' Add a Parameter Indicating If a Subject Had a Response
#' 
#' Add a new parameter indicating if a response has been observed (AVALC and AVAL).
#' If a response has been observed, AVALC is set to "Y", AVAL to 1 and ADT is set to the first date 
#' when a response has been observed.
#' If a response has not been observed, AVALC is set to "N", AVAL to 0 and ADT is set NA
#' 
#' @param dataset Input dataset expected.
#'   The variables specified by the `subject_keys` are expected expected.
#'   
#' @param dataset_adsl Input dataset
#'
#'   + The variables specified for `subject_keys` are expected. 
#'   + For each observation of the specified dataset a new observation is added to the input dataset.
#'
#' @param source_datasets Source dataset
#'
#'   All observations in the specified dataset fulfilling the condition
#'   specified by `filter_source` are considered as response
#'
#'   The variables specified by the `subject_keys` and
#'   `date_var` parameter are expected.
#'
#' @param filter_source Source filter
#'
#'   All observations in `source_datasets` fulfilling the specified condition are
#'   considered as response
#'
#'   For subjects with at least one response `AVALC` is set to `"Y"`, `AVAL` to
#'   `1`, and `ADT` to the first date where the response occured.
#'
#'   For all other subjects `AVALC` is set to `"N"`, `AVAL` to `0`, and `ADT` to
#'   `NA`.
#'
#' @param date_var Date variable
#'
#'   Date variable in the source dataset (`source_datasets`). The variable is
#'   used to sort the source dataset. `ADT` is set to the specified variable for
#'   response
#'
#' @param set_values_to Variables to set
#'
#'   A named list returned by `vars()` defining the variables to be set for the
#'   new parameter, e.g. `vars(PARAMCD = "RSP", PARAM = "Response by investigator")`
#'   is expected. The values must be symbols, character strings, numeric values,
#'   or `NA`.
#'
#' @param subject_keys Variables to uniquely identify a subject
#'
#'   A list of symbols created using `vars()` is expected.
#'
#'
#' @details
#'   1. The input dataset is restricted to observations fulfilling
#'   `filter_source`.
#'   1. For each subject (with respect to the variables specified for the
#'   `subject_keys` parameter) the first observation (with respect to
#'   `date_var`) where the event condition (`filter_source` parameter) is
#'   fulfilled is selected.
#'   1. For each observation in `dataset_adsl` a new observation is created. For
#'   subjects with event `AVALC` is set to `"Y"`, `AVAL` to `1`, and `ADT` to
#'   the first date where the event condition is fulfilled. For all other
#'   subjects `AVALC` is set to `"N"`, `AVAL` to `0`, and `ADT` to `NA`.
#'   1. The variables specified by the `set_values_to` parameter are added to
#'   the new observations.
#'   1. The new observations are added to input dataset.
#'
#' @author Samia Kabi
#'
#' @return The input dataset with a new parameter indicating if and when an
#'   response occurred
#'
#' @keywords derivation bds
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' library(lubridate)
#' library(admiral)
#' library(admiraltest)
derive_param_response<- function (
  dataset,
  dataset_adsl,
  filter_source,
  source_pd,
  source_datasets,
  set_values_to,
  subject_keys = vars(STUDYID, USUBJID)
){
 
  # ---- checking and quoting ---- 
  assert_data_frame(dataset)
  assert_data_frame(dataset_adsl)
  filter_s <- assert_filter_cond(enquo(filter_source), optional = TRUE)
  assert_list_of(source_datasets, "data.frame")
  source_names <- names(source_datasets)
  
  assert_s3_class(source_pd, "date_source")
  if (!(source_pd$dataset_name %in% source_names)){
    abort(paste( 
      "The dataset names must be included in the list specified for the ",
    "`source_datasets` parameter.\n",
    "Following names were provided by `source_datasets`:\n",
    enumerate(source_names, quote_fun = squote)
    ))
  }

  assert_varval_list(set_values_to, accept_expr = TRUE, optional = TRUE)
  if (!is.null(set_values_to$PARAMCD) & !is.null(dataset)) {
    assert_param_does_not_exist(dataset, quo_get_expr(set_values_to$PARAMCD))
  } 
  
  # ---- Read in data ----
  data <- vector("list", length(source_names))
  for (i in seq_along(source_names)) {
      #---PD  datasets ----
     if (source_names[[i]] == source_pd$dataset_name){
       pddt<- source_datasets[[i]] %>%
         #admiral::filter_if(source_pd$filter) %>%
         filter_if(source_pd$filter) %>%
         mutate(PDDT=ADT) %>%
         select(!!!subject_keys, PDDT)
     }
    # ---- response datasets ----
    else {
    data[[i]] <- source_datasets[[i]] %>%
      #admiral::filter_if(filter_s)
      filter_if(filter_s)
    }
  }
  
  #---- Only responses before PD ----
  resp_data<-data %>%
      bind_rows() %>%
      left_join(pddt, by = vars2chr(subject_keys))%>%
      filter(ADT <= PDDT | is.na(PDDT)) %>% #Q: how to handle the < or  <=?
      select (!!!subject_keys,ADT, AVALC_=AVALC)

  # ---- Select the First response ----
  fresp_data<-resp_data %>%
    filter_extreme(
    by_vars = subject_keys,
    order = vars(ADT),
    mode = "first"
  ) 

  # ---- Create a record for each pt in ADSL ----
  add_data<-dataset_adsl %>%
    left_join(fresp_data, by = vars2chr(subject_keys))%>%
    mutate(AVALC= if_else(!is.na(AVALC_), "Y", "N"),
           ADT=if_else(!is.na(AVALC_), ADT, as.Date(NA)),
           !!!set_values_to
    )%>%
    select(-AVALC_)
   
  # ----Keep only variables from dataset and remove all other adsl_var  ----
  rm_col<-colnames(dataset_adsl)[!(colnames(dataset_adsl) %in% colnames(dataset))]
  # ----Append new obs to input dataset  ----
  bind_rows(dataset,add_data%>% select(-all_of(rm_col)))

}
