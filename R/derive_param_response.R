
#if (!requireNamespace("remotes", quietly = TRUE)) {
#  install.packages("remotes")
#}
#remotes::install_github("pharmaverse/admiral", ref = "devel")
#
#if (!requireNamespace("remotes", quietly = TRUE)) {
#  install.packages("remotes")
#}
#remotes::install_github("pharmaverse/admiraltest", ref = "main")

library(admiral)
library(rlang)
library(admiraltest)
library(dplyr)
devtools::load_all()

data("adsl")
data ("rs")
adrs<-rs %>%
  ungroup()

adsl<-adsl

date_source <- function(dataset_name,
                       filter = NULL,
                       date) {
  out <- list(
    dataset_name = assert_character_scalar(dataset_name),
    filter = assert_filter_cond(enquo(filter), optional = TRUE),
    date = assert_symbol(enquo(date))
  )
  class(out) <- c("date_source", "source", "list")
  out
}

response_source <- function(dataset_name,
                        filter = NULL) {
  out <- list(
    dataset_name = assert_character_scalar(dataset_name),
    filter = assert_filter_cond(enquo(filter), optional = TRUE)
  )
  class(out) <- c("response_source", "source", "list")
  out
}
pd <- date_source(
  dataset_name = "adrs",
  date = ADT,
  filter = PARAMCD == "PD" & AVALC == "Y"
)

response<-response_source(
  dataset_name = "rs", 
  filter =  RSTESTCD=="OVRLRESP" & RSSTRESC %in% c("CR", "PR")
)


derive_param_response<- function (
  dataset,
  dataset_adsl,
  #source_param,
  #source_end_date,
  source_datasets,
  source_response, 
  set_values_to,
  subject_keys = vars(STUDYID, USUBJID)
){
 
  # checking and quoting #
  assert_data_frame(dataset)
  assert_data_frame(dataset_adsl)
  assert_vars(subject_keys)
  #assert_list_of(source_end_date, "date_source")
  assert_list_of(source_response, "response_source")
  assert_list_of(source_datasets, "data.frame")
  source_names <- names(source_datasets)
  #assert_list_element(
  #  list = source_end_date,
  #  element = "dataset_name",
  #  condition = dataset_name %in% source_names,
  #  source_names = source_names,
  #  message_text = paste0(
  #    "The dataset names must be included in the list specified for the ",
  #    "`source_datasets` parameter.\n",
  #    "Following names were provided by `source_datasets`:\n",
  #    enumerate(source_names, quote_fun = squote)
  #  )
  #)
  assert_list_element(
    list = source_response,
    element = "dataset_name",
    condition = dataset_name %in% source_names,
    source_names = source_names,
    message_text = paste0(
      "The dataset names must be included in the list specified for the ",
      "`source_datasets` parameter.\n",
      "Following names were provided by `source_datasets`:\n",
      enumerate(source_names, quote_fun = squote)
    )
  )
    
  assert_varval_list(set_values_to, accept_expr = TRUE, optional = TRUE)
  if (!is.null(set_values_to$PARAMCD) & !is.null(dataset)) {
    assert_param_does_not_exist(dataset, quo_get_expr(set_values_to$PARAMCD))
  } 
}

test<-derive_param_response(
  dataset = adrs,
  dataset_adsl = adsl,
  #source_param = OVR,
  #source_pd = list(pd),
  source_response = response,
  source_datasets = list(adrs = adrs, rs=rs),
  #resp_cond = AVALC %in% c("CR", "PR") & ANL01FL="Y",
  set_values_to = vars(
    PARAMCD = "RSP",
    PARAM = " Response by Investigator (confirmation not required)")
)