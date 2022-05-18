#' Filter a Dataset to Only Include the Source Parameter Records up to and
#' including the first PD. These records are passed to downstream derivations
#' regarding responses such as BOR.
#'
#' @param dataset An input BDS style data set is expected
#'
#' @param filter_source A filter condition is expected, upon which the ADRS
#'        data set will be filtered to capture the PARAM containing the PD
#'        response
#'
#' @param source_pd The variable name which contains the dates of the responses
#'        A date/datetime object is expected.
#'
#' @param source_datasets A named list of data sets is expected.
#'        Default is ADRS.
#'
#' @return A subset data set of ADRS of records that only include
#'         the source_param records, keeping only those occurring
#'        up to and including the first PD,
#'         where the date comes from the ADT of
#'         the source_param/source_pd records.
#'
#' @author Teckla Akinyi
#'
#' @export
#'
#' @seealso [derive_vars_merged()], [filter_extreme()], [filter_if()]
#'
#' @keywords user_utility ADRS PD response
#'
#' @examples
#'adrs <- tibble::tribble(
#'~STUDYID, ~USUBJID, ~PARAMCD, ~AVALC, ~ADT,
#'"CDISCPILOT01", "01-701-1015", "OVR", "CR", "2016-01-25",
#'"CDISCPILOT01", "01-701-1015", "OVR", "PD", "2016-01-25",
#'"CDISCPILOT01", "01-701-1015", "BOR", "PD", "2016-02-21",
#'"CDISCPILOT01", "01-701-1034", "OVR", "SD", "2015-07-12",
#'"CDISCPILOT01", "01-701-1034", "OVR", "PD", "2016-04-25",
#'"CDISCPILOT01", "01-701-1034", "OVR", "PD", "2016-06-25",
#'"CDISCPILOT01", "01-701-1034", "BOR", "PD", "2016-05-25"
#') %>% dplyr::mutate(
#'  ADT = lubridate::as_date(ADT))
#'
#'adlb <- tibble::tribble(
#'  ~STUDYID, ~USUBJID, ~ADT,
#'  "CDISCPILOT01", "01-701-1015", "2014-02-13",
#'  "CDISCPILOT01", "01-701-1015", "2015-05-21",
#'  "CDISCPILOT01", "01-701-1015", "2015-12-21",
#'  "CDISCPILOT01", "01-701-1015", "2016-07-08",
#'  "CDISCPILOT01", "01-701-1015", "2016-11-25",
#'  "CDISCPILOT01", "01-701-1015", "2017-02-21",
#'  "CDISCPILOT01", "01-701-1034", "2015-04-03",
#'  "CDISCPILOT01", "01-701-1034", "2016-01-05",
#'  "CDISCPILOT01", "01-701-1034", "2016-04-02",
#'  "CDISCPILOT01", "01-701-1034", "2016-04-25",
#'  "CDISCPILOT01", "01-701-1034", "2016-06-25",
#'  "CDISCPILOT01", "01-701-1034", "2016-05-25"
#') %>% dplyr::mutate(
#'  ADT = lubridate::as_date(ADT))
#'
#'  filter_pd(dataset = adlb,
#'            filter_source = PARAMCD ==  "OVR" & AVALC == "PD",
#'            source_pd = ADT,
#'            source_dataset = adrs)
filter_pd <- function(dataset,
                      filter_source = PARAMCD == "OVR" & AVALC == "PD",
                      source_pd = ADT,
                      source_dataset = list(adrs = adrs)) {

  assert_data_frame(dataset,
                    required_vars = vars(STUDYID, USUBJID, ADT),
                    optional = FALSE)
  source_pd <- assert_symbol(enquo(source_pd))
  assert_data_frame(source_dataset,
                    required_vars = quo_c(source_pd,
                                          vars(STUDYID, USUBJID, AVALC)),
                                          optional = FALSE)
  filter_source <- assert_filter_cond(enquo(filter_source),
                                      optional = FALSE)

  #filter source dataset and choose first PD
  #based on condition specified by user
  source_data <- filter_if(source_dataset,
                           filter_source)
  source_data <- filter_extreme(source_data,
                                by_vars = vars(STUDYID, USUBJID),
                                order =  vars(!!source_pd),
                                mode = "FIRST")

  #merge filtered source data and compare dates
  #to keep only those up to source date
  derive_vars_merged(dataset = dataset,
                     dataset_add = source_data,
                     by_vars = vars(STUDYID, USUBJID),
                     order = vars(!!source_pd),
                     new_vars = vars(temp_ADT = !!source_pd)) %>%
                     filter(ADT <= temp_ADT) %>%
                     select(-temp_ADT)
}
