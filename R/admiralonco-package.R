#' @keywords internal
#' @family internal
#' @importFrom admiral censor_source count_vals derive_param_first_event
#'   derive_var_obs_number derive_vars_merged event_source filter_confirmation
#'   filter_extreme filter_relative list_tte_source_objects max_cond min_cond
#'   yn_to_numeric
#' @importFrom admiraldev assert_character_scalar assert_character_vector
#'   assert_data_frame assert_filter_cond assert_list_of assert_function
#'   assert_integer_scalar assert_list_of assert_logical_scalar
#'   assert_order_vars assert_param_does_not_exist assert_s3_class assert_symbol
#'   assert_vars assert_varval_list enumerate expect_dfs_equal extract_vars
#'   filter_if get_dataset quo_c squote set_dataset suppress_warning vars2chr
#' @importFrom dplyr arrange bind_rows case_when filter group_by if_else
#'   left_join mutate rename right_join select slice vars ungroup
#' @importFrom lubridate days
#' @importFrom magrittr `%>%`
#' @importFrom rlang abort enquo inform quo_get_expr quo_is_null warn
#' @importFrom tidyselect all_of
"_PACKAGE"
