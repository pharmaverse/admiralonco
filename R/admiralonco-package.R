#' @keywords internal
#' @family internal
#' @importFrom admiral censor_source count_vals derive_extreme_records
#'   derive_var_obs_number derive_vars_merged event event_joined event_source
#'   filter_joined filter_extreme filter_relative get_admiral_option
#'   list_tte_source_objects max_cond min_cond params restrict_derivation
#'   set_admiral_options yn_to_numeric
#' @importFrom admiraldev assert_character_scalar assert_character_vector
#'   assert_data_frame assert_filter_cond assert_list_of assert_function
#'   assert_integer_scalar assert_list_of assert_logical_scalar
#'   assert_param_does_not_exist assert_s3_class assert_symbol assert_vars
#'   assert_varval_list enumerate expect_dfs_equal extract_vars filter_if expr_c
#'   process_set_values_to squote suppress_warning vars2chr
#' @importFrom dplyr arrange bind_rows case_when filter group_by if_else
#'   left_join mutate rename right_join select slice ungroup
#' @importFrom lifecycle deprecate_warn deprecated deprecate_stop
#' @importFrom lubridate days
#' @importFrom magrittr `%>%`
#' @importFrom rlang abort enexpr exprs inform warn
#' @importFrom tidyselect all_of
"_PACKAGE"
