#' @keywords internal
#' @family internal
#' @importFrom admiral censor_source count_vals derive_extreme_records
#' @importFrom admiral derive_var_obs_number derive_vars_merged event
#' @importFrom admiral event_joined event_source filter_joined filter_extreme
#' @importFrom admiral filter_relative get_admiral_option list_tte_source_objects
#' @importFrom admiral max_cond min_cond params restrict_derivation
#' @importFrom admiral set_admiral_options yn_to_numeric
#'
#' @importFrom admiraldev assert_character_scalar assert_character_vector
#' @importFrom admiraldev assert_data_frame assert_expr assert_expr_list
#' @importFrom admiraldev assert_filter_cond assert_list_of assert_function
#' @importFrom admiraldev assert_integer_scalar assert_list_of
#' @importFrom admiraldev assert_logical_scalar assert_param_does_not_exist
#' @importFrom admiraldev assert_s3_class assert_symbol assert_vars
#' @importFrom admiraldev assert_varval_list deprecate_inform enumerate
#' @importFrom admiraldev expect_dfs_equal extract_vars filter_if expr_c
#' @importFrom admiraldev process_set_values_to squote suppress_warning vars2chr
#'
#' @importFrom cli cli_abort
#'
#' @importFrom dplyr arrange bind_rows case_when filter group_by if_else
#' @importFrom dplyr left_join mutate rename right_join select slice ungroup
#'
#' @importFrom lifecycle deprecate_warn deprecated deprecate_stop
#'
#' @importFrom lubridate days
#'
#' @importFrom magrittr `%>%`
#'
#' @importFrom rlang abort enexpr exprs inform warn
#'
#' @importFrom tidyselect all_of
"_PACKAGE"
