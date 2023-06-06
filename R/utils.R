#' Creates `AVAL` from `AVALC` by Calling User Function
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function is *deprecated*, please use
#' `admiraldev::process_set_values_to()` instead.
#'
#' Create `AVAL` from `AVALC` by calling a function provided by the user. If
#' calling the function fails, the error is caught and a helpful error message
#' provided.
#'
#' @param dataset Input dataset
#'
#'   The variable `AVALC` is expected.
#'
#'   *Permitted Values*: a dataframe
#'
#' @param aval_fun Function returning the `AVALC` values
#'
#'   The specified function must expect one argument expecting a character
#'   vector and must return a numeric vector.
#'
#'   *Permitted Values*: a function
#'
#' @details The new variable `AVAL` is set to `aval_fun(AVALC)`.
#'
#' @author Stefan Bundfuss
#'
#' @returns The input dataset with `AVAL` added
#'
#' @keywords deprecated
#' @family deprecated
#'
#' @export
call_aval_fun <- function(dataset,
                          aval_fun) {
  assert_data_frame(dataset, required_vars = exprs(AVALC))
  assert_function(aval_fun)

  deprecate_warn("0.4.0", "call_aval_fun()", "admiraldev::process_set_values_to()")

  tryCatch(
    mutate(
      dataset,
      AVAL = aval_fun(AVALC)
    ),
    error = function(cnd) {
      abort(
        paste0(
          "Assigning new AVAL records with aval_fun (`AVAL = aval_fun(AVALC)`) has failed.\n",
          "Error message:\n  ",
          cnd
        )
      )
    }
  )
}
