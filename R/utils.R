#' Creates `AVAL` from `AVALC` by Calling User Function
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
#' @keywords utils_catch
#' @family utils_catch
#'
#' @export
#'
#' @examples
#'
#' library(tibble)
#' library(dplyr)
#'
#' data <- tribble(
#'   ~AVALC,
#'   "YES",
#'   "NO"
#' )
#'
#' yn_map <- function(x) {
#'   case_when(
#'     x == "YES" ~ 1,
#'     x == "NO" ~ 0
#'   )
#' }
#'
#' call_aval_fun(
#'   data,
#'   yn_map
#' )
call_aval_fun <- function(dataset,
                          aval_fun) {
  assert_data_frame(dataset, required_vars = vars(AVALC))
  assert_function(aval_fun)

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
