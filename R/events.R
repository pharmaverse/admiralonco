#' Pre-Defined Event Objects
#'
#' These pre-defined `event()` and `event_joined()` objects can be used as input
#' to `admiral::derive_extreme_event()`.
#'
#' @details
#' To see the definition of the various objects simply print the object in the
#' R console, e.g. `bor_sd`. For details of how to use these objects
#' please refer to [admiral::derive_extreme_event()].
#'
#' It is assumed that `dataset_name = "ovr"` refers to the dataset of the
#' overall response assessments which should be considered for the parameter
#' derivations. For example the dataset should not include assessment after PD
#' or start of anti-cancer therapy.
#'
#' @seealso [admiral::derive_extreme_event()], [admiral::event()], [admiral::event_joined()]
#'
#' @format NULL
#'
#' @export
#'
#' @keywords source_specifications
#'
#' @rdname event_objects
#'
#' @examples
#' # This shows the definition of all pre-defined `event` objects that ship
#' # with {admiralonco}
#' exports <- sort(getNamespaceExports("admiralonco"))
#' for (obj_name in exports) {
#'   obj <- getExportedValue("admiralonco", obj_name)
#'   if (inherits(obj, "event_def")) {
#'     cat("\n", obj_name, ":\n", sep = "")
#'     print(obj, indent = 2)
#'   }
#' }
response_y <- event(
  dataset_name = "ovr",
  condition = AVALC %in% c("CR", "PR"),
  set_values_to = exprs(AVALC = "Y")
)

#' @format NULL
#' @keywords source_specifications
#' @rdname event_objects
#' @export
no_data_n <- event(
  dataset_name = "adsl",
  condition = TRUE,
  set_values_to = exprs(AVALC = "N"),
  keep_vars_source = exprs(RANDDT)
)

#' @format NULL
#' @keywords source_specifications
#' @rdname event_objects
#' @export
cb_y <- event(
  dataset_name = "ovr",
  condition = AVALC %in% c("CR", "PR", "SD", "NON-CR/NON-PD") &
    ADT >= RANDDT + 42,
  set_values_to = exprs(AVALC = "Y")
)

#' @format NULL
#' @keywords source_specifications
#' @rdname event_objects
#' @export
bor_cr <- event(
  dataset_name = "ovr",
  condition = AVALC == "CR",
  set_values_to = exprs(
    AVALC = "CR"
  )
)

#' @format NULL
#' @keywords source_specifications
#' @rdname event_objects
#' @export
bor_pr <- event(
  dataset_name = "ovr",
  condition = AVALC == "PR",
  set_values_to = exprs(
    AVALC = "PR"
  )
)

#' @format NULL
#' @keywords source_specifications
#' @rdname event_objects
#' @export
bor_sd <- event(
  dataset_name = "ovr",
  condition = AVALC %in% c("CR", "PR", "SD") & ADT >= RANDDT + 42,
  set_values_to = exprs(
    AVALC = "SD"
  )
)

#' @format NULL
#' @keywords source_specifications
#' @rdname event_objects
#' @export
bor_non_crpd <- event(
  dataset_name = "ovr",
  condition = AVALC == "NON-CR/NON-PD" & ADT >= RANDDT + 42,
  set_values_to = exprs(
    AVALC = "NON-CR/NON-PD"
  )
)

#' @format NULL
#' @keywords source_specifications
#' @rdname event_objects
#' @export
bor_pd <- event(
  dataset_name = "ovr",
  condition = AVALC == "PD",
  set_values_to = exprs(
    AVALC = "PD"
  )
)

#' @format NULL
#' @keywords source_specifications
#' @rdname event_objects
#' @export
bor_ne <- event(
  dataset_name = "ovr",
  condition = AVALC %in% c("SD", "NON-CR/NON-PD", "NE"),
  set_values_to = exprs(
    AVALC = "NE"
  )
)

#' @format NULL
#' @keywords source_specifications
#' @rdname event_objects
#' @export
no_data_missing <- event(
  dataset_name = "adsl",
  condition = TRUE,
  set_values_to = exprs(
    AVALC = "MISSING"
  ),
  keep_vars_source = exprs(RANDDT)
)

#' @format NULL
#' @keywords source_specifications
#' @rdname event_objects
#' @export
crsp_y_cr <- event_joined(
  dataset_name = "ovr",
  join_vars = exprs(AVALC, ADT),
  join_type = "after",
  order = exprs(ADT),
  first_cond = AVALC.join == "CR" &
    ADT.join >= ADT + days(28),
  condition = AVALC == "CR" &
    all(AVALC.join %in% c("CR", "NE")) &
    count_vals(var = AVALC.join, val = "NE") <= 1,
  set_values_to = exprs(AVALC = "Y")
)

#' @format NULL
#' @keywords source_specifications
#' @rdname event_objects
#' @export
crsp_y_pr <- event_joined(
  dataset_name = "ovr",
  join_vars = exprs(AVALC, ADT),
  join_type = "after",
  order = exprs(ADT),
  first_cond = AVALC.join %in% c("CR", "PR") &
    ADT.join >= ADT + days(28),
  condition = AVALC == "PR" &
    all(AVALC.join %in% c("CR", "PR", "NE")) &
    count_vals(var = AVALC.join, val = "NE") <= 1 &
    (
      min_cond(
        var = ADT.join,
        cond = AVALC.join == "CR"
      ) > max_cond(var = ADT.join, cond = AVALC.join == "PR") |
        count_vals(var = AVALC.join, val = "CR") == 0 |
        count_vals(var = AVALC.join, val = "PR") == 0
    ),
  set_values_to = exprs(AVALC = "Y")
)

#' @format NULL
#' @keywords source_specifications
#' @rdname event_objects
#' @export
cbor_cr <- event_joined(
  dataset_name = "ovr",
  join_vars = exprs(AVALC, ADT),
  join_type = "after",
  first_cond = AVALC.join == "CR" &
    ADT.join >= ADT + 28,
  condition = AVALC == "CR" &
    all(AVALC.join %in% c("CR", "NE")) &
    count_vals(var = AVALC.join, val = "NE") <= 1,
  set_values_to = exprs(AVALC = "CR")
)

#' @format NULL
#' @keywords source_specifications
#' @rdname event_objects
#' @export
cbor_pr <- event_joined(
  dataset_name = "ovr",
  join_vars = exprs(AVALC, ADT),
  join_type = "after",
  first_cond = AVALC.join %in% c("CR", "PR") &
    ADT.join >= ADT + 28,
  condition = AVALC == "PR" &
    all(AVALC.join %in% c("CR", "PR", "NE")) &
    count_vals(var = AVALC.join, val = "NE") <= 1 &
    (
      min_cond(
        var = ADT.join,
        cond = AVALC.join == "CR"
      ) > max_cond(var = ADT.join, cond = AVALC.join == "PR") |
        count_vals(var = AVALC.join, val = "CR") == 0 |
        count_vals(var = AVALC.join, val = "PR") == 0
    ),
  set_values_to = exprs(AVALC = "PR")
)
