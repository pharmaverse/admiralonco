#' Pre-Defined Response Event Objects
#'
#' These pre-defined `event()` and `event_joined()` objects can be used as input
#' to `admiral::derive_extreme_event()`.
#'
#' @details
#' To see the definition of the various objects simply print the object in the
#' R console, e.g. `bor_sd`. For details of how to use these objects
#' please refer to [admiral::derive_extreme_event()].
#'
#' It is assumed that `dataset_name = "ovr"` refers to the dataset of the only
#' overall response assessments at each visit which should be considered for the
#' parameter derivations. For example the dataset should include only
#' post-baseline assessments up to first PD and before start of anti-cancer
#' therapy.
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
rsp_y <- event(
  description = "Define CR or PR as (unconfirmed) response",
  dataset_name = "ovr",
  condition = AVALC %in% c("CR", "PR"),
  set_values_to = exprs(AVALC = "Y")
)

#' @format NULL
#' @keywords source_specifications
#' @rdname event_objects
#' @export
no_data_n <- event(
  description = "Define no response for all patients in adsl (should be used as last event)",
  dataset_name = "adsl",
  condition = TRUE,
  set_values_to = exprs(AVALC = "N"),
  keep_source_vars = exprs(RANDDT)
)

#' @format NULL
#' @keywords source_specifications
#' @rdname event_objects
#' @export
cb_y <- event(
  description = paste(
    "Define CR, PR, SD, or NON-CR/NON-PD occuring at least 42 days after",
    "randomization as clinical benefit"
  ),
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
  description = "Define complete response (CR) for best overall response (BOR)",
  dataset_name = "ovr",
  condition = AVALC == "CR",
  set_values_to = exprs(AVALC = "CR")
)

#' @format NULL
#' @keywords source_specifications
#' @rdname event_objects
#' @export
bor_pr <- event(
  description = "Define partial response (PR) for best overall response (BOR)",
  dataset_name = "ovr",
  condition = AVALC == "PR",
  set_values_to = exprs(AVALC = "PR")
)

#' @format NULL
#' @keywords source_specifications
#' @rdname event_objects
#' @export
bor_sd <- event(
  description = paste(
    "Define stable disease (SD) for best overall respone (BOR) as CR, PR, or SD",
    "occurring at least 42 days after randomization"
  ),
  dataset_name = "ovr",
  condition = AVALC %in% c("CR", "PR", "SD") & ADT >= RANDDT + 42,
  set_values_to = exprs(AVALC = "SD")
)

#' @format NULL
#' @keywords source_specifications
#' @rdname event_objects
#' @export
bor_non_crpd <- event(
  description = paste(
    "Define NON-CR/NON-PD for best overall response (BOR) as NON-CR/NON-PD",
    "occuring at least 42 days after randomization"
  ),
  dataset_name = "ovr",
  condition = AVALC == "NON-CR/NON-PD" & ADT >= RANDDT + 42,
  set_values_to = exprs(AVALC = "NON-CR/NON-PD")
)

#' @format NULL
#' @keywords source_specifications
#' @rdname event_objects
#' @export
bor_pd <- event(
  description = "Define progressive disease (PD) for best overall response (BOR)",
  dataset_name = "ovr",
  condition = AVALC == "PD",
  set_values_to = exprs(AVALC = "PD")
)

#' @format NULL
#' @keywords source_specifications
#' @rdname event_objects
#' @export
bor_ne <- event(
  description = paste(
    "Define not evaluable (NE) for best overall response (BOR) as CR, PR, SD,",
    "NON-CR/NON-PD, or NE (should be specified after bor_sd and bor_non_crpd)"
  ),
  dataset_name = "ovr",
  condition = AVALC %in% c("CR", "PR", "SD", "NON-CR/NON-PD", "NE"),
  set_values_to = exprs(AVALC = "NE")
)

#' @format NULL
#' @keywords source_specifications
#' @rdname event_objects
#' @export
no_data_missing <- event(
  description = paste(
    "Define missing response (MISSING) for all subjects in adsl in your population",
    "who has no post baseline response (should be used as last event)"
  ),
  dataset_name = "adsl",
  condition = TRUE,
  set_values_to = exprs(AVALC = "MISSING"),
  keep_source_vars = exprs(RANDDT)
)

#' @format NULL
#' @keywords source_specifications
#' @rdname event_objects
#' @export
crsp_y_cr <- event_joined(
  description = paste(
    "Define confirmed response as CR followed by CR at least 28 days later and",
    "at most one NE in between"
  ),
  dataset_name = "ovr",
  join_vars = exprs(AVALC, ADT),
  join_type = "after",
  order = exprs(ADT),
  first_cond_upper = AVALC.join == "CR" &
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
  description = paste(
    "Define confirmed response as PR followed by CR or PR at least 28 days later,",
    "at most one NE in between, and no PR after CR"
  ),
  dataset_name = "ovr",
  join_vars = exprs(AVALC, ADT),
  join_type = "after",
  order = exprs(ADT),
  first_cond_upper = AVALC.join %in% c("CR", "PR") &
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
  description = paste(
    "Define complete response (CR) for confirmed best overall response (CBOR) as",
    "CR followed by CR at least 28 days later and at most one NE in between"
  ),
  dataset_name = "ovr",
  join_vars = exprs(AVALC, ADT),
  join_type = "after",
  first_cond_upper = AVALC.join == "CR" &
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
  description = paste(
    "Define partial response (PR) for confirmed best overall response (CBOR) as",
    "PR followed by CR or PR at least 28 days later, at most one NE in between,",
    "and no PR after CR"
  ),
  dataset_name = "ovr",
  join_vars = exprs(AVALC, ADT),
  join_type = "after",
  first_cond_upper = AVALC.join %in% c("CR", "PR") &
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
