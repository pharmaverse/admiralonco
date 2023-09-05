#' Pre-Defined Time-to-Event Source Objects
#'
#' These pre-defined `tte_source` objects can be used as input to `admiral::derive_param_tte()`.
#'
#' @details
#' To see the definition of the various objects simply print the object in the
#' R console, e.g. `print(death_event)`. For details of how to use these objects
#' please refer to [admiral::derive_param_tte()].
#'
#' Printing an object will display input dataset_name, filter (if applicable), date variable,
#' and appropriate values for `EVNTDESC`, `CNSDTDSC`, `SRCDOM`, `SRCVAR`, and `SRCSEQ`.
#'
#' @format NULL
#'
#' @seealso [admiral::derive_param_tte()], [admiral::tte_source()], [admiral::event_source()],
#'  [admiral::censor_source()]
#'
#' @export
#'
#' @family source_specifications
#' @keywords source_specifications
#'
#' @rdname tte_source_objects
#'
#' @examples
#' # This shows the definition of all pre-defined `tte_source` objects that ship
#' # with {admiralonco}
#' for (obj in admiral::list_tte_source_objects(package = "admiralonco")$object) {
#'   cat(obj, "\n")
#'   print(get(obj))
#'   cat("\n")
#' }
death_event <- event_source(
  dataset_name = "adrs",
  filter = PARAMCD == "DEATH" & AVALC == "Y" & ANL01FL == "Y",
  date = ADT,
  set_values_to = exprs(
    EVNTDESC = "Death",
    SRCDOM = "ADRS",
    SRCVAR = "ADT",
    SRCSEQ = ASEQ
  )
)

#' @format NULL
#' @keywords source_specifications
#' @rdname tte_source_objects
#' @export
lastalive_censor <- censor_source(
  dataset_name = "adsl",
  date = LSTALVDT,
  set_values_to = exprs(
    EVNTDESC = "Alive",
    CNSDTDSC = "Alive During Study",
    SRCDOM = "ADSL",
    SRCVAR = "LSTALVDT"
  )
)

#' @format NULL
#' @keywords source_specifications
#' @rdname tte_source_objects
#' @export
pd_event <- event_source(
  dataset_name = "adrs",
  filter = PARAMCD == "PD" & AVALC == "Y" & ANL01FL == "Y",
  date = ADT,
  set_values_to = exprs(
    EVNTDESC = "Disease Progression",
    SRCDOM = "ADRS",
    SRCVAR = "ADT",
    SRCSEQ = ASEQ
  )
)

#' @format NULL
#' @keywords source_specifications
#' @rdname tte_source_objects
#' @export
lasta_censor <- censor_source(
  dataset_name = "adrs",
  filter = PARAMCD == "LSTA" & ANL01FL == "Y",
  date = ADT,
  set_values_to = exprs(
    EVNTDESC = "Last Tumor Assessment",
    CNSDTDSC = "Last Tumor Assessment",
    SRCDOM = "ADRS",
    SRCVAR = "ADT",
    SRCSEQ = ASEQ
  )
)

#' @format NULL
#' @keywords source_specifications
#' @rdname tte_source_objects
#' @export
rand_censor <- censor_source(
  dataset_name = "adsl",
  date = RANDDT,
  set_values_to = exprs(
    EVNTDESC = "Randomization",
    CNSDTDSC = "Randomization",
    SRCDOM = "ADSL",
    SRCVAR = "RANDDT"
  )
)

#' @format NULL
#' @keywords source_specifications
#' @rdname tte_source_objects
#' @export
trts_censor <- censor_source(
  dataset_name = "adsl",
  date = TRTSDT,
  set_values_to = exprs(
    EVNTDESC = "Treatment Start",
    CNSDTDSC = "Treatment Start",
    SRCDOM = "ADSL",
    SRCVAR = "TRTSDT"
  )
)
