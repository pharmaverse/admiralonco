#' Pre-Defined Time-to-Event Source Objects
#'
#' These pre-defined `onco_tte_source` objects can be used as input to `derive_param_tte()`.
#'
#' @details
#' To see the definition of the various objects simply print the object in the
#' R console, e.g. `print(death_event)`.
#'
#' @seealso [derive_param_tte()], [tte_source()], [event_source()], [censor_source()]
#'
#' @export
#'
#' @keywords onco_tte_source
#'
#' @rdname onco_tte_source_objects
#'
death_event <- event_source(
  dataset_name = "adrs",
  filter = PARAMCD == "DEATH" & AVALC == "Y" & ANL01FL == "Y",
  date = ADT,
  set_values_to = vars(
    EVNTDESC = "DEATH",
    CNSDTDSC = "",
    SRCDOM = "ADRS",
    SRCVAR = "ADT",
    SRCSEQ = ASEQ
  )
)

#' @keywords tte_source
#' @rdname tte_source_objects
#' @export
lastalive_censor <- censor_source(
  dataset_name = "adsl",
  date = LSTALVDT,
  set_values_to = vars(
    EVNTDESC = "ALIVE",
    CNSDTDSC = "Alive During Study",
    SRCDOM = "ADSL",
    SRCVAR = "LSTALVDT",
    SRCSEQ =
  )
)

#' @keywords tte_source
#' @rdname tte_source_objects
#' @export
pd_event <- event_source(
  dataset_name = "adrs",
  filter = PARAMCD == "PD" & AVALC == "Y" & ANL01FL == "Y",
  date = ADT,
  set_values_to = vars(
    EVNTDESC = "Disease Progression",
    CNSDTDSC = ,
    SRCDOM = "ADRS",
    SRCVAR = "ADT",
    SRCSEQ = ASEQ
  )
)

#' @keywords tte_source
#' @rdname tte_source_objects
#' @export
lasta_censor <- censor_source(
  dataset_name = "adrs",
  filter = PARAMCD == "LSTA" & ANL01FL == "Y",
  date = ADT,
  set_values_to = vars(
    EVNTDESC = "Last TUmor Assessment",
    CNSDTDSC = "Last Tumor Assessment",
    SRCDOM = "ADRS",
    SRCVAR = "ADT",
    SRCSEQ = ASEQ
  )
)

#' @keywords tte_source
#' @rdname tte_source_objects
#' @export
rand_censor <- censor_source(
  dataset_name = "adsl",
  date = RANDDT,
  set_values_to = vars(
    EVNTDESC = "Randomization",
    CNSDTDSC = "Randomization",
    SRCDOM = "ADSL",
    SRCVAR = "RANDDT",
    SRCSEQ =
  )
)

#' @keywords tte_source
#' @rdname tte_source_objects
#' @export
trts_censor <- censor_source(
  dataset_name = "adsl",
  date = TRTSDT,
  set_values_to = vars(
    EVNTDESC = "Treatment Start",
    CNSDTDSC = "Treatment Start",
    SRCDOM = "ADSL",
    SRCVAR = "TRTSDT",
    SRCSEQ =
  )
)





