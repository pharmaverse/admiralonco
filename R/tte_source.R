#' Pre-Defined Time-to-Event Source Objects
#'
#' These pre-defined `tte_source` objects can be used as input to `derive_param_tte()`.
#'
#' @details
#' To see the definition of the various objects simply print the object in the
#' R console, e.g. `print(death_event)`.
#'
#' @seealso [derive_param_tte], [tte_source], [event_source], [censor_source]
#'
#' @export
#'
#' @keywords tte_source
#'
#' @rdname tte_source_objects
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


#' @keywords tte_source
#' @rdname tte_source_objects
#' @export
ae_event <- event_source(
  dataset_name = "adae",
  filter = TRTEMFL == "Y",
  date = ASTDT,
  set_values_to = vars(
    EVNTDESC = "ADVERSE EVENT",
    SRCDOM = "ADAE",
    SRCVAR = "ASTDT",
    SRCSEQ = AESEQ
  )
)

#' @keywords tte_source
#' @rdname tte_source_objects
#' @export
ae_ser_event <- event_source(
  dataset_name = "adae",
  filter = TRTEMFL == "Y" & AESER == "Y",
  date = ASTDT,
  set_values_to = vars(
    EVNTDESC = "SERIOUS ADVERSE EVENT",
    SRCDOM = "ADAE",
    SRCVAR = "ASTDT",
    SRCSEQ = AESEQ
  )
)

#' @keywords tte_source
#' @rdname tte_source_objects
#' @export
ae_gr1_event <- event_source(
  dataset_name = "adae",
  filter = TRTEMFL == "Y" & ATOXGR == "1",
  date = ASTDT,
  set_values_to = vars(
    EVNTDESC = "GRADE 1 ADVERSE EVENT",
    SRCDOM = "ADAE",
    SRCVAR = "ASTDT",
    SRCSEQ = AESEQ
  )
)

#' @keywords tte_source
#' @rdname tte_source_objects
#' @export
ae_gr2_event <- event_source(
  dataset_name = "adae",
  filter = TRTEMFL == "Y" & ATOXGR == "2",
  date = ASTDT,
  set_values_to = vars(
    EVNTDESC = "GRADE 2 ADVERSE EVENT",
    SRCDOM = "ADAE",
    SRCVAR = "ASTDT",
    SRCSEQ = AESEQ
  )
)

#' @keywords tte_source
#' @rdname tte_source_objects
#' @export
ae_gr3_event <- event_source(
  dataset_name = "adae",
  filter = TRTEMFL == "Y" & ATOXGR == "3",
  date = ASTDT,
  set_values_to = vars(
    EVNTDESC = "GRADE 3 ADVERSE EVENT",
    SRCDOM = "ADAE",
    SRCVAR = "ASTDT",
    SRCSEQ = AESEQ
  )
)

#' @keywords tte_source
#' @rdname tte_source_objects
#' @export
ae_gr4_event <- event_source(
  dataset_name = "adae",
  filter = TRTEMFL == "Y" & ATOXGR == "4",
  date = ASTDT,
  set_values_to = vars(
    EVNTDESC = "GRADE 4 ADVERSE EVENT",
    SRCDOM = "ADAE",
    SRCVAR = "ASTDT",
    SRCSEQ = AESEQ
  )
)

#' @keywords tte_source
#' @rdname tte_source_objects
#' @export
ae_gr5_event <- event_source(
  dataset_name = "adae",
  filter = TRTEMFL == "Y" & ATOXGR == "5",
  date = ASTDT,
  set_values_to = vars(
    EVNTDESC = "GRADE 5 ADVERSE EVENT",
    SRCDOM = "ADAE",
    SRCVAR = "ASTDT",
    SRCSEQ = AESEQ
  )
)

#' @keywords tte_source
#' @rdname tte_source_objects
#' @export
ae_gr35_event <- event_source(
  dataset_name = "adae",
  filter = TRTEMFL == "Y" & ATOXGR %in% c("3", "4", "5"),
  date = ASTDT,
  set_values_to = vars(
    EVNTDESC = "GRADE 3-5 ADVERSE EVENT",
    SRCDOM = "ADAE",
    SRCVAR = "ASTDT",
    SRCSEQ = AESEQ
  )
)

#' @keywords tte_source
#' @rdname tte_source_objects
#' @export
ae_sev_event <- event_source(
  dataset_name = "adae",
  filter = TRTEMFL == "Y" & AESEV == "SEVERE",
  date = ASTDT,
  set_values_to = vars(
    EVNTDESC = "SEVERE ADVERSE EVENT",
    SRCDOM = "ADAE",
    SRCVAR = "ASTDT",
    SRCSEQ = AESEQ
  )
)

#' @keywords tte_source
#' @rdname tte_source_objects
#' @export
ae_wd_event <- event_source(
  dataset_name = "adae",
  filter = TRTEMFL == "Y" & AEACN == "DRUG WITHDRAWN",
  date = ASTDT,
  set_values_to = vars(
    EVNTDESC = "ADVERSE EVENT LEADING TO DRUG WITHDRAWAL",
    SRCDOM = "ADAE",
    SRCVAR = "ASTDT",
    SRCSEQ = AESEQ
  )
)
