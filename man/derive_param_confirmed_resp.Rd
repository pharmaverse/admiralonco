% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/derive_param_confirmed_resp.R
\name{derive_param_confirmed_resp}
\alias{derive_param_confirmed_resp}
\title{Adds a Parameter for Confirmed Response}
\usage{
derive_param_confirmed_resp(
  dataset,
  dataset_adsl,
  filter_source,
  source_pd = NULL,
  source_datasets = NULL,
  ref_confirm,
  max_nr_ne = 1,
  accept_sd = FALSE,
  aval_fun,
  set_values_to,
  subject_keys = get_admiral_option("subject_keys")
)
}
\arguments{
\item{dataset}{Input dataset

The \code{PARAMCD}, \code{ADT}, and \code{AVALC} variables and the variables specified by
\code{subject_keys} and \code{reference_date} are expected.

After applying \code{filter_source} and/or \code{source_pd} the variable \code{ADT} and the
variables specified by \code{subject_keys} must be a unique key of the dataset.}

\item{dataset_adsl}{ADSL input dataset

The variables specified for \code{subject_keys} are expected. For each subject
of the specified dataset a new observation is added to the input dataset.}

\item{filter_source}{Source filter

All observations in \code{dataset_source} fulfilling the specified condition are
considered for deriving the confirmed response.}

\item{source_pd}{Date of first progressive disease (PD)

If the parameter is specified, the observations of the input dataset for
deriving the new parameter are restricted to observations up to the
specified date. Observations at the specified date are included. For
subjects without first PD date all observations are take into account.

\emph{Permitted Values:} a \code{date_source} object (see \code{admiral::date_source()}
for details)}

\item{source_datasets}{Source dataset for the first PD date

A named list of datasets is expected. It links the \code{dataset_name} from
\code{source_pd} with an existing dataset.

For example if \code{source_pd = pd_date} with

\if{html}{\out{<div class="sourceCode r">}}\preformatted{pd_date <- date_source(
  dataset_name = "adrs",
  date = ADT,
  filter = PARAMCD == PD
)
}\if{html}{\out{</div>}}

and the actual response dataset in the script is \code{myadrs}, \code{source_datasets = list(adrs = myadrs)} should be specified.}

\item{ref_confirm}{Minimum time period for confirmation

The assessment and the confirmatory assessment for \code{"CR"} and \code{"PR"} have
to be at least the specified number of days apart.}

\item{max_nr_ne}{The specified number of \code{"NE"} assessments between the assessment and the
confirmatory assessment for \code{"CR"} and \code{"PR"} response is accepted.

\emph{Permitted Values:} a non-negative numeric scalar}

\item{accept_sd}{Accept \code{"SD"} for \code{"PR"}?

If the argument is set to \code{TRUE}, one \code{"SD"} assessment between the
assessment and the confirmatory assessment for \code{"PR"} response is accepted.
Otherwise, no \code{"SD"} assessment must occur between the two assessments.

\emph{Permitted Values:} a logical scalar}

\item{aval_fun}{\emph{Deprecated}, please use \code{set_values_to} instead.

Function to map character analysis value (\code{AVALC}) to numeric analysis
value (\code{AVAL})

The (first) argument of the function must expect a character vector and the
function must return a numeric vector.}

\item{set_values_to}{Variables to set

A named list returned by \code{exprs()} defining the variables to be set for the
new parameter, e.g. \code{exprs(PARAMCD = "CRSP", PARAM = "Confirmed Response")}
is expected. The values must be symbols, character strings, numeric values,
or \code{NA}.}

\item{subject_keys}{Variables to uniquely identify a subject

A list of symbols created using \code{exprs()} is expected.}
}
\value{
The input dataset with a new parameter for confirmed response
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#superseded}{\figure{lifecycle-superseded.svg}{options: alt='[Superseded]'}}}{\strong{[Superseded]}} The \code{derive_param_confirmed_resp()}
function has been superseded in favor of \code{derive_extreme_event()}.

Adds a parameter for confirmed response
}
\details{
\enumerate{
\item The input dataset (\code{dataset}) is restricted to the observations matching
\code{filter_source} and to observations before or at the date specified by
\code{source_pd}.

\item A subject is considered as responder if there is at least one
observation in the restricted dataset with

\itemize{
\item \code{AVALC == "CR"},
\item there is a confirmatory assessment with \code{AVALC == "CR"} at least
\code{ref_confirm} days after the assessment,
\item all assessments between the assessment and the confirmatory
assessment are \code{"CR"} or \code{"NE"}, and
\item there are at most \code{max_nr_ne} \code{"NE"} assessments between the
assessment and the confirmatory assessment.}

or at least one observation with

\itemize{
\item \code{AVALC == "PR"},
\item there is a confirmatory assessment with \code{AVALC \%in\% c("CR", "PR")} at
least \code{ref_confirm} days after the assessment,
\item all assessments between the assessment and the confirmatory
assessment are \code{"CR"}, \code{"PR"}, \code{"SD"}, or \code{"NE"},
\item there is no \code{"PR"} assessment after a \code{"CR"} assessment in the
confirmation period,
\item there are at most \code{max_nr_ne} \code{"NE"} assessments between the
assessment and the confirmatory assessment,
\item if the \code{accept_sd} argument is set to \code{TRUE}, one \code{"SD"} assessment
in the confirmation period is accepted. Otherwise, no \code{"SD"} assessment
must occur within the confirmation period.
}

\item For responders \code{AVALC} is set to \code{"Y"} and \code{ADT} to the first date where
the response criteria are fulfilled. For all other subjects in
\code{dataset_adsl} \code{AVALC} is set to \code{"N"} and \code{ADT} to \code{NA}.

\item The \code{AVAL} variable is added and set to \code{aval_fun(AVALC)}.

\item The variables specified by the \code{set_values_to} parameter are added to
the new observations.

\item The new observations are added to input dataset.
}
}
\examples{

library(dplyr)
library(admiral)

# Create ADSL dataset
adsl <- tibble::tribble(
  ~USUBJID, ~TRTSDTC,
  "1",      "2020-01-01",
  "2",      "2019-12-12",
  "3",      "2019-11-11",
  "4",      "2019-12-30",
  "5",      "2020-01-01",
  "6",      "2020-02-02",
  "7",      "2020-02-02",
  "8",      "2020-04-01",
  "9",      "2020-03-01"
) \%>\%
  mutate(
    STUDYID = "XX1234"
  )

# Create ADRS dataset
ovr_obs <- tibble::tribble(
  ~USUBJID, ~ADTC,        ~AVALC,
  "1",      "2020-01-01", "PR",
  "1",      "2020-02-01", "CR",
  "1",      "2020-02-16", "NE",
  "1",      "2020-03-01", "CR",
  "1",      "2020-04-01", "SD",
  "2",      "2020-01-01", "SD",
  "2",      "2020-02-01", "PR",
  "2",      "2020-03-01", "SD",
  "2",      "2020-03-13", "CR",
  "3",      "2019-11-12", "CR",
  "3",      "2019-12-02", "CR",
  "3",      "2020-01-01", "SD",
  "4",      "2020-01-01", "PR",
  "4",      "2020-03-01", "SD",
  "4",      "2020-04-01", "SD",
  "4",      "2020-05-01", "PR",
  "4",      "2020-05-15", "NON-CR/NON-PD",
  "5",      "2020-01-01", "PR",
  "5",      "2020-01-10", "SD",
  "5",      "2020-01-20", "PR",
  "5",      "2020-05-15", "NON-CR/NON-PD",
  "6",      "2020-02-06", "PR",
  "6",      "2020-02-16", "CR",
  "6",      "2020-03-30", "PR",
  "6",      "2020-04-12", "PD",
  "6",      "2020-05-01", "CR",
  "6",      "2020-06-01", "CR",
  "7",      "2020-02-06", "PR",
  "7",      "2020-02-16", "CR",
  "7",      "2020-04-01", "NE",
  "9",      "2020-03-16", "CR",
  "9",      "2020-04-01", "NE",
  "9",      "2020-04-16", "NE",
  "9",      "2020-05-01", "CR"
) \%>\%
  mutate(PARAMCD = "OVR", ANL01FL = "Y")

pd_obs <-
  bind_rows(tibble::tribble(
    ~USUBJID, ~ADTC,        ~AVALC,
    "6",      "2020-04-12", "Y"
  ) \%>\%
    mutate(PARAMCD = "PD", ANL01FL = "Y"))

adrs <- bind_rows(ovr_obs, pd_obs) \%>\%
  mutate(
    ADT = lubridate::ymd(ADTC),
    STUDYID = "XX1234"
  ) \%>\%
  select(-ADTC)

pd_date <- date_source(
  dataset_name = "adrs",
  date = ADT,
  filter = PARAMCD == "PD" & ANL01FL == "Y"
)

# Derive confirmed response parameter
derive_param_confirmed_resp(
  adrs,
  dataset_adsl = adsl,
  filter_source = PARAMCD == "OVR" & ANL01FL == "Y",
  source_pd = pd_date,
  source_datasets = list(adrs = adrs),
  ref_confirm = 28,
  set_values_to = exprs(
    PARAMCD = "CRSP",
    PARAM = "Confirmed Response by Investigator"
  )
) \%>\%
  filter(PARAMCD == "CRSP")

# Derive confirmed response parameter (accepting SD for PR and two NEs)
derive_param_confirmed_resp(
  adrs,
  dataset_adsl = adsl,
  filter_source = PARAMCD == "OVR" & ANL01FL == "Y",
  source_pd = pd_date,
  source_datasets = list(adrs = adrs),
  ref_confirm = 28,
  max_nr_ne = 2,
  accept_sd = TRUE,
  set_values_to = exprs(
    PARAMCD = "CRSP",
    PARAM = "Confirmed Response by Investigator"
  )
) \%>\%
  filter(PARAMCD == "CRSP")
}
\seealso{
Other superseded: 
\code{\link{derive_param_bor}()},
\code{\link{derive_param_clinbenefit}()},
\code{\link{derive_param_confirmed_bor}()},
\code{\link{derive_param_response}()},
\code{\link{filter_pd}()}
}
\author{
Stefan Bundfuss
}
\concept{superseded}
\keyword{superseded}
