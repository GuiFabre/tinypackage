% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/05-summary_functions.R
\name{study_assessement_report}
\alias{study_assessement_report}
\title{xxx xxx xxx}
\usage{
study_assessement_report(
  study = NULL,
  dataset = NULL,
  data_dict = NULL,
  taxonomy = NULL
)
}
\arguments{
\item{study}{xxx xxx xxx}

\item{dataset}{xxx xxx xxx}

\item{data_dict}{xxx xxx xxx}

\item{taxonomy}{xxx xxx xxx}
}
\value{
xxx xxx xxx.
}
\description{
xxx xxx xxx.
}
\examples{
\dontrun{
# Example 1: xxx xxx xxx.


dataset   <- study_TOKYO \%>\% mutate(dob = as_any_date(dob, format = "mdy"))
data_dict <- dd_TOKYO_format_maelstrom_tagged

study_assessement_report(dataset, data_dict)

}

}
