% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/emii_add_recurrent_nodes.R
\name{process_followup_nodes}
\alias{process_followup_nodes}
\title{Process Follow-up Node Data}
\usage{
process_followup_nodes(.data, diagnosis_nodes)
}
\arguments{
\item{.data}{A data frame containing follow-up node information}

\item{diagnosis_nodes}{Lookup table of nodes at diagnosis}
}
\value{
A tibble with processed follow-up node data
}
\description{
Internal helper function to efficiently process follow-up node data by
extracting and transforming node information from multiple follow-up visits.
}
\keyword{internal}
