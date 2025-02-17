% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_vitalstatus_event.R
\name{add_vitalstatus_event}
\alias{add_vitalstatus_event}
\title{Add Vital Status Events to DataFrame}
\usage{
add_vitalstatus_event(df)
}
\arguments{
\item{df}{A data frame that contains at least the columns `vital_status_os` and `latest_assessment_date_disease`.}
}
\value{
A data frame with the updated or new `vital_status_os` column.
}
\description{
This function adds or updates a column named `vital_status_os` in the given data frame.
It sets the value of `vital_status_os` based on the existing value and the value of `latest_assessment_date_disease`.
If `vital_status_os` is NA and `latest_assessment_date_disease` is not NA, `vital_status_os` is set to 0.
}
\examples{
\dontrun{
# Given a DataFrame df with columns `vital_status_os` and `latest_assessment_date_disease`
# df <- add_vitalstatus_event(df)
}

}
