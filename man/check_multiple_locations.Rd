% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/emii_add_recurrent_nodes.R
\name{check_multiple_locations}
\alias{check_multiple_locations}
\title{Check for Multiple Nodal Locations and Add to Original Data}
\usage{
check_multiple_locations(.data, locations, time_point = "any")
}
\arguments{
\item{.data}{The input dataframe containing patient node data}

\item{locations}{Character vector of anatomical locations to check}

\item{time_point}{Optional character string to specify which timepoint to check}
}
\value{
Original dataframe with additional boolean columns for each location
}
\description{
This function extends check_nodal_location to check for multiple locations simultaneously
}
\examples{
\dontrun{
# Check for multiple locations
locations <- c("R int iliac", "L int iliac", "Para Aortic")
result <- check_multiple_locations(patient_data, locations)
}
}
