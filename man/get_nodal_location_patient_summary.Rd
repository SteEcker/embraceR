% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/emii_get_lymph_nodes_statd.R
\name{get_nodal_location_patient_summary}
\alias{get_nodal_location_patient_summary}
\title{Generate Patient-Level Summary Table of Nodal Anatomical Locations}
\usage{
get_nodal_location_patient_summary(df)
}
\arguments{
\item{df}{A dataframe containing the EMBRACE-II dataset}
}
\value{
A gt table object showing the distribution of patients with lymph nodes by location
}
\description{
Creates a formatted summary table using gtsummary showing the number and percentage
of patients who have at least one lymph node in each anatomical location from
EMBRACE-II at diagnosis.
}
