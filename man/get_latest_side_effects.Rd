% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_latest_side_effects.R
\name{get_latest_side_effects}
\alias{get_latest_side_effects}
\title{Get the latest side effects for each patient}
\usage{
get_latest_side_effects(df)
}
\arguments{
\item{df}{The data frame containing the side effect and follow-up columns.}
}
\value{
A data frame with new columns for the latest side effects at the latest follow-up ID.
}
\description{
Get the latest side effects for each patient
}
