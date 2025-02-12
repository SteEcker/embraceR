% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_morbidity_events.R
\name{add_grade_ctcae_event_column}
\alias{add_grade_ctcae_event_column}
\title{Add a Column for Grade XX CTCAE Event}
\usage{
add_grade_ctcae_event_column(df, grade_threshold)
}
\arguments{
\item{df}{DataFrame containing the data.}

\item{grade_threshold}{Numeric, the threshold value to compare against.}
}
\value{
DataFrame with an additional column "has_gradeX_ctcae_event" where X is the grade threshold.
}
\description{
This function selects all outcome columns and adds a new column named dynamically
based on the grade threshold (e.g., "grade4_ctcae_event").
}
