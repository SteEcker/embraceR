% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_icis.R
\name{add_icis}
\alias{add_icis}
\title{Add ICIS Indicator Based on Technique}
\usage{
add_icis(.data)
}
\arguments{
\item{.data}{A data frame containing an `embrace_id` column and other columns that end with "technique_tdvh" and
start with "fraction". These should contain values that can be transformed into factors.}
}
\value{
A data frame with an additional column `icis`, which is a numerical indicator (1 or 0) based on the
        conditions applied to columns ending with "technique_tdvh" and starting with "fraction".
}
\description{
This function takes a data frame that includes an `embrace_id` column and other columns ending with "technique_tdvh"
and starting with "fraction". It processes these columns to determine if ICIS should be "TRUE" or "FALSE" for each `embrace_id`,
converting it into a numerical indicator (1 or 0) in a new column named `icis`.
}
\examples{
\dontrun{
df <- data.frame(embrace_id = c(1, 2, 3),
                 fraction_1_technique_tdvh = c("0", "1", "0"),
                 fraction_2_technique_tdvh = c("0", "1", "1"))
add_icis(df)
}
}
