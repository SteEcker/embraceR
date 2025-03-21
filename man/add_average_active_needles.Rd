% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_average_active_needles.R
\name{add_average_active_needles}
\alias{add_average_active_needles}
\title{Calculate Average Number of Active Needles}
\usage{
add_average_active_needles(df)
}
\arguments{
\item{df}{A data frame containing columns with pattern "fraction_*_active_needles"
that store the number of active needles for each treatment fraction}
}
\value{
The input data frame with an additional column `average_nr_active_needles`
        containing the rounded average. NaN values are converted to NA.
}
\description{
Computes the average number of active needles across all fractions for each subject.
Identifies columns containing fraction-specific active needle counts and calculates
their mean value.
}
\examples{
\dontrun{
df <- data.frame(fraction_1_active_needles = c(1, 2, 3),
                 fraction_2_active_needles = c(3, 4, 5),
                 some_other_column = c('a', 'b', 'c'))
average_active_needles(df)
}
}
