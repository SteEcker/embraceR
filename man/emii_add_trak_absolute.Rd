% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/emii_add_trak_absolute.R
\name{emii_add_trak_absolute}
\alias{emii_add_trak_absolute}
\title{Add Trak Absolute Values and Clean Data}
\usage{
emii_add_trak_absolute(df, num_fractions = 7)
}
\arguments{
\item{df}{A dataframe containing the trak and percentage columns for multiple fractions.}

\item{num_fractions}{An integer specifying the number of fractions to process. Default is 7.}
}
\value{
The dataframe with the new absolute values for each trak component, cleaned based on the given rules.
}
\description{
This function calculates the absolute contributions for `tandem_applicator`, `vaginal_applicator`, `needles`,
and `trak` totals across multiple fractions
}
\details{
The function works in three main steps:
1. **trak_calculate_all_fractions()**: Calculates absolute contributions for all components (tandem, vaginal, needles) for each fraction.
2. **trak_calculate_totals()**: Calculates total sums for all fractions.
3. **trak_clean_data()**: Cleans the data, setting the sum columns to `NA` if volume is greater than 150 or any trak component is zero.
}
