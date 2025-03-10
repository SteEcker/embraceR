% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/emii_add_number_paraaortic_ln_statd.R
\name{add_number_paraaortic_ln_stat_d}
\alias{add_number_paraaortic_ln_stat_d}
\title{Count Para-Aortic Lymph Nodes}
\usage{
add_number_paraaortic_ln_stat_d(df)
}
\arguments{
\item{df}{A data frame containing lymph node information with anatomical positions}
}
\value{
A data frame with added `number_paraaortic_ln_stat_d` column
}
\description{
Calculates the number of para-aortic lymph nodes per patient from staging data
and adds this count as a new column to the dataset.
}
\examples{
\dontrun{
emii_data <- load_embrace_ii()
result <- add_number_paraaortic_ln_stat_d(emii_data)
}
}
\keyword{internal}
