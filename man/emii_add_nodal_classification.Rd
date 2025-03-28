% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/emii_add_recurrent_nodes.R
\name{emii_add_nodal_classification}
\alias{emii_add_nodal_classification}
\title{Add Nodal Classification}
\usage{
emii_add_nodal_classification(.data)
}
\arguments{
\item{.data}{A data frame containing patient node data}
}
\value{
Data frame with an additional 'nodal_classification' column
}
\description{
Adds a new column classifying nodal status as 'N0', 'N1PAN', or 'N1pelvic'
based on pathological nodes present and Para-Aortic lymph node status at diagnosis.
}
\examples{
\dontrun{
emii_data <- load_embrace_ii()
result <- emii_add_nodal_classification(emii_data)
}
}
