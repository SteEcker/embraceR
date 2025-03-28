% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/recode_factor_levels.R
\name{recode_factor_levels}
\alias{recode_factor_levels}
\title{Recode factor levels for multiple columns in a data frame}
\usage{
recode_factor_levels(x, old_levels, new_levels)
}
\arguments{
\item{x}{A factor vector from a data frame column}

\item{old_levels}{A character vector of old factor levels}

\item{new_levels}{A character vector of new factor levels}
}
\value{
The factor vector with recoded factor levels
}
\description{
This function takes a data frame and recodes the factor levels of multiple columns
based on the supplied old and new levels. It is designed to be used with dplyr's
mutate(across()) function.
}
