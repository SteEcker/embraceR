% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_latest_side_effects.R
\name{get_max_side_effects}
\alias{get_max_side_effects}
\title{Get maximum side effect values and corresponding timepoints and export to Excel with conditional formatting}
\usage{
get_max_side_effects(
  df,
  endpoints,
  max_grade = 0,
  export_to_excel = FALSE,
  file_name = "max_side_effects.xlsx"
)
}
\arguments{
\item{df}{A data frame containing side effect columns with multiple timepoints and a patient ID column "embrace_id".}

\item{endpoints}{A character vector of endpoint basenames (e.g., from embraceR::clean_side_effect_names()).}

\item{max_grade}{Numeric threshold; only patients with at least one max_value >= max_grade will be retained.}

\item{export_to_excel}{Logical; if TRUE, exports the result to an Excel file.}

\item{file_name}{Character; the name of the Excel file to export if export_to_excel is TRUE.}
}
\value{
A data frame with maximum side effect values and timepoints for each endpoint and patient.
}
\description{
Get maximum side effect values and corresponding timepoints and export to Excel with conditional formatting
}
