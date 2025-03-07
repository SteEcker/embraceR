% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load_embrace_i.R
\name{load_embrace_i}
\alias{load_embrace_i}
\title{Load and Process EMBRACE-I Data}
\usage{
load_embrace_i(
  file_path = here::here("data_raw/embrace_I"),
  add_new_columns = TRUE,
  mapping_file = here::here("data_raw/mapping_table/mapping_table.xlsx"),
  filter_cohort = TRUE
)
}
\arguments{
\item{file_path}{Character, path to the directory containing the primary data files}

\item{add_new_columns}{Logical, if TRUE, calculate and add derived columns (default: TRUE)}

\item{mapping_file}{Character, path to the column mapping table file}

\item{filter_cohort}{Logical, if TRUE, filter to include only patients with local failure data (default: TRUE)}
}
\value{
A tibble containing the cleaned and joined EMBRACE-I data
}
\description{
Loads data from multiple sources for the EMBRACE-I study, processes them,
and joins them by the `embrace_id` column. The function handles T-stage data,
morbidity outcomes, and main study data, combining them into a comprehensive dataset.
}
\examples{
\dontrun{
  # Load with default settings
  df <- load_embrace_i()
  
  # Load without filtering cohort
  df <- load_embrace_i(filter_cohort = FALSE)
  
  # Load with custom mapping file
  df <- load_embrace_i(mapping_file = "path/to/custom_mapping.xlsx")
}
}
