% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/emii_add_distant_alone.R
\name{emii_add_distant_alone_with_verification}
\alias{emii_add_distant_alone_with_verification}
\title{Add Distant Alone Status with Verification}
\usage{
emii_add_distant_alone_with_verification(.data, save_excel = FALSE)
}
\arguments{
\item{.data}{The input dataframe containing patient failure data}

\item{save_excel}{Logical indicating whether to save results as Excel file (default: FALSE)}
}
\value{
A tibble with the distant alone variable and relevant columns
}
\description{
This function adds a column indicating whether a patient had distant metastases alone
and returns a tibble with the created column and all columns used for verification.
}
\examples{
\dontrun{
data <- tibble(
  embrace_id = c("A1", "A2"),
  event_systemicfailure = c(TRUE, FALSE),
  has_paraaortic_nodes_above_l2 = c(FALSE, TRUE),
  event_paraaortic_nodal = c(FALSE, FALSE),
  event_localfailure = c(FALSE, FALSE),
  event_pelvic_nodal = c(FALSE, FALSE)
)
result <- add_distant_alone_with_verification(data)
result <- add_distant_alone_with_verification(data, save_excel = TRUE)
}
}
