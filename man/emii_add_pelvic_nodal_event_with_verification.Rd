% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/emii_add_pelvic_nodal_event.R
\name{emii_add_pelvic_nodal_event_with_verification}
\alias{emii_add_pelvic_nodal_event_with_verification}
\title{Add Pelvic Nodal Event Status with Verification}
\usage{
emii_add_pelvic_nodal_event_with_verification(.data, save_excel = FALSE)
}
\arguments{
\item{.data}{The input dataframe containing patient node data}

\item{save_excel}{Logical indicating whether to save results as Excel file (default: FALSE)}
}
\value{
A tibble with the pelvic nodal event variable and relevant columns
}
\description{
This function adds a column indicating whether a patient had a pelvic nodal event
and returns a tibble with the created column and all columns used for verification.
}
\examples{
\dontrun{
data <- tibble(
  embrace_id = c("A1", "A2"),
  event_nodalfailure = c(TRUE, FALSE),
  has_Para.Aortic_followup = c(FALSE, TRUE),
  has_other_followup = c(FALSE, FALSE),
  has_L.groin_followup = c(FALSE, FALSE),
  has_R.groin_followup = c(FALSE, FALSE)
)
result <- emii_add_pelvic_nodal_event_with_verification(data)
result <- emii_add_pelvic_nodal_event_with_verification(data, save_excel = TRUE)
}
}
