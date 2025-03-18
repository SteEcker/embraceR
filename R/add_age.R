#' Add Age to Data Frame
#'
#' Calculates the age of subjects based on their birth information and histology assessment date.
#' For EMBRACE-I study, age is calculated using birth_date_pat, while other studies use year_of_birth.
#'
#' @param df A data frame containing:
#'   - study: Character indicating the study name ("embrace_i" or other)
#'   - histology_assessment_date: Date of histology assessment
#'   - birth_date_pat: Birth date for EMBRACE-I subjects
#'   - year_of_birth: Birth year for non-EMBRACE-I subjects
#'
#' @return The input data frame with an additional 'age' column
#' @export
#'
#' @importFrom dplyr mutate
#' @importFrom lubridate year
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   study = c("embrace_i", "other_study"),
#'   histology_assesment_date = as.Date(c("2024-01-01", "2024-01-02")),
#'   birth_date_pat = c(1980, NA),
#'   year_of_birth = c(NA, 1990)
#' )
#' df_with_age <- add_age(df)
#' }
add_age <- function(df) {
  # Ensure that 'dplyr' and 'lubridate' are loaded
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("package 'dplyr' is required")
  }
  if (!requireNamespace("lubridate", quietly = TRUE)) {
    stop("package 'lubridate' is required")
  }

  if (df$study[1] == "embrace_i") {
    df <- dplyr::mutate(df, age = lubridate::year(histology_assesment_date) - lubridate::year(birth_date_pat))
  } else {
    df <- dplyr::mutate(df, age = lubridate::year(histology_assesment_date) - year_of_birth)
  }

  return(df)
}

