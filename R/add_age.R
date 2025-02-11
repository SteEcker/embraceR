

#' Add Age to Data Frame
#'
#' This function calculates the age of subjects in a data frame based on their
#' year of birth or birth date and the date of histology assessment. The method
#' of calculation depends on the study specified in the data frame.
#'
#' @param df A data frame containing the columns `study`, `histology_assessment_date`,
#'   `birth_date_pat`, and `year_of_birth`. The `study` column is used to determine
#'   the method of age calculation. The `histology_assessment_date` must be in a
#'   date format that `lubridate::year()` can recognize.
#' @return Returns the input data frame with an additional column `age` that contains
#'   the calculated age of the subjects.
#' @export
#' @examples
#' df <- data.frame(
#'   study = c("embrace_i", "other_study"),
#'   histology_assesment_date = as.Date(c("2024-01-01", "2024-01-02")),
#'   birth_date_pat = c(1980, NA),
#'   year_of_birth = c(NA, 1990)
#' )
#' df_with_age <- add_age(df)
#' @importFrom dplyr mutate
#' @importFrom lubridate year
add_age <- function(df) {

  # Ensure that 'dplyr' and 'lubridate' are loaded
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("package 'dplyr' is required")
  if (!requireNamespace("lubridate", quietly = TRUE)) stop("package 'lubridate' is required")

  if (df$study[1] == "embrace_i") {
    df <- dplyr::mutate(df, age = lubridate::year(histology_assesment_date) - lubridate::year(birth_date_pat))
  } else {
    df <- dplyr::mutate(df, age = lubridate::year(histology_assesment_date) - year_of_birth)
  }

  return(df)
}

