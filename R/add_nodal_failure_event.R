#' Add Nodal Failure Event Column
#'
#' Adds a study-specific nodal failure event indicator column based on disease nodal status.
#' For EMBRACE-I: 'event_nodalcontrol_incl_pao'
#' For EMBRACE-II: 'event_nodalfailure'
#'
#' @param df A data frame with 'disease_nodal_status_*' columns and a 'study' column
#'
#' @return A data frame with an added nodal failure event column (1=failure, 0=no failure)
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' test_data <- tibble(
#'   study = c("embrace_i", "embrace_ii"),
#'   disease_nodal_status_3m = c(0, 2),
#'   disease_nodal_status_6m = c(0, 0)
#' )
#' result <- add_nodal_failure_event(test_data)
#' }
add_nodal_failure_event <- function(df) {
  message('Adding nodal failure events')

  # Get the column names that correspond to nodal disease recurrence
  nodal_disease_cols <- df %>% select(starts_with('disease_nodal_status_')) %>% colnames()

  # Vectorized approach instead of rowwise
  df <- df %>%
    mutate(
      # Calculate event status
      event_temp = case_when(
        # If all values are NA, return NA
        rowSums(!is.na(across(all_of(nodal_disease_cols)))) == 0 ~ NA_integer_,
        # Otherwise check for any 2's
        TRUE ~ as.integer(rowSums(across(all_of(nodal_disease_cols)) == 2, na.rm = TRUE) > 0)
      )
    ) %>%
    # Add the appropriate column based on study
    mutate(
      event_nodalcontrol_incl_pao = if_else(study == "embrace_i", event_temp, NA_integer_),
      event_nodalfailure = if_else(study == "embrace_ii", event_temp, NA_integer_)
    ) %>%
    select(-event_temp)  # Remove temporary column

  return(df)
}
