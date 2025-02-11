#' Add a Column for Nodal Failure Events
#'
#' This function adds a new column to the data frame that indicates whether
#' a nodal failure event has occurred for each patient. The column name varies by study:
#' - EMBRACE-I: 'event_nodalcontrol_incl_pao'
#' - EMBRACE-II: 'event_nodalfailure'
#' 
#' A nodal failure event is defined as an occurrence of value 2 in any nodal status column.
#'
#' @param df A data frame containing:
#'   - columns starting with 'disease_nodal_status_' containing numerical values
#'   - a 'study' column indicating either 'embrace_i' or 'embrace_ii'
#'
#' @return A data frame with an additional column (name depends on study) containing
#' a 1 if a nodal failure event has occurred and 0 otherwise. If all values are NA across 
#' all relevant columns for a particular patient, the result will also be NA.
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
#'
#' @export
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
