#' Add a Column for Systemic Failure Events
#'
#' This function adds a new column to the data frame that indicates whether
#' a systemic failure event has occurred for each patient. The column name and event 
#' definition varies by study:
#' 
#' EMBRACE-I ('event_systemic_excl_pao'):
#'   - Event defined as occurrence of value 1 or 2 in any systemic status column
#' EMBRACE-II ('event_systemicfailure'):
#'   - Event defined as occurrence of value 2 in any systemic status column
#'
#' @param df A data frame containing:
#'   - columns starting with 'disease_systemic_status_' containing numerical values
#'   - a 'study' column indicating either 'embrace_i' or 'embrace_ii'
#'
#' @return A data frame with additional columns for systemic events. Each patient will
#' have a value in the column corresponding to their study, with NA in the other column.
#'
#' @examples
#' \dontrun{
#' test_data <- tibble(
#'   study = c("embrace_i", "embrace_ii"),
#'   disease_systemic_status_3m = c(1, 2),
#'   disease_systemic_status_6m = c(0, 0)
#' )
#' result <- add_systemic_failure_event(test_data)
#' }
#'
#' @export
add_systemic_failure_event <- function(df) {
  message('Adding systemic failure events')

  # Get the column names that correspond to systemic disease recurrence
  systemic_disease_cols <- df %>% select(starts_with('disease_systemic_status_')) %>% colnames()

  # Vectorized approach instead of rowwise
  df <- df %>%
    mutate(
      # Calculate event status
      event_temp = case_when(
        # If all values are NA, return NA
        rowSums(is.na(pick(all_of(systemic_disease_cols)))) == length(systemic_disease_cols) ~ NA_integer_,
        # For embrace_ii, only check for 2's
        study == "embrace_ii" ~ as.integer(rowSums(pick(all_of(systemic_disease_cols)) == 2, na.rm = TRUE) > 0),
        # For embrace_i, check for both 1's and 2's
        TRUE ~ as.integer(rowSums(pick(all_of(systemic_disease_cols)) == 1 | 
                                pick(all_of(systemic_disease_cols)) == 2, na.rm = TRUE) > 0)
      )
    ) %>%
    # Add the appropriate column based on study
    mutate(
      event_systemic_excl_pao = if_else(study == "embrace_i", event_temp, NA_integer_),
      event_systemicfailure = if_else(study == "embrace_ii", event_temp, NA_integer_)
    ) %>%
    select(-event_temp)  # Remove temporary column

  return(df)
}
