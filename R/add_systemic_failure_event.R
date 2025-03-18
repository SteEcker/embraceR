#' Add Systemic Failure Event Column
#'
#' Adds a study-specific systemic failure event indicator column.
#' For EMBRACE-I: 'event_systemic_excl_pao' (values 1 or 2 count as events)
#' For EMBRACE-II: 'event_systemicfailure' (only value 2 counts as event)
#'
#' @param df A data frame with 'disease_systemic_status_*' columns and a 'study' column
#'
#' @return A data frame with added systemic failure event columns
#'
#' @export
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
