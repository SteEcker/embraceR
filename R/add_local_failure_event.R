#' Add a Column for Local Failure Events
#'
#' This function adds a new column to the dataframe that indicates whether a local failure event
#' has occurred for each patient. A local failure event is defined as an occurrence of the value 2
#' in any of the 'overall_disease_local_recurrence_' columns.
#'
#' @param df A data frame containing at least one column starting with 'overall_disease_local_recurrence_'.
#' These columns should contain numerical values where 2 indicates a local failure event.
#'
#' @return A data frame with an additional column named 'event_localfailure'. This column contains
#' a 1 if a local failure event has occurred and a 0 otherwise.
#'
#' @examples
#' \dontrun{
#' # Example DataFrame (df)
#' # overall_disease_local_recurrence_3m overall_disease_local_recurrence_6m
#' #                                    0                                   2
#' #                                    0                                   0
#' #                                    2                                   0
#' # df <- add_local_failure_event(df)
#' }
#'
#' @export
add_local_failure_event <- function(df) {
  message('Adding local failure events')

  # Get the column names that correspond to local disease recurrence
  local_disease_cols <- df %>% select(starts_with('disease_local_status_')) %>% colnames()

  # Vectorized approach instead of rowwise
  df <- df %>%
    mutate(
      event_localfailure = case_when(
        # If all values are NA, return NA
        rowSums(!is.na(across(all_of(local_disease_cols)))) == 0 ~ NA_integer_,
        # Otherwise check for any 2's
        TRUE ~ as.integer(rowSums(across(all_of(local_disease_cols)) == 2, na.rm = TRUE) > 0)
      )
    )

  return(df)
}
