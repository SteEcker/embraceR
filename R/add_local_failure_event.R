#' Add Local Failure Event Column
#'
#' Adds a binary indicator column for local failure events based on disease status.
#'
#' @param df A data frame containing columns starting with 'disease_local_status_'
#'
#' @return A data frame with an additional 'event_localfailure' column (1=failure, 0=no failure)
#' @export
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   disease_local_status_3m = c(0, 2, 0),
#'   disease_local_status_6m = c(0, 0, 2)
#' )
#' result <- add_local_failure_event(df)
#' }
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
