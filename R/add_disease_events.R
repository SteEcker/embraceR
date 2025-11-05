#' Add Disease Event Columns
#'
#' Adds columns for local, nodal, and systemic failure events, plus time-to-event data.
#'
#' @param df A dataframe containing patient data with required columns for failure events
#'
#' @return A dataframe with added columns for failure events and time measurements
#' @export
#'
#' @import dplyr
#'
#' @examples
#' \dontrun{
#' patient_data <- data.frame(...)
#' enhanced_data <- add_disease_events(patient_data)
#' }
add_disease_events <- function(df){
  df <- df %>%
    add_local_failure_event() %>%
    add_nodal_failure_event() %>%
    add_systemic_failure_event()
  
  # Replace NA with 0 for event variables where all three are NA
  event_vars <- c("event_localfailure", "event_nodalfailure", "event_systemicfailure")
  if (all(event_vars %in% names(df))) {
    df <- df %>%
      mutate(
        # Identify patients where all three event variables are NA
        all_events_na = is.na(event_localfailure) & 
                        is.na(event_nodalfailure) & 
                        is.na(event_systemicfailure),
        # Replace NA with 0 for those patients only
        event_localfailure = if_else(all_events_na, 0, event_localfailure),
        event_nodalfailure = if_else(all_events_na, 0, event_nodalfailure),
        event_systemicfailure = if_else(all_events_na, 0, event_systemicfailure)
      ) %>%
      select(-all_events_na)  # Remove temporary helper column
  }
  
  df %>%
    add_time_to_diseaseevent() %>%
    add_time_to_last_vitalstatus() %>%
    add_vitalstatus_event()

}
