#' Add Disease Event Columns
#'
#' Adds columns for local, nodal, and systemic failure events, plus time-to-event data.
#'
#' @param df A dataframe containing patient data with required columns for failure events
#'
#' @return A dataframe with added columns for failure events and time measurements
#' @keywords internal
#'
#' @importFrom dplyr "%>%"
#'
#' @examples
#' \dontrun{
#' patient_data <- data.frame(...)
#' enhanced_data <- add_disease_events(patient_data)
#' }
add_disease_events <- function(df){
  df %>%
    add_local_failure_event() %>%
    add_nodal_failure_event() %>%
    add_systemic_failure_event() %>%
    add_time_to_diseaseevent() %>%

    add_time_to_last_vitalstatus() %>%
    add_vitalstatus_event()

}
