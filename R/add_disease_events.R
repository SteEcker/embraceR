#' Add Disease Event Columns to a DataFrame
#'
#' This function enriches a dataframe by adding several new columns related to disease events.
#' It sequentially applies functions to add local failure event, nodal failure event,
#' systemic failure event, and time to disease event columns.
#'
#' @param df A dataframe containing patient data. It is expected that `df` already contains
#'   the necessary columns required by the `add_local_failure_event()`,
#'   `add_nodal_failure_event()`, `add_systemic_failure_event()`, and
#'   `add_time_to_diseaseevent()` functions.
#'
#' @return A dataframe that includes the original data along with four new columns:
#'   - Local failure event: Indicates whether a local failure event occurred.
#'   - Nodal failure event: Indicates whether a nodal failure event occurred.
#'   - Systemic failure event: Indicates whether a systemic failure event occurred.
#'   - Time to disease event: The time until a disease event occurred.
#'
#' @export
#' @importFrom dplyr "%>%"
add_disease_events <- function(df){
  df %>%
    add_local_failure_event() %>%
    add_nodal_failure_event() %>%
    add_systemic_failure_event() %>%
    add_time_to_diseaseevent() %>%

    add_time_to_last_vitalstatus() %>%
    add_vitalstatus_event()

}
