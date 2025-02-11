#' Get All EMII Outcome Endpoints
#'
#' @description
#' This function applies all endpoint definitions to the raw EMII data,
#' creating a comprehensive dataset with all outcome measures.
#'
#' @param .data The raw EMII dataframe
#' @return Dataframe with all additional outcome columns added
#' @export
#'
#' @import dplyr
emii_add_outcome <- function(.data) {
  # Required input columns
  required_cols <- c(
    "event_nodalfailure",
    "event_localfailure",
    "event_systemicfailure",
    "event_vitalstatus",
    "vital_status_cause_of_death_vital_status"
  )

  # Check if all required columns are present
  missing_cols <- setdiff(required_cols, names(.data))
  if (length(missing_cols) > 0) {
    stop(
      "Missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }

  .data %>%
    emii_add_recurrent_nodes() %>%
    emii_add_nodal_classification() %>%
    emii_add_pelvic_nodal_event() %>%
    emii_add_pelvic_event() %>%
    emii_add_systemic_excl_pao() %>%
    emii_add_paraaortic_nodal() %>%
    emii_add_nodalcontrol_incl_pao() %>%
    add_locoregional_event() %>%
    emii_add_cancer_specific() %>%
    emii_add_disease_control() %>%
    emii_add_progression_free_survival() %>%
    emii_add_distant_alone()
}
