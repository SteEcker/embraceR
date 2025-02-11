#' Get Combined Outcomes Data from EMBRACE-I and EMBRACE-II
#'
#' @description
#' Loads and combines outcome data from both EMBRACE studies, ensuring consistent
#' variable names and structure.
#'
#' @return A tibble containing combined outcome data from both studies with a 'study' identifier
#' @export
#'
#' @importFrom dplyr select mutate bind_rows %>%
get_combined_outcomes <- function() {
  # Define the columns we want in our final dataset
  outcome_cols <- c(
    "timetoevent_disease",
    "timetoevent_vitalstatus",
    "event_localfailure",
    "event_nodalfailure",
    "event_systemicfailure",
    "event_nodalcontrol_incl_pao",
    "event_pelvic_nodal",
    "event_pelvic",
    "event_paraaortic_nodal",
    "event_systemic_excl_pao",
    "event_locoregional",
    "event_locoregional_alone",
    "event_distant_alone",
    "event_cancer_specific",
    "event_vitalstatus",
    "event_disease_control",
    "event_progression_free"
  )

  # Load and process EMBRACE-I data
  emi <- embraceR::load_embrace_i() %>%
    emi_add_disease_control() %>%
    emi_add_progression_free_survival()

  # Load and process EMBRACE-II data
  emii <- embraceR::load_embrace_ii()
  df <- emii %>%
    embraceR::add_disease_events()

  # Select standardized outcome variables for both datasets
  emi_outcomes <- emi %>%
    select(any_of(outcome_cols)) %>%
    mutate(study = "EMBRACE-I")

  emii_outcomes <- df %>%
    emii_add_outcome() %>%
    select(all_of(outcome_cols)) %>%
    mutate(study = "EMBRACE-II")

  # Combine datasets
  bind_rows(emii_outcomes, emi_outcomes)
}
