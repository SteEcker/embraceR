#' Combine Outcomes Data from EMBRACE-I and EMBRACE-II
#'
#' Loads and combines outcome data from both EMBRACE studies, ensuring consistent
#' variable names and structure for comparative analysis.
#'
#' @return A tibble containing combined outcome data from both studies with:
#'   - Time-to-event variables for disease progression and vital status
#'   - Event indicators for various failure types (local, nodal, systemic)
#'   - Event indicators for specific anatomical locations
#'   - Composite endpoint indicators (locoregional, distant, disease control)
#'   - A 'study' identifier column to distinguish between cohorts
#'
#' @export
#'
#' @importFrom dplyr select mutate bind_rows %>%
#'
#' @examples
#' \dontrun{
#' combined_data <- get_combined_outcomes()
#' table(combined_data$study)
#' }
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
    select(embrace_id, any_of(outcome_cols)) %>%
    mutate(study = "EMBRACE-I")

  emii_outcomes <- df %>%
    emii_add_outcome() %>%
    select(embrace_id, all_of(outcome_cols)) %>%
    mutate(study = "EMBRACE-II")

  # Combine datasets
  bind_rows(emii_outcomes, emi_outcomes)
}
