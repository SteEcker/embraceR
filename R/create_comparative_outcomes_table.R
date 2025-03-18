#' Create Comparative Outcomes Table
#'
#' Generates a formatted table comparing clinical outcomes between EMBRACE-I and EMBRACE-II
#' studies, including local control, nodal control, survival metrics, and time-to-event data.
#'
#' @return A gtsummary table with outcome comparisons and p-values
#'
#' @export
#'
#' @importFrom gtsummary tbl_summary add_overall add_p modify_header modify_spanning_header modify_caption bold_p
#'
#' @examples
#' \dontrun{
#' outcomes_table <- create_comparative_outcomes_table()
#' }
create_comparative_outcomes_table <- function() {
  # Get combined outcomes data
  combined_outcomes <- get_combined_outcomes() %>%
    select(-embrace_id)

  # Create comparative table
  combined_outcomes %>%
    gtsummary::tbl_summary(
      by = study,
      # missing = "no",
      label = list(
        timetoevent_disease ~ "Time to Event",
        timetoevent_vitalstatus ~ "Time to Last Vital Status",
        event_localfailure ~ "Local Control",
        event_nodalfailure ~ "Nodal Control (lower PAO)",
        event_systemicfailure ~ "Systemic Control (incl. upper PAO)",
        event_nodalcontrol_incl_pao ~ "Nodal control (incl. all PAO)",
        event_pelvic_nodal ~ "Pelvic Nodal control",
        event_pelvic ~ "Pelvic control (local + nodal)",
        event_paraaortic_nodal ~ "Paraaortic nodal control",
        event_systemic_excl_pao ~ "Systemic control (excl. PAO)",
        event_locoregional ~ "Locoregional (Pelvic/PAO)",
        event_locoregional_alone ~ "Locoregional alone (no metastases)",
        event_distant_alone ~ "Distant alone",
        event_disease_control ~ "Disease Control",
        event_cancer_specific ~ "Cancer-Specific Survival",
        event_vitalstatus ~ "Overall Survival",
        event_progression_free ~ "Progression-Free Survival"
      )
    ) %>%
    gtsummary::add_p() %>%
    gtsummary::modify_header(label = "**Endpoint**") %>%
    gtsummary::modify_spanning_header(c("stat_1", "stat_2") ~ "Study") %>%
    gtsummary::modify_caption("Comparison of Outcomes between EMBRACE-I and EMBRACE-II") %>%
    gtsummary::bold_p(t = 0.05)
}

