#' Verify All Event Calculations
#'
#' Runs all event verification functions and combines results into a single Excel workbook
#' with multiple sheets. Provides a comprehensive validation of disease events, survival
#' outcomes, and failure patterns for EMBRACE-II data.
#'
#' @param save_excel Logical; whether to save results to Excel file (default: TRUE)
#'
#' @return A list containing all verification results with each element representing
#'   a different event verification dataset
#'
#' @export
#'
#' @import dplyr
#' @importFrom openxlsx createWorkbook addWorksheet writeData saveWorkbook
#'
#' @examples
#' \dontrun{
#' results <- emii_verify_all_events()
#' results <- emii_verify_all_events(save_excel = FALSE)
#' }
emii_verify_all_events <- function(save_excel = TRUE) {
  # Create list to store all verification results
  verification_results <- list()

  .data <- embraceR::load_embrace_ii()

  .data <- .data %>%
    embraceR::add_disease_events()

  # Run verification functions for BASE EVENTS (these are the fundamental events)
  message("Verifying base events...")
  verification_results$local_failure <- add_local_failure_event_with_verification(.data)
  verification_results$nodal_failure <- add_nodal_failure_event_with_verification(.data)
  verification_results$systemic_failure <- add_systemic_failure_event_with_verification(.data)
  verification_results$vital_status <- add_vitalstatus_event_with_verification(.data)
  
  # Run verification functions for DERIVED/COMPOSITE EVENTS
  message("Verifying derived/composite events...")
  verification_results$pelvic_nodal <- emii_add_pelvic_nodal_event_with_verification(.data)
  verification_results$pelvic_event <- emii_add_pelvic_event_with_verification(.data)
  verification_results$systemic_excl_pao <- emii_add_systemic_excl_pao_with_verification(.data)
  verification_results$paraaortic_nodal <- emii_add_paraaortic_nodal_with_verification(.data)
  verification_results$nodal_control_incl_pao <- emii_add_nodalcontrol_incl_pao_with_verification(.data)
  verification_results$locoregional <- add_locoregional_event_with_verification(.data)
  verification_results$cancer_specific <- emii_add_cancer_specific_with_verification(.data)
  verification_results$disease_control <- emii_add_disease_control_with_verification(.data)
  verification_results$progression_free <- emii_add_progression_free_survival_with_verification(.data)
  verification_results$distant_alone <- emii_add_distant_alone_with_verification(.data)
  
  # Run verification for METASTASES (helper variables used by multiple events)
  message("Verifying metastases variables...")
  verification_results$metastases <- add_metastases_with_verification(.data)
  if (save_excel) {
    # Create workbook
    wb <- createWorkbook()

    # Add each verification result as a separate worksheet
    for (name in names(verification_results)) {
      addWorksheet(wb, name)
      writeData(wb, name, verification_results[[name]])
    }

    # Add documentation sheet
    addWorksheet(wb, "Documentation")
    doc_text <- data.frame(
      Description = c(
        "Event Verification Results - EMBRACE-II",
        "",
        "=== BASE EVENTS (Fundamental disease status assessments) ===",
        "",
        "1. Local Failure (event_localfailure)",
        "   - Based on: disease_local_status_* columns",
        "   - Event when any status = 2 (recurrence/progression)",
        "",
        "2. Nodal Failure (event_nodalfailure)",
        "   - Based on: disease_nodal_status_* columns",
        "   - Event when any status = 2 (recurrence/progression)",
        "",
        "3. Systemic Failure (event_systemicfailure)",
        "   - Based on: disease_systemic_status_* columns",
        "   - Event when any status = 2 (recurrence/progression)",
        "",
        "4. Vital Status (event_vitalstatus)",
        "   - Based on: vital_status and latest_assessment_date_disease",
        "   - Event = 1 if patient died, 0 if alive",
        "",
        "=== DERIVED/COMPOSITE EVENTS (Based on base events and anatomical locations) ===",
        "",
        "5. Pelvic Nodal (event_pelvic_nodal)",
        "   - Based on: event_nodalfailure + pelvic anatomical locations",
        "   - Includes: ext/int/com iliac, parametrial, but excludes para-aortic, groin",
        "",
        "6. Pelvic Event (event_pelvic)",
        "   - Based on: event_localfailure OR event_pelvic_nodal",
        "",
        "7. Systemic excl. PAO (event_systemic_excl_pao)",
        "   - Based on: event_systemicfailure MINUS isolated para-aortic nodes above L2",
        "   - Cases with PAO + other metastases remain included",
        "",
        "8. Para-aortic Nodal (event_paraaortic_nodal)",
        "   - Based on: has_paraaortic_nodes_above_l2 OR has_Para.Aortic_followup",
        "",
        "9. Nodal Control incl. PAO (event_nodalcontrol_incl_pao)",
        "   - Based on: event_pelvic_nodal OR event_paraaortic_nodal",
        "",
        "10. Locoregional (event_locoregional + event_locoregional_alone)",
        "    - event_locoregional: event_localfailure OR event_nodalcontrol_incl_pao",
        "    - event_locoregional_alone: locoregional WITHOUT systemic_excl_pao",
        "",
        "11. Cancer-Specific Survival (event_cancer_specific)",
        "    - Based on: event_vitalstatus=1 AND cause_of_death in (1,3)",
        "",
        "12. Disease Control (event_disease_control)",
        "    - Based on: event_localfailure OR event_nodalfailure OR event_systemicfailure",
        "",
        "13. Progression-Free Survival (event_progression_free)",
        "    - Based on: event_disease_control OR event_vitalstatus=1",
        "",
        "14. Distant Alone (event_distant_alone)",
        "    - Based on: event_systemic_excl_pao WITHOUT event_locoregional",
        "",
        "15. Metastases (helper variables)",
        "    - has_*_metastases flags for: liver, bone, brain, lung, nodes, etc.",
        "",
        "=== Sheet Structure ===",
        "",
        "Each sheet contains:",
        "- embrace_id: Patient identifier",
        "- Input columns: Variables used in the calculation",
        "- Output column(s): Event variable(s) generated by the function",
        "",
        "All 15 event endpoints from variable-description.Rmd are included."
      )
    )
    writeData(wb, "Documentation", doc_text)

    # Save workbook
    saveWorkbook(wb, "event_verification_results.xlsx", overwrite = TRUE)
  }

  return(verification_results)
}
