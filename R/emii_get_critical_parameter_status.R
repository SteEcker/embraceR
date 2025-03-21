#' Get Status of Critical Parameters
#'
#' Generates a summary table comparing critical parameters between EMBRACE I and II datasets.
#' The function analyzes key clinical and treatment parameters, calculates the proportion
#' of missing data for each parameter, and performs statistical comparisons between the
#' two study cohorts.
#'
#' @param save_excel Logical; if TRUE, saves the summary table as an Excel file (default: FALSE)
#'
#' @return A gtsummary object containing the combined summary table with p-values
#'
#' @export
#'
#' @import dplyr
#' @importFrom gtsummary tbl_summary modify_header modify_caption bold_labels add_p
#' @importFrom openxlsx createWorkbook addWorksheet writeData saveWorkbook
#'
#' @examples
#' \dontrun{
#' # Generate summary table
#' summary_table <- emii_get_critical_parameter_status()
#'
#' # Generate and save to Excel
#' summary_table <- emii_get_critical_parameter_status(save_excel = TRUE)
#' }
emii_get_critical_parameter_status <- function(save_excel = FALSE) {

  # Load both datasets
  data_ii <- embraceR::load_embrace_ii() %>%
    emii_add_ott()
  data_i <- embraceR::load_embrace_i()

  # Load required libraries within the function
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("gtsummary", quietly = TRUE)
  requireNamespace("tidyr", quietly = TRUE)
  requireNamespace("gt", quietly = TRUE)
  requireNamespace("openxlsx", quietly = TRUE)

  # Select EMBRACE II critical parameters
  selected_data_ii <- data_ii %>%
    dplyr::select(
      # registration_date,
      embrace_id,
      tnmt_stage_sta_d,
      tnmn_stage_sta_d,
      tnmm_stage_sta_d,
      age,
      who_score_sta_d,
      histopathological_type_sta_d,
      # histology_assesment_date,
      pathological_nodes_present,
      conc_chemo_given_tdvh,
      conc_chemo_courses100pct_tdvh,
      # ebrt_start_date_tdvh,
      ebrt_gtv_t_volume_tdvh,
      ebrt_itv45_elective_nodes_incl_tdvh,
      fraction01dose_rate_tdvh,
      bt_implants_sta_b,
      bt_fractions_tdvh,
      icis,
      fraction01hrctv_volume_tdvh,
      ott
    ) %>%
    embraceR::recode_and_convert_all_columns() %>%
    mutate(study = "EMBRACE II")

  # Select corresponding EMBRACE I parameters
  selected_data_i <- data_i %>%
    dplyr::select(
      embrace_id,
      fig_oincl_ib1,
      histopathological_type_sta_d,
      age,
      pathological_nodes_present,
      fraction01dose_rate_tdvh,
      bt_implants_sta_b,
      bt_fractions_tdvh,
      icis,
      fraction01hrctv_volume_tdvh,
      ott
    ) %>%
    embraceR::recode_and_convert_all_columns() %>%
    mutate(study = "EMBRACE I")

  # Combine datasets
  combined_data <- bind_rows(selected_data_i, selected_data_ii)

  # Create a summary table grouped by study
  summary_table <- combined_data %>%
    dplyr::select(-embrace_id) %>%
    gtsummary::tbl_summary(
      # missing = "no",
      by = study,
      label = list(
        # registration_date ~ "Registration Date",
        tnmt_stage_sta_d ~ "TNM T-Stage",
        tnmn_stage_sta_d ~ "TNM N-Stage",
        tnmm_stage_sta_d ~ "TNM M-Stage",
        fig_oincl_ib1 ~ "FIGO 2009 (Lancet/JCO)",
        histopathological_type_sta_d ~ "Histopathological Type",
        pathological_nodes_present ~ "Pathological Nodes Present",
        conc_chemo_given_tdvh ~ "Chemotherapy Given",
        conc_chemo_courses100pct_tdvh ~ "Chemotherapy Courses 100%",
        ebrt_gtv_t_volume_tdvh ~ "EBRT GTV T Volume",
        ebrt_itv45_elective_nodes_incl_tdvh ~ "EBRT Elective Targets",
        fraction01dose_rate_tdvh ~ "BT Dose Rate",
        bt_implants_sta_b ~ "n BT Implants",
        bt_fractions_tdvh ~ "n BT Fractions",
        icis ~ "IC/IS",
        fraction01hrctv_volume_tdvh ~ "CTV-HR Volume"
      )
    ) %>%
    gtsummary::modify_header(label = "**Variable**") %>%
    gtsummary::modify_caption("**EMBRACE I & II: Comparison of critical parameters**") %>%
    gtsummary::bold_labels() %>%
    gtsummary::add_p()  # Add p-values for comparing groups

  # Handle Excel export
  if (save_excel) {
    file_name <- paste0(Sys.Date(), "_overview_critical_parameters_comparison.xlsx")

    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "Summary Table")
    openxlsx::addWorksheet(wb, "Raw Data")

    openxlsx::writeData(wb, sheet = "Summary Table", x = combined_data)
    openxlsx::writeData(wb, sheet = "Raw Data", x = combined_data)

    openxlsx::saveWorkbook(wb, file_name, overwrite = TRUE)
    message("Summary table saved to: ", file_name)
  }

  return(summary_table)
}
