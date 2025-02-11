#' Get Status of Dosimetric Parameters and Compare with EMBRACE-I
#'
#' @return A gtsummary object containing the comparative summary table.
#'
#' @export
emii_get_dosimetric_parameter_status <- function() {
  
  emi <- embraceR::load_embrace_i()
  emii <- embraceR::load_embrace_ii()

  # Select dosimetric parameters from both studies
  emii_data <- emii %>%
    dplyr::select(
      embrace_id,
      starts_with('total_')
    ) %>%
    embraceR::recode_and_convert_all_columns() %>%
    dplyr::mutate(study = "EMBRACE-II")

selected_cols <- emii_data %>% select(-embrace_id) %>% colnames()

  emi_data <- emi %>%
    dplyr::select(
      embrace_id,
      all_of(selected_cols)
    ) %>%
    dplyr::mutate(study = "EMBRACE-I")

  # Combine datasets
  combined_data <- dplyr::bind_rows(emii_data, emi_data)

  # Create comparative summary table
  summary_table <- combined_data %>%
    dplyr::select(-embrace_id) %>%
    gtsummary::tbl_summary(
      by = study,
      missing = "no"
    ) %>%
    gtsummary::add_p() %>%
    gtsummary::modify_header(label = "**Variable**") %>%
    gtsummary::modify_spanning_header(c("stat_1", "stat_2") ~ "Study") %>%
    gtsummary::modify_caption("**Comparison of Dosimetric Parameters between EMBRACE-I and EMBRACE-II**") 

  return(summary_table)
} 