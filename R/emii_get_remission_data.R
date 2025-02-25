#' Extract First Remission Timepoint Data from EMBRACE-II Follow-up
#'
#' This function identifies the first timepoint where a patient achieves remission
#' (mri_remission_status_primary == 1) and extracts corresponding MRI measurements
#' from other variables at that timepoint.
#'
#' @param data A tibble containing EMBRACE-II follow-up data
#' @return A tibble with remission timepoint data (embrace_id, timepoint, and all MRI measurements)
#' @keywords internal
#'
#' @import dplyr
#' @import tidyr
#' @importFrom stringr str_extract
#'
#' @examples
#' \dontrun{
#'   followup_data <- load_embrace_ii_followup()
#'   remission_data <- emii_get_remission_data(followup_data)
#' }
emii_get_remission_data <- function(data) {
  
  # Ensure required columns exist
  if (!"embrace_id" %in% names(data)) {
    stop("Data must contain 'embrace_id' column")
  }
  
  # Get all remission status columns
  remission_cols <- grep("^mri_remission_status_primary_\\d+m$", 
                        names(data), value = TRUE)
  
  if (length(remission_cols) == 0) {
    stop("No remission status columns found")
  }
  
  # Define all MRI measurements to extract
  mri_measurements <- c(
    "tumor_width",
    "tumor_height",
    "tumor_thickness",
    "left_parametrium",
    "right_parametrium",
    "corpus_uteri",
    "vagina",
    "bladder",
    "rectum",
    "tumor_relationship_bt_target"
  )

  
  # Process each patient
  result <- data %>%
    # First convert to long format for remission status
    pivot_longer(
      cols = all_of(remission_cols),
      names_to = "timepoint",
      values_to = "remission_status"
    ) %>%
    # Extract month number from timepoint
    mutate(
      month = as.numeric(str_extract(timepoint, "\\d+"))
    ) %>%
    # Group by patient
    group_by(embrace_id) %>%
    # Find first remission timepoint
    filter(remission_status == 1) %>%
    slice_min(month, n = 1) %>%
    ungroup() %>%
    # Keep only month number
    select(embrace_id, month)
  
  # Now get corresponding values for all MRI measurements
  final_result <- result %>%
    rowwise() %>%
    mutate(
      !!!map(mri_measurements, ~{
        quo({
          col_name <- paste0("mri_", !!.x, "_", month, "m")
          if (col_name %in% names(data)) {
            data[[col_name]][data$embrace_id == embrace_id]
          } else {
            NA
          }
        })
      }) %>% 
      set_names(paste0("mri_", mri_measurements))
    ) %>%
    ungroup()
  
  # Return final dataset
  final_result %>%
    rename(timepoint_months = month) %>%
    select(embrace_id, timepoint_months, starts_with("mri_"))
} 



#' Extract First Remission Timepoint Data from EMBRACE-II Follow-up (Gynecological Examination)
#'
#' This function identifies the first timepoint where a patient achieves remission
#' based on gynecological examination (gyn_remission_status == 1) and extracts corresponding
#' gynecological measurements from other variables at that timepoint.
#'
#' @param data A tibble containing EMBRACE-II follow-up data
#' @return A tibble with remission timepoint data (embrace_id, timepoint, and all gynecological measurements)
#' @keywords internal
#'
#' @import dplyr
#' @import tidyr
#' @importFrom stringr str_extract
#'
#' @examples
#' \dontrun{
#'   followup_data <- load_embrace_ii_followup()
#'   gyn_remission_data <- emii_get_gyn_remission_data(followup_data)
#' }
emii_get_gyn_remission_data <- function(data) {
  
  # Ensure required columns exist
  if (!"embrace_id" %in% names(data)) {
    stop("Data must contain 'embrace_id' column")
  }
  
  # Get all remission status columns
  remission_cols <- grep("^gyn_remission_status_\\d+m$", 
                        names(data), value = TRUE)
  
  if (length(remission_cols) == 0) {
    stop("No gynecological remission status columns found")
  }
  
  # Define all gynecological measurements to extract
  gyn_measurements <- c(
    "tumor_width",
    "tumor_thickness",
    "left_parametrium",
    "right_parametrium",
    "vagina",
    "vagina_anterior",
    "vagina_posterior",
    "vagina_left_lateral",
    "vagina_right_lateral",
    "vagina_max_distal_extension_fornix",
    "bladder_cystoscopy",
    "rectum_exploration",
    "rectum_endoscopy",
    "biopsy"
  )
  
  # Process each patient
  result <- data %>%
    # First convert to long format for remission status
    pivot_longer(
      cols = all_of(remission_cols),
      names_to = "timepoint",
      values_to = "remission_status"
    ) %>%
    # Extract month number from timepoint
    mutate(
      month = as.numeric(str_extract(timepoint, "\\d+"))
    ) %>%
    # Group by patient
    group_by(embrace_id) %>%
    # Find first remission timepoint
    filter(remission_status == 1) %>%
    slice_min(month, n = 1) %>%
    ungroup() %>%
    # Keep only month number
    select(embrace_id, month)
  
  # Now get corresponding values for all gynecological measurements
  final_result <- result %>%
    rowwise() %>%
    mutate(
      !!!map(gyn_measurements, ~{
        quo({
          col_name <- paste0("gyn_", !!.x, "_", month, "m")
          if (col_name %in% names(data)) {
            data[[col_name]][data$embrace_id == embrace_id]
          } else {
            NA
          }
        })
      }) %>% 
      set_names(paste0("gyn_", gyn_measurements))
    ) %>%
    ungroup()
  
  # Return final dataset
  final_result %>%
    rename(timepoint_months = month) %>%
    select(embrace_id, timepoint_months, starts_with("gyn_"))
}