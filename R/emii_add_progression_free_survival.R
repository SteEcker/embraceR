#' Add Progression-Free Survival Status
#'
#' @description
#' This function adds a column indicating progression-free survival events,
#' defined as either any disease event (local failure, nodal control including PAO,
#' or systemic excluding PAO) OR death.
#'
#' @param .data The input dataframe containing patient event data
#' @return Original dataframe with an additional boolean column 'event_progression_free'
#' @keywords internal
#'
#' @import dplyr
emii_add_progression_free_survival <- function(.data) {
  .data %>%
    emii_add_disease_control() %>%
    mutate(
      event_progression_free = event_disease_control | event_vitalstatus == 1
    )
}


#' Add Progression-Free Survival Status for Embrace I
#'
#' @description
#' This function adds a column indicating progression-free survival events,
#' defined as either any disease event (local failure, regional event,
#' or systemic event) OR death.
#'
#' @param .data The input dataframe containing patient event data
#' @return Original dataframe with an additional boolean column 'event_progression_free'
#' @keywords internal
#'
#' @import dplyr
emi_add_progression_free_survival <- function(.data) {
  .data %>%
    # emi_add_disease_control() %>%
    mutate(
      # event_progression_free = event_disease_control | event_vitalstatus == 1
      event_progression_free = NA
    )
}

#' Add Progression-Free Survival Status with Verification
#'
#' @description
#' This function adds a column indicating progression-free survival events and returns
#' a tibble with the created column and all columns used for verification.
#'
#' @param .data The input dataframe containing patient event data
#' @param save_excel Logical indicating whether to save results as Excel file (default: FALSE)
#'
#' @return A tibble with the progression-free survival variable and relevant columns
#' @keywords internal
#'
#' @import dplyr
#' @importFrom openxlsx write.xlsx
#'
#' @examples
#' \dontrun{
#' data <- tibble(
#'   embrace_id = c("A1", "A2", "A3"),
#'   event_disease_control = c(TRUE, FALSE, FALSE),
#'   event_vitalstatus = c(0, 1, 0)
#' )
#' result <- emii_add_progression_free_survival_with_verification(data)
#' result <- emii_add_progression_free_survival_with_verification(data, save_excel = TRUE)
#' }
emii_add_progression_free_survival_with_verification <- function(.data, save_excel = FALSE) {
  # Add required variables first
  .data <- .data %>% 
    add_metastases() %>%
    emii_add_recurrent_nodes() %>%
    emii_add_pelvic_nodal_event() %>%
    emii_add_systemic_excl_pao() %>%
    emii_add_paraaortic_nodal() %>% 
    emii_add_nodalcontrol_incl_pao() %>%
    emii_add_disease_control()
  
  # Validate required columns
  required_cols <- c(
    "embrace_id",
    "event_disease_control",
    "event_vitalstatus"
  )
  
  missing_cols <- setdiff(required_cols, names(.data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Use the main function to add the progression-free survival variable
  result <- .data %>%
    emii_add_progression_free_survival() %>%
    select(
      embrace_id,
      event_disease_control,
      event_vitalstatus,
      event_progression_free
    )
  
  # Optionally save as Excel
  if (save_excel) {
    openxlsx::write.xlsx(result, "progression_free_survival_verification.xlsx")
  }
  
  result
} 