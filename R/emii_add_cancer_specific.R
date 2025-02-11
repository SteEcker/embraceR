#' Add Cancer-Specific Survival Status
#'
#' @description
#' This function adds a column indicating cancer-specific death events,
#' defined as death (Vital Status = 1) where the main cause of death
#' was primary cancer (codes 1 or 3).
#'
#' @param .data The input dataframe containing patient vital status data
#' @return Original dataframe with an additional boolean column 'event_cancer_specific'
#' @export
#'
#' @import dplyr
emii_add_cancer_specific <- function(.data) {
  .data %>%
    mutate(
      event_cancer_specific = event_vitalstatus == 1 & 
        vital_status_cause_of_death_vital_status %in% c(1, 3)
    )
}

#' Add Cancer-Specific Survival Status with Verification
#'
#' @description
#' This function adds a column indicating cancer-specific death events and returns
#' a tibble with the created column and all columns used for verification.
#'
#' @param .data The input dataframe containing patient vital status data
#' @param save_excel Logical indicating whether to save results as Excel file (default: FALSE)
#'
#' @return A tibble with the cancer-specific survival variable and relevant columns
#' @export
#'
#' @import dplyr
#' @importFrom openxlsx write.xlsx
#'
#' @examples
#' \dontrun{
#' data <- tibble(
#'   embrace_id = c("A1", "A2", "A3"),
#'   event_vitalstatus = c(1, 1, 0),
#'   vital_status_cause_of_death_vital_status = c(1, 2, NA)
#' )
#' result <- emii_add_cancer_specific_with_verification(data)
#' result <- emii_add_cancer_specific_with_verification(data, save_excel = TRUE)
#' }
emii_add_cancer_specific_with_verification <- function(.data, save_excel = FALSE) {
  # Validate required columns
  required_cols <- c(
    "embrace_id",
    "event_vitalstatus",
    "vital_status_cause_of_death_vital_status"
  )
  
  missing_cols <- setdiff(required_cols, names(.data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Use the main function to add the cancer-specific survival variable
  result <- .data %>%
    emii_add_cancer_specific() %>%
    select(
      embrace_id,
      event_vitalstatus,
      vital_status_cause_of_death_vital_status,
      event_cancer_specific
    )
  
  # Optionally save as Excel
  if (save_excel) {
    openxlsx::write.xlsx(result, "cancer_specific_survival_verification.xlsx")
  }
  
  result
}
