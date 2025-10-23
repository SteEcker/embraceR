#' Add Local Failure Event with Verification
#'
#' @description
#' This function adds a column indicating local failure events and returns
#' a tibble with the created column and all columns used for verification.
#'
#' @param .data The input dataframe containing disease status data
#' @param save_excel Logical indicating whether to save results as Excel file (default: FALSE)
#'
#' @return A tibble with the local failure event variable and relevant columns
#' @export
#'
#' @import dplyr
#' @importFrom openxlsx write.xlsx
#'
#' @examples
#' \dontrun{
#' data <- tibble(
#'   embrace_id = c("A1", "A2"),
#'   disease_local_status_3m = c(0, 2),
#'   disease_local_status_6m = c(0, 0)
#' )
#' result <- add_local_failure_event_with_verification(data)
#' result <- add_local_failure_event_with_verification(data, save_excel = TRUE)
#' }
add_local_failure_event_with_verification <- function(.data, save_excel = FALSE) {
  # Get all disease_local_status columns
  local_status_cols <- .data %>% 
    select(starts_with('disease_local_status_')) %>% 
    colnames()
  
  if (length(local_status_cols) == 0) {
    stop("No disease_local_status_* columns found in the data.")
  }
  
  # Validate required columns
  required_cols <- c("embrace_id")
  missing_cols <- setdiff(required_cols, names(.data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Use the main function to add the local failure event variable
  result <- .data %>%
    add_local_failure_event() %>%
    select(
      embrace_id,
      all_of(local_status_cols),
      event_localfailure
    )
  
  # Optionally save as Excel
  if (save_excel) {
    openxlsx::write.xlsx(result, "local_failure_event_verification.xlsx")
  }
  
  result
}

#' Add Nodal Failure Event with Verification
#'
#' @description
#' This function adds a column indicating nodal failure events and returns
#' a tibble with the created column and all columns used for verification.
#'
#' @param .data The input dataframe containing disease status data
#' @param save_excel Logical indicating whether to save results as Excel file (default: FALSE)
#'
#' @return A tibble with the nodal failure event variable and relevant columns
#' @export
#'
#' @import dplyr
#' @importFrom openxlsx write.xlsx
#'
#' @examples
#' \dontrun{
#' data <- tibble(
#'   embrace_id = c("A1", "A2"),
#'   disease_nodal_status_3m = c(0, 2),
#'   disease_nodal_status_6m = c(0, 0)
#' )
#' result <- add_nodal_failure_event_with_verification(data)
#' result <- add_nodal_failure_event_with_verification(data, save_excel = TRUE)
#' }
add_nodal_failure_event_with_verification <- function(.data, save_excel = FALSE) {
  # Get all disease_nodal_status columns
  nodal_status_cols <- .data %>% 
    select(starts_with('disease_nodal_status_')) %>% 
    colnames()
  
  if (length(nodal_status_cols) == 0) {
    stop("No disease_nodal_status_* columns found in the data.")
  }
  
  # Validate required columns
  required_cols <- c("embrace_id")
  missing_cols <- setdiff(required_cols, names(.data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Use the main function to add the nodal failure event variable
  result <- .data %>%
    add_nodal_failure_event() %>%
    select(
      embrace_id,
      all_of(nodal_status_cols),
      event_nodalfailure
    )
  
  # Optionally save as Excel
  if (save_excel) {
    openxlsx::write.xlsx(result, "nodal_failure_event_verification.xlsx")
  }
  
  result
}

#' Add Systemic Failure Event with Verification
#'
#' @description
#' This function adds a column indicating systemic failure events and returns
#' a tibble with the created column and all columns used for verification.
#'
#' @param .data The input dataframe containing disease status data
#' @param save_excel Logical indicating whether to save results as Excel file (default: FALSE)
#'
#' @return A tibble with the systemic failure event variable and relevant columns
#' @export
#'
#' @import dplyr
#' @importFrom openxlsx write.xlsx
#'
#' @examples
#' \dontrun{
#' data <- tibble(
#'   embrace_id = c("A1", "A2"),
#'   disease_systemic_status_3m = c(0, 2),
#'   disease_systemic_status_6m = c(0, 0)
#' )
#' result <- add_systemic_failure_event_with_verification(data)
#' result <- add_systemic_failure_event_with_verification(data, save_excel = TRUE)
#' }
add_systemic_failure_event_with_verification <- function(.data, save_excel = FALSE) {
  # Get all disease_systemic_status columns
  systemic_status_cols <- .data %>% 
    select(starts_with('disease_systemic_status_')) %>% 
    colnames()
  
  if (length(systemic_status_cols) == 0) {
    stop("No disease_systemic_status_* columns found in the data.")
  }
  
  # Validate required columns
  required_cols <- c("embrace_id", "study")
  missing_cols <- setdiff(required_cols, names(.data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Use the main function to add the systemic failure event variable
  result <- .data %>%
    add_systemic_failure_event() %>%
    select(
      embrace_id,
      study,
      all_of(systemic_status_cols),
      event_systemicfailure
    )
  
  # Optionally save as Excel
  if (save_excel) {
    openxlsx::write.xlsx(result, "systemic_failure_event_verification.xlsx")
  }
  
  result
}

#' Add Vital Status Event with Verification
#'
#' @description
#' This function adds a column indicating vital status events and returns
#' a tibble with the created column and all columns used for verification.
#'
#' @param .data The input dataframe containing vital status data
#' @param save_excel Logical indicating whether to save results as Excel file (default: FALSE)
#'
#' @return A tibble with the vital status event variable and relevant columns
#' @export
#'
#' @import dplyr
#' @importFrom openxlsx write.xlsx
#'
#' @examples
#' \dontrun{
#' data <- tibble(
#'   embrace_id = c("A1", "A2", "A3"),
#'   vital_status = c(1, NA, NA),
#'   latest_assessment_date_disease = c(NA, as.Date("2022-01-01"), NA)
#' )
#' result <- add_vitalstatus_event_with_verification(data)
#' result <- add_vitalstatus_event_with_verification(data, save_excel = TRUE)
#' }
add_vitalstatus_event_with_verification <- function(.data, save_excel = FALSE) {
  # Validate required columns
  required_cols <- c(
    "embrace_id",
    "vital_status",
    "latest_assessment_date_disease"
  )
  
  missing_cols <- setdiff(required_cols, names(.data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Use the main function to add the vital status event variable
  result <- .data %>%
    add_vitalstatus_event() %>%
    select(
      embrace_id,
      vital_status,
      latest_assessment_date_disease,
      event_vitalstatus
    )
  
  # Optionally save as Excel
  if (save_excel) {
    openxlsx::write.xlsx(result, "vitalstatus_event_verification.xlsx")
  }
  
  result
}

