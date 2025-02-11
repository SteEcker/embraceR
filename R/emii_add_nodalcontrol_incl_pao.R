#' Add Nodal Control (incl. para-aortic) Event Variable
#'
#' This function creates a new disease event variable for the endpoint
#' "Nodal control (incl. para-aortic)" based on EMBRACE-II data.
#'
#' @param data A tibble containing EMBRACE-II data
#'
#' @return A tibble with the new disease event variable added
#'
#' @import dplyr
#' @importFrom rlang sym
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' 
#' # Create sample data
#' data <- tibble(
#'   event_pelvic_nodal = c(0L, 1L, 0L, 1L),
#'   event_paraaortic_nodal = c(0L, 0L, 1L, 1L)
#' )
#' 
#' # Apply the function
#' result <- emii_add_nodalcontrol_incl_pao(data)
#' print(result)
emii_add_nodalcontrol_incl_pao <- function(data) {
  # Validate input
  validate_input(data)
  
  # Create the new variable based on the simplified definition
  data %>%
    mutate(
      event_nodalcontrol_incl_pao = event_pelvic_nodal | event_paraaortic_nodal
    )
}

#' Validate Input for add_nodal_control_incl_pao Function
#'
#' @param data A tibble containing EMBRACE-II data
#'
#' @return NULL
#' @noRd
validate_input <- function(data) {
  if (!is.data.frame(data)) {
    stop("Input 'data' must be a data frame or tibble.")
  }
  
  required_vars <- c("event_pelvic_nodal", "event_paraaortic_nodal")
  missing_vars <- setdiff(required_vars, names(data))
  
  if (length(missing_vars) > 0) {
    stop("Required variables not found in the data: ", 
         paste(missing_vars, collapse = ", "))
  }
}

#' Add Nodal Control (incl. para-aortic) Event Variable with Verification
#'
#' This function creates a new disease event variable and returns a tibble with 
#' the created column and all columns used by the function for verification.
#'
#' @param data A tibble containing EMBRACE-II data
#' @param save_excel Logical indicating whether to save results as Excel file (default: FALSE)
#'
#' @return A tibble with the new disease event variable and relevant columns
#'
#' @import dplyr
#' @importFrom openxlsx write.xlsx
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data <- tibble(
#'   embrace_id = c("A1", "A2"),
#'   event_pelvic_nodal = c(0L, 1L),
#'   event_paraaortic_nodal = c(0L, 1L)
#' )
#' result <- emii_add_nodalcontrol_incl_pao_with_verification(data)
#' result <- emii_add_nodalcontrol_incl_pao_with_verification(data, save_excel = TRUE)
#' }
emii_add_nodalcontrol_incl_pao_with_verification <- function(data, save_excel = FALSE) {
  
  # Add necessary variables by calling other functions
  data <- data %>%
    add_metastases() %>% 
    emii_add_recurrent_nodes() %>% 
    emii_add_pelvic_nodal_event() %>%
    emii_add_paraaortic_nodal()
  
  # Create the new variable based on the simplified definition
  result <- data %>%
    mutate(
      event_nodalcontrol_incl_pao = event_pelvic_nodal | event_paraaortic_nodal
    ) %>%
    select(
      embrace_id,
      event_pelvic_nodal,
      event_paraaortic_nodal,
      event_nodalcontrol_incl_pao
    )
  
  # Optionally save as Excel
  if (save_excel) {
    openxlsx::write.xlsx(result, "nodal_control_incl_pao_verification.xlsx")
  }
  
  result
}

#' Add Locoregional Event Status (Pelvic/PAO)
#'
#' @description
#' This function adds columns indicating whether a patient had a locoregional event,
#' defined as either a local failure or nodal control including para-aortic, and
#' whether the event was locoregional alone, defined as locoregional and not systemic
#' excluding para-aortic.
#'
#' @param .data The input dataframe containing patient event data
#' @return Original dataframe with additional boolean columns 'event_locoregional' and 'event_locoregional_alone'
#' @export
#'
#' @import dplyr
add_locoregional_event <- function(.data) {
  .data %>%
    mutate(
      event_locoregional = event_localfailure | event_nodalcontrol_incl_pao,
      event_locoregional_alone = event_locoregional & (!event_systemic_excl_pao)  # New column
    )
}

#' Add Locoregional Event Status with Verification
#'
#' @description
#' This function adds columns indicating whether a patient had a locoregional event
#' and whether the event was locoregional alone. It returns a tibble with the created
#' columns and all columns used for verification.
#'
#' @param .data The input dataframe containing patient event data
#' @param save_excel Logical indicating whether to save results as Excel file (default: FALSE)
#'
#' @return A tibble with the locoregional event variables and relevant columns
#' @export
#'
#' @import dplyr
#' @importFrom openxlsx write.xlsx
#'
#' @examples
#' \dontrun{
#' data <- tibble(
#'   embrace_id = c("A1", "A2"),
#'   event_localfailure = c(TRUE, FALSE),
#'   event_nodalcontrol_incl_pao = c(FALSE, TRUE),
#'   event_systemic_excl_pao = c(FALSE, TRUE)
#' )
#' result <- add_locoregional_event_with_verification(data)
#' result <- add_locoregional_event_with_verification(data, save_excel = TRUE)
#' }
add_locoregional_event_with_verification <- function(.data, save_excel = FALSE) {
  # Validate required columns
  required_cols <- c(
    "embrace_id",
    "event_localfailure",
    "event_nodalcontrol_incl_pao",
    "event_systemic_excl_pao"  # Ensure this column is present
  )
  
  missing_cols <- setdiff(required_cols, names(.data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Use the main function to add the locoregional event variable
  result <- .data %>%
    emii_add_recurrent_nodes() %>%
    add_metastases() %>%
    emii_add_pelvic_nodal_event() %>%
    emii_add_paraaortic_nodal() %>%
    emii_add_nodalcontrol_incl_pao() %>%
    emii_add_systemic_excl_pao() %>%
    add_locoregional_event() %>%
    select(
      embrace_id,
      event_localfailure,
      event_nodalcontrol_incl_pao,
      event_systemic_excl_pao,
      event_locoregional,
      event_locoregional_alone  # Include the new column in the result
    )
  
  # Optionally save as Excel
  if (save_excel) {
    openxlsx::write.xlsx(result, "locoregional_event_verification.xlsx")
  }
  
  result
}