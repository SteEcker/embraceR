#' Add Pelvic Nodal Event Status
#'
#' @description
#' This function adds a column indicating whether a patient had a pelvic nodal event.
#' A pelvic nodal event is defined as having event_nodalfailure AND at least one positive
#' node in any of the following locations: external iliac, internal iliac, common iliac,
#' parametrial/paracervical regions (left or right), or other pelvic nodal locations.
#'
#' @param .data The input dataframe containing patient node data
#'
#' @return Original dataframe with an additional boolean column 'event_pelvic_nodal'
#' @export
#'
#' @import dplyr
emii_add_pelvic_nodal_event <- function(.data) {
  .data %>%
    mutate(
      event_pelvic_nodal = event_nodalfailure & 
        (has_L.ext.iliac_followup | 
         has_L.int.iliac_followup | 
         has_L.com.iliac_followup |
         has_R.ext.iliac_followup |
         has_R.int.iliac_followup |
         has_R.com.iliac_followup |
         has_R.parame.paracervix_followup |
         has_L.parame.paracervix_followup |
         has_other_followup
         )
    )
}

#' Add Pelvic Nodal Event Status with Verification
#'
#' @description
#' This function adds a column indicating whether a patient had a pelvic nodal event
#' and returns a tibble with the created column and all columns used for verification.
#'
#' @param .data The input dataframe containing patient node data
#' @param save_excel Logical indicating whether to save results as Excel file (default: FALSE)
#'
#' @return A tibble with the pelvic nodal event variable and relevant columns
#' @export
#'
#' @import dplyr
#' @importFrom openxlsx write.xlsx
#'
#' @examples
#' \dontrun{
#' data <- tibble(
#'   embrace_id = c("A1", "A2"),
#'   event_nodalfailure = c(TRUE, FALSE),
#'   has_Para.Aortic_followup = c(FALSE, TRUE),
#'   has_other_followup = c(FALSE, FALSE),
#'   has_L.groin_followup = c(FALSE, FALSE),
#'   has_R.groin_followup = c(FALSE, FALSE)
#' )
#' result <- emii_add_pelvic_nodal_event_with_verification(data)
#' result <- emii_add_pelvic_nodal_event_with_verification(data, save_excel = TRUE)
#' }
emii_add_pelvic_nodal_event_with_verification <- function(.data, save_excel = FALSE) {
  # Validate required columns
  required_cols <- c(
    "has_L.ext.iliac_followup",
    "has_L.int.iliac_followup",
    "has_L.com.iliac_followup",
    "has_R.ext.iliac_followup",
    "has_R.int.iliac_followup",
    "has_R.com.iliac_followup",
    "has_R.parame.paracervix_followup",
    "has_L.parame.paracervix_followup",
    "has_Para.Aortic_followup",
    "has_other_followup",
    "has_L.groin_followup",
    "has_R.groin_followup"
  )

  
  .data <- emii_add_recurrent_nodes(.data)
  
  
  missing_cols <- setdiff(required_cols, names(.data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Use the main function to add the pelvic nodal event variable
  result <- .data %>%
    emii_add_pelvic_nodal_event() %>%
    select(
      embrace_id,
      event_nodalfailure,
      # All has_ columns
      starts_with("has_"),
      # The calculated event
      event_pelvic_nodal
    )
  
  # Optionally save as Excel
  if (save_excel) {
    openxlsx::write.xlsx(result, "pelvic_nodal_event_verification.xlsx")
  }
  
  result
}

