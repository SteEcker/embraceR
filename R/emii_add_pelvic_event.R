#' Add Composite Pelvic Event Status
#'
#' @description
#' This function adds a column indicating whether a patient had any pelvic event,
#' defined as either a local failure or a pelvic nodal event.
#'
#' @param .data The input dataframe containing patient event data
#' @return Original dataframe with an additional boolean column 'event_pelvic'
#' @export
#'
#' @import dplyr
emii_add_pelvic_event <- function(.data) {
  .data %>%
    mutate(
      event_pelvic = event_localfailure | event_pelvic_nodal
    )
}


#' Add Pelvic Event Status with Verification
#'
#' @description
#' This function adds a column indicating whether a patient had any pelvic event
#' and returns a tibble with the created column and all columns used for verification.
#'
#' @param .data The input dataframe containing patient event data
#' @param save_excel Logical indicating whether to save results as Excel file (default: FALSE)
#'
#' @return A tibble with the pelvic event variable and relevant columns
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
#'   event_pelvic_nodal = c(FALSE, TRUE)
#' )
#' result <- emii_add_pelvic_event_with_verification(data)
#' result <- emii_add_pelvic_event_with_verification(data, save_excel = TRUE)
#' }
emii_add_pelvic_event_with_verification <- function(.data, save_excel = FALSE) {
  .data <- .data %>%
    add_metastases() %>%
    emii_add_recurrent_nodes() %>%
    emii_add_pelvic_nodal_event()
  
  # Validate required columns
  required_cols <- c(
    "embrace_id",
    "event_localfailure",
    "event_pelvic_nodal"
  )
  
  missing_cols <- setdiff(required_cols, names(.data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Use the main function to add the pelvic event variable
  result <- .data %>%
    emii_add_pelvic_event() %>%
    select(
      embrace_id,
      event_localfailure,
      event_pelvic_nodal,
      event_pelvic
    )
  
  # Optionally save as Excel
  if (save_excel) {
    openxlsx::write.xlsx(result, "pelvic_event_verification.xlsx")
  }
  
  result
}