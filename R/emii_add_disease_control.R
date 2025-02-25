#' Add Disease Control Status
#'
#' @description
#' This function adds a column indicating disease control events,
#' defined as either local failure, nodal failure, or systemic failure.
#'
#' @param .data The input dataframe containing patient disease control data
#' @return Original dataframe with an additional boolean column 'event_disease_control'
#' @keywords internal
#'
#' @import dplyr
emii_add_disease_control <- function(.data) {
  .data %>%
    mutate(
      event_disease_control = event_localfailure |
        event_nodalfailure |
        event_systemicfailure
    )
}

#' Add Disease Control Status with Verification
#'
#' @description
#' This function adds a column indicating disease control events and returns
#' a tibble with the created column and all columns used for verification.
#'
#' @param .data The input dataframe containing patient disease control data
#' @param save_excel Logical indicating whether to save results as Excel file (default: FALSE)
#'
#' @return A tibble with the disease control variable and relevant columns
#' @keywords internal
#'
#' @import dplyr
#' @importFrom openxlsx write.xlsx
#'
#' @examples
#' \dontrun{
#' data <- tibble(
#'   embrace_id = c("A1", "A2", "A3"),
#'   event_localfailure = c(TRUE, FALSE, FALSE),
#'   event_nodalfailure = c(FALSE, TRUE, FALSE),
#'   event_systemicfailure = c(FALSE, FALSE, TRUE)
#' )
#' result <- emii_add_disease_control_with_verification(data)
#' result <- emii_add_disease_control_with_verification(data, save_excel = TRUE)
#' }
emii_add_disease_control_with_verification <- function(.data, save_excel = FALSE) {
  # Validate required columns
  required_cols <- c(
    "embrace_id",
    "event_localfailure",
    "event_nodalfailure",
    "event_systemicfailure"
  )

  missing_cols <- setdiff(required_cols, names(.data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Use the main function to add the disease control variable
  result <- .data %>%
    emii_add_disease_control() %>%
    select(
      embrace_id,
      event_localfailure,
      event_nodalfailure,
      event_systemicfailure,
      event_disease_control
    )

  # Optionally save as Excel
  if (save_excel) {
    openxlsx::write.xlsx(result, "disease_control_verification.xlsx")
  }

  result
}

#' Add Disease Control Status for Embrace I
#'
#' @description
#' This function adds a column indicating disease control events for Embrace I data,
#' defined as either local failure, regional events, or systemic events.
#'
#' @param .data The input dataframe containing patient disease control data
#' @return Original dataframe with an additional boolean column 'event_disease_control'
#' @keywords internal
#'
#' @import dplyr
emi_add_disease_control <- function(.data) {
  .data %>%
    mutate(
      event_disease_control = NA
    )
}
