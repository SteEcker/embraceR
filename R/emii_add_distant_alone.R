#' Add Distant Alone Status
#'
#' @description
#' This function adds a column indicating whether a patient had distant metastases alone,
#' defined as having systemic failure without para-aortic nodes and no local or nodal failures.
#'
#' @param .data The input dataframe containing patient failure data
#' @return Original dataframe with an additional boolean column 'distant_alone'
#' @keywords internal
#'
#' @import dplyr
emii_add_distant_alone <- function(.data) {
  .data %>%
    mutate(
      has_other_metas = event_systemicfailure == 1 & (!has_paraaortic_nodes_above_l2),
      event_distant_alone = has_other_metas & !event_paraaortic_nodal & !event_localfailure & !event_pelvic_nodal
    )
}

#' Add Distant Alone Status with Verification
#'
#' @description
#' This function adds a column indicating whether a patient had distant metastases alone
#' and returns a tibble with the created column and all columns used for verification.
#'
#' @param .data The input dataframe containing patient failure data
#' @param save_excel Logical indicating whether to save results as Excel file (default: FALSE)
#'
#' @return A tibble with the distant alone variable and relevant columns
#' @keywords internal
#'
#' @import dplyr
#' @importFrom openxlsx write.xlsx
#'
#' @examples
#' \dontrun{
#' data <- tibble(
#'   embrace_id = c("A1", "A2"),
#'   event_systemicfailure = c(TRUE, FALSE),
#'   has_paraaortic_nodes_above_l2 = c(FALSE, TRUE),
#'   event_paraaortic_nodal = c(FALSE, FALSE),
#'   event_localfailure = c(FALSE, FALSE),
#'   event_pelvic_nodal = c(FALSE, FALSE)
#' )
#' result <- add_distant_alone_with_verification(data)
#' result <- add_distant_alone_with_verification(data, save_excel = TRUE)
#' }
emii_add_distant_alone_with_verification <- function(.data, save_excel = FALSE) {
  # Validate required columns
  required_cols <- c(
    "embrace_id",
    "event_systemicfailure",
    "event_localfailure"
  )
  
  missing_cols <- setdiff(required_cols, names(.data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Use the main function to add the distant alone variable
  result <- .data %>%
    add_metastases() %>%
    emii_add_recurrent_nodes() %>%
    emii_add_paraaortic_nodal() %>%
    emii_add_pelvic_nodal_event() %>%
    emii_add_distant_alone() %>%
    select(
      embrace_id,
      event_systemicfailure,
      has_paraaortic_nodes_above_l2,
      event_paraaortic_nodal,
      event_localfailure,
      event_pelvic_nodal,
      event_distant_alone
    )
  
  # Optionally save as Excel
  if (save_excel) {
    openxlsx::write.xlsx(result, "distant_alone_verification.xlsx")
  }
  
  result
}