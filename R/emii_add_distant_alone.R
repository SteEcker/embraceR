#' Add Distant Alone Status
#'
#' @description
#' This function adds a column indicating whether a patient had distant metastases alone,
#' defined as having systemic failure excluding para-aortic nodes (event_systemic_excl_pao) 
#' AND no locoregional failure (event_locoregional != 1).
#'
#' @param .data The input dataframe containing patient failure data
#' @return Original dataframe with an additional boolean column 'distant_alone'
#' @export
#'
#' @import dplyr
#' @importFrom rlang .data
emii_add_distant_alone <- function(.data) {
  .data %>%
    mutate(
      # Distant alone = distant failure (excl. PAO) AND locoregional failure != 1
      event_distant_alone = event_systemic_excl_pao & (!event_locoregional)
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
#' @export
#'
#' @import dplyr
#' @importFrom openxlsx write.xlsx
#' @importFrom rlang .data
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
    "event_systemicfailure"
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
    emii_add_nodalcontrol_incl_pao() %>%
    add_locoregional_event() %>%
    emii_add_systemic_excl_pao() %>%
    emii_add_distant_alone() %>%
    select(
      embrace_id,
      # Base events
      event_localfailure,
      event_systemicfailure,
      # Components of locoregional
      event_nodalcontrol_incl_pao,
      event_locoregional,
      # Components of systemic_excl_pao
      has_paraaortic_nodes_above_l2,
      event_systemic_excl_pao,
      # Final endpoint
      event_distant_alone
    )
  
  # Optionally save as Excel
  if (save_excel) {
    openxlsx::write.xlsx(result, "distant_alone_verification.xlsx")
  }
  
  result
}