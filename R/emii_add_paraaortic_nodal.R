#' Add Para-aortic Nodal Control Status
#'
#' @description
#' This function adds a column indicating whether a patient had para-aortic nodal involvement,
#' defined as either having para-aortic metastases or para-aortic nodal failure during follow-up.
#'
#' @param .data The input dataframe containing patient failure data
#' @return Original dataframe with an additional boolean column 'event_paraaortic_nodal'
#' @keywords internal
#'
#' @import dplyr
emii_add_paraaortic_nodal <- function(.data) {
  .data %>%
    mutate(
      event_paraaortic_nodal = has_paraaortic_nodes_above_l2 | has_Para.Aortic_followup
    )
}

#' Add Para-aortic Nodal Control Status with Verification
#'
#' @description
#' This function adds a column indicating whether a patient had para-aortic nodal involvement
#' and returns a tibble with the created column and all columns used for verification.
#'
#' @param .data The input dataframe containing patient failure data
#' @param save_excel Logical indicating whether to save results as Excel file (default: FALSE)
#'
#' @return A tibble with the para-aortic nodal control variable and relevant columns
#' @keywords internal
#'
#' @import dplyr
#' @importFrom openxlsx write.xlsx
#'
#' @examples
#' \dontrun{
#' data <- tibble(
#'   embrace_id = c("A1", "A2"),
#'   has_paraaortic_metastases = c(TRUE, FALSE),
#'   has_Para.Aortic_followup = c(FALSE, TRUE)
#' )
#' result <- emii_add_paraaortic_nodal_with_verification(data)
#' result <- emii_add_paraaortic_nodal_with_verification(data, save_excel = TRUE)
#' }
emii_add_paraaortic_nodal_with_verification <- function(.data, save_excel = FALSE) {
  .data <- .data %>% 
    add_metastases() %>%
    emii_add_recurrent_nodes()
    
  # Validate required columns
  required_cols <- c(
    "embrace_id",
    "has_paraaortic_nodes_above_l2",
    "has_Para.Aortic_followup"
  )
  
  missing_cols <- setdiff(required_cols, names(.data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Use the main function to add the para-aortic nodal control variable
  result <- .data %>%
    emii_add_paraaortic_nodal() %>%
    select(
      embrace_id,
      has_paraaortic_nodes_above_l2,
      has_Para.Aortic_followup,
      event_paraaortic_nodal
    )
  
  # Optionally save as Excel
  if (save_excel) {
    openxlsx::write.xlsx(result, "paraaortic_nodal_verification.xlsx")
  }
  
  result
}
