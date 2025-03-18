#' Add Systemic Control Status (Excluding Para-aortic)
#'
#' @description
#' This function adds a column indicating whether a patient had systemic failure,
#' defined as having systemic failure AND at least one of the following:
#' supraclavicular nodes, mediastinal nodes, liver metastases, bone metastases,
#' brain metastases, lung metastases, abdominal carcinomatosis, or other metastases.
#'
#' @param .data The input dataframe containing patient failure data
#' @return Original dataframe with an additional boolean column 'event_systemic_excl_pao'
#' @export
#'
#' @import dplyr
emii_add_systemic_excl_pao <- function(.data) {
  .data %>%
    add_metastases() %>%
    mutate(
      event_systemic_excl_pao = event_systemicfailure & 
        (has_supraclavicular_nodes |
         has_mediastinal_nodes |
         has_liver_metastases |
         has_bone_metastases |
         has_brain_metastases |
         has_lung_metastases |
         has_abdominal_carcinomatosis |
         has_other_metastases)
    )
}

#' Add Systemic Control Status (Excluding Para-aortic) with Verification
#'
#' @description
#' This function adds a column indicating whether a patient had systemic failure
#' and returns a tibble with the created column and all columns used for verification.
#'
#' @param .data The input dataframe containing patient failure data
#' @param save_excel Logical indicating whether to save results as Excel file (default: FALSE)
#'
#' @return A tibble with the systemic control variable and relevant columns
#' @export
#'
#' @import dplyr
#' @importFrom openxlsx write.xlsx
#'
#' @examples
#' \dontrun{
#' data <- tibble(
#'   embrace_id = c("A1", "A2"),
#'   event_systemicfailure = c(TRUE, FALSE),
#'   has_liver_metastases = c(TRUE, FALSE),
#'   has_bone_metastases = c(FALSE, TRUE)
#' )
#' result <- emii_add_systemic_excl_pao_with_verification(data)
#' result <- emii_add_systemic_excl_pao_with_verification(data, save_excel = TRUE)
#' }
emii_add_systemic_excl_pao_with_verification <- function(.data, save_excel = FALSE) {
  .data <- .data %>% 
    add_metastases()

  # Validate required columns
  required_cols <- c(
    "embrace_id",
    "event_systemicfailure",
    "has_paraaortic_nodes_above_l2",
    "has_supraclavicular_nodes",
    "has_mediastinal_nodes",
    "has_liver_metastases",
    "has_bone_metastases",
    "has_brain_metastases",
    "has_lung_metastases",
    "has_abdominal_carcinomatosis",
    "has_other_metastases"
  )
  
  missing_cols <- setdiff(required_cols, names(.data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Use the main function to add the systemic control variable
  result <- .data %>%
    emii_add_systemic_excl_pao() %>%
    select(
      embrace_id,
      event_systemicfailure,
      has_paraaortic_nodes_above_l2,
      has_supraclavicular_nodes,
      has_mediastinal_nodes,
      has_liver_metastases,
      has_bone_metastases,
      has_brain_metastases,
      has_lung_metastases,
      has_abdominal_carcinomatosis,
      has_other_metastases,
      event_systemic_excl_pao
    )
  
  # Optionally save as Excel
  if (save_excel) {
    openxlsx::write.xlsx(result, "systemic_control_excl_pao_verification.xlsx")
  }
  
  result
}
