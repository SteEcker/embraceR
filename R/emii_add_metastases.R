#' Add Metastases Variables
#'
#' This function adds variables indicating the presence of metastases
#' at various locations based on EMBRACE-II data.
#'
#' @param data A tibble containing EMBRACE-II data
#'
#' @return A tibble with all metastases indicator variables added
#'
#' @import dplyr
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' data <- tibble(
#'   supraclavicular_nodes_3m = c(0, 1, 0, 1),
#'   supraclavicular_nodes_6m = c(0, 0, 1, 1)
#' )
#' result <- add_metastases(data)
#' print(result)
#' }
add_metastases <- function(data) {
  # Define metastases locations and their base pattern
  metastases_patterns <- c(
    "paraaortic_nodes_above_l2",
    "supraclavicular_nodes",
    "mediastinal_nodes",
    "liver_metastases",
    "bone_metastases",
    "brain_metastases",
    "lung_metastases",
    "abdominal_carcinomatosis",
    "other_metastases"
  )
  
  # Initialize result
  result <- data
  
  # Create all indicator variables at once
  for (pattern in metastases_patterns) {
    # Get all variables for this metastasis type
    vars <- grep(paste0(pattern, "_\\d+m"), names(data), value = TRUE)
    
    if (length(vars) > 0) {
      # Create new column name
      new_col <- paste0("has_", pattern)
      
      # Vectorized operation to check for any 1s in the relevant columns
      result[[new_col]] <- apply(
        result[vars] == 1, 
        1, 
        any, 
        na.rm = TRUE
      )
    }
  }
  
  result
}

#' Add Metastases Variables with Verification
#'
#' @param data A tibble containing EMBRACE-II data
#' @param save_excel Logical indicating whether to save results as Excel file
#'
#' @return A tibble with all metastases indicators and relevant columns
#'
#' @import dplyr
#' @importFrom openxlsx write.xlsx
#'
#' @keywords internal
add_metastases_with_verification <- function(data, save_excel = FALSE) {
  # Define metastases patterns (moved to top level to avoid redefining)
  metastases_patterns <- c(
    "paraaortic_nodes_above_l2",
    "supraclavicular_nodes",
    "mediastinal_nodes",
    "liver_metastases",
    "bone_metastases",
    "brain_metastases",
    "lung_metastases",
    "abdominal_carcinomatosis",
    "other_metastases"
  )
  
  # More efficient pattern matching for all variables at once
  all_vars <- grep(
    paste0("(", paste(metastases_patterns, collapse = "|"), ")_\\d+m"),
    names(data),
    value = TRUE
  )
  
  if (length(all_vars) == 0) {
    stop("No metastases variables found in the data.")
  }
  
  # Pre-compute the expected output columns
  has_cols <- paste0("has_", metastases_patterns)
  
  # Validate required columns
  required_cols <- c("embrace_id")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Use the main function and select columns more efficiently
  result <- add_metastases(data) %>%
    select(embrace_id, all_of(all_vars), all_of(has_cols))
  
  # Optionally save as Excel
  if (save_excel) {
    openxlsx::write.xlsx(result, "metastases_verification.xlsx")
  }
  
  result
} 