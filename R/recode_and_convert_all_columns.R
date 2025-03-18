#' Recode and convert columns based on the mapping
#'
#' This function converts columns to factors and recodes their levels based on the provided mapping.
#' It can process both specific column names and columns with common base names.
#'
#' @param df A data frame containing the columns to be recoded and converted
#' @param stop_on_missing_col Logical, if TRUE, stops if a column is missing; if FALSE, skips missing columns (default: FALSE)
#'
#' @return A data frame with the specified columns recoded and converted to factors
#' @export
#' @import here
#' @importFrom jsonlite fromJSON
#'
#' @examples
#' \dontrun{
#'   # Load data
#'   data <- load_embrace_ii()
#'   
#'   # Recode and convert all columns
#'   recoded_data <- recode_and_convert_all_columns(data)
#'   
#'   # Recode with strict checking
#'   recoded_data <- recode_and_convert_all_columns(data, stop_on_missing_col = TRUE)
#' }
recode_and_convert_all_columns <- function(df, stop_on_missing_col = FALSE) {
  mapping_path <- here::here("data_raw", "embrace_II", "factor_labels.json")
  mapping <- jsonlite::fromJSON(mapping_path)

  for (name in names(mapping)) {
    matching_columns <- grep(paste0("^", name, "(_[A-Za-z0-9]+|_baseline_morbidity|_bm_4w|_bm_eort)?$"), names(df), value = TRUE)

    if (length(matching_columns) == 0) {
      message("No columns found for base name: ", name)
      if (stop_on_missing_col) {
        stop("Missing column(s) for base name: ", name)
      }
    } else {
      for (col in matching_columns) {
        df <- recode_and_convert_column(df, col, mapping[[name]])
      }
    }
  }

  return(df)
}

#' Recode and convert a single column based on the mapping
#'
#' @param df A data frame containing the column to be recoded
#' @param col_name Character, name of the column to recode
#' @param mapping A list containing the old and new levels for the column
#'
#' @return The data frame with the specified column recoded and converted to factor
#' @export
recode_and_convert_column <- function(df, col_name, mapping) {
  if (col_name %in% names(df)) {
    # cat("Recoding and converting column:", col_name, "\n")
    df[[col_name]] <- as.factor(df[[col_name]])
    old_levels <- names(mapping)
    new_levels <- unname(unlist(mapping))
    df[[col_name]] <- recode_factor_levels(df[[col_name]], old_levels, new_levels)
  } else {
    cat("Column", col_name, "not found in the data frame.\n")
  }
  return(df)
}
