#' Recode and convert columns based on the mapping
#'
#' This function converts columns to factors and recodes their levels based on the provided mapping.
#' It can process both specific column names and columns with common base names.
#'
#' @param df A data frame containing the columns to be recoded and converted.
#' @param mapping A list containing the old and new levels for each column or base name.
#' @param stop_on_missing_col A logical value. If TRUE, the function stops if a column is missing in the data frame. If FALSE, it skips the missing columns.
#' @return The data frame with the specified columns recoded and converted to factors.
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

#' Recode and convert columns based on the mapping
#'
#' @export
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
