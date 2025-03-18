#' Map Columns in a Dataframe Based on a Mapping Table
#'
#' This function renames columns in a given dataframe based on a mapping table.
#' The mapping table should contain the old column names and the new column names
#' that they should be mapped to.
#'
#' @param data The dataframe whose columns are to be renamed.
#' @param mapping A dataframe containing at least two columns:
#'   `embrace_i_column` for the Embrace I dataset column names, and
#'   `embrace_ii_column` for the Embrace II dataset column names.
#' @param study name of study
#'
#' @return A dataframe with columns renamed based on the mapping table.
#' @export
#'
#' @examples
#' \dontrun{
#'   mapping <- data.frame(embrace_i_column = c('col1', 'col2'),
#'                         embrace_ii_column = c('column1', 'column2'),
#'                         new_combined_name = c('new_col1', 'new_col2'))
#'   data <- data.frame(col1 = 1:5, col2 = 6:10)
#'   new_data <- map_columns_based_on_mapping(data, mapping)
#' }
map_column_names <- function(data, mapping, study = 'emi') {
  study_column <- ifelse(study == 'emi', 'embrace_i_column', 'embrace_ii_column')

  for (i in seq_len(nrow(mapping))) {
    map_row <- mapping[i, ]
    base_name <- map_row[[study_column]]
    new_name <- map_row$new_combined_name

    # Adjust the regex to match base_name followed directly by a suffix with no additional underscores
    if (grepl("_\\*$", base_name)) {
      # Remove the wildcard and prepare a regex that matches only suffixes without an underscore
      base_pattern <- sub("_\\*$", "", base_name)
      # The regex now ensures that after the base pattern, there is a suffix without any underscores
      cols_to_rename <- grep(paste0("^", base_pattern, "_[^_]+$"), colnames(data), value = TRUE)

      for (col_name in cols_to_rename) {
        suffix <- gsub(paste0("^", base_pattern), "", col_name)
        final_name <- paste0(new_name, suffix)
        data <- dplyr::rename(data, !!final_name := !!sym(col_name))
      }
    } else {
      # Direct rename for exact matches
      if (base_name %in% colnames(data)) {
        data <- dplyr::rename(data, !!new_name := !!sym(base_name))
      }
    }
  }

  return(data)
}