#' Load and Combine Embrace I and Embrace II Data
#'
#' This function loads data from EMBRACE-I and EMBRACE-II, processes them,
#' and combines them into a single dataframe.
#'
#' @param file_path_i The path to the EMBRACE-I data root folder.
#' @param file_path_ii The path to the EMBRACE-II Excel file.
#' @param file_path_eqd2 The path to the EMBRACE-II EQD2 Excel file.
#' @param return_common_columns If TRUE, return only matched columns
#' @import dplyr
#' @import here
#'
#' @export
#'
#' @return A tibble containing the cleaned and combined data from both studies.
load_combined_embrace <- function(file_path_i = here::here('data_raw/embrace_I'),
                                  file_path_ii = here::here("data_raw/embrace_II/emii.xlsx"),
                                  file_path_eqd2 = here::here("data_raw/embrace_II/emii_eqd2.xlsx"),
                                  mapping_file = here::here("data_raw/mapping_table/mapping_table.xlsx"),
                                  return_common_columns = T
) {

  # Load the Embrace I data
  embrace_i_data <- load_embrace_i(file_path_i)

  # Load the Embrace II data
  embrace_ii_data <- load_embrace_ii(file_path = file_path_ii,
                                     file_path_eqd2 = file_path_eqd2)

  message('Combine Embrace I and Embrace II Data')

  # Read the mapping file using here::here
  mapping <- readxl::read_excel(path = mapping_file)

  # Map columns for each dataset based on the mapping file
  embrace_i_data <- map_column_names(embrace_i_data, mapping)
  embrace_ii_data <- map_column_names(embrace_ii_data, mapping, 'emii')

  # Column mapping
  common_columns <- intersect(colnames(embrace_i_data), colnames(embrace_ii_data))

  combined_data <- dplyr::full_join(embrace_i_data, embrace_ii_data, by = common_columns)

  if (return_common_columns) {
    combined_data <- combined_data %>% select(all_of(common_columns))
  }

  return(combined_data)
}
