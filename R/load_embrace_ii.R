#' Load and Process Embrace II Data
#'
#' This function loads data from EMBRACE-II website exports,
#' processes them, and joins them by the `embrace_id` column.
#'
#' @param file_path The path to the directory containing the primary data files.
#' @param file_path_eqd2 The path to the EMBRACE-II EQD2 Excel file.
#' @param add_new_columns If TRUE, calculate and add new columns
#' @param mapping_file Path to the mapping table file
#' @param include_only_study_patients If TRUE, only return patients included in the study
#' @return A tibble containing the cleaned and joined data.
#' @export
#' @import dplyr
#' @import readxl
#' @import janitor
#' @import tidyr
#' @import here
#'
#' @examples
#' \dontrun{
#'   df <- load_embrace_ii(file_path = "/path/to/xlsx", file_path_eqd2 = "/path/to/eqd2_xlsx")
#' }
load_embrace_ii <- function(file_path = here::here("data_raw/embrace_II/emii.xlsx"),
                          file_path_eqd2 = here::here("data_raw/embrace_II/emii_eqd2.xlsx"),
                          add_new_columns = T,
                          mapping_file = here::here("data_raw/mapping_table/mapping_table.xlsx"),
                          include_only_study_patients = TRUE,
                          replace_minusone_with_na = TRUE
                          ) {

  message('Loading EMBRACE-II')

  # Initialize list of sheet names to be imported
  sheet_names <- c("EmbraceIIPatient",
                  "EmbraceIIRegistration",
                  "EmbraceIIStatusAtDiagnosis",
                  "EmbraceIIBaselineMorbidity",
                  "EmbraceIIStatusAtBT",
                  "EmbraceIITreatmentAndDVH",
                  "EmbraceIIVitalStatus")

  sheet_name_map <- list(
    "Patient" = "pat",
    "Registration" = "reg",
    "StatusAtDiagnosis" = "sta_d",
    "BaselineMorbidity" = "BaselineMorbidity",
    "StatusAtBT" = "sta_b",
    "TreatmentAndDVH" = "tdvh",
    "VitalStatus" = "VitalStatus"
  )

  # Load data from the first sheet
  all_data <- readxl::read_excel(file_path, sheet = sheet_names[1], guess_max = 5000)

  # Loop through the remaining sheets
  for(sheet in sheet_names[-1]) {
    # Read data from the current sheet
    sheet_data <- readxl::read_excel(file_path, sheet = sheet, guess_max = 5000)

    # Remove 'EmbraceII' prefix from sheet name
    original_sheet_name <- gsub("EmbraceII", "", sheet)

    # Lookup the new shortened name
    clean_sheet_name <- sheet_name_map[[original_sheet_name]]

    # Rename columns to include the new shortened sheet name (exclude 'EMBRACE_ID')
    colnames(sheet_data)[-1] <- paste0(colnames(sheet_data)[-1], "_", clean_sheet_name)

    # Join the newly imported data with the existing data
    all_data <- dplyr::left_join(all_data, sheet_data, by = "EMBRACE_ID")
  }

  # Clean column names
  all_data <- janitor::clean_names(all_data)

  # Read and clean EQD2 data
  emii_eqd2 <- readxl::read_excel(file_path_eqd2, sheet = "EQD2", skip = 6, guess_max = 5000) %>%
    janitor::clean_names()

  # Append '_eqd2' to column names of emii_eqd2
  colnames(emii_eqd2) <- paste0(colnames(emii_eqd2), "_eqd2")

  # Join EQD2 data with the existing dataset
  all_data <- dplyr::left_join(all_data, emii_eqd2, by = c("embrace_id" = "embrace_id_eqd2"))


  # List of sheets to be transposed
  transpose_sheet_names <- c("EmbraceIIEarlyMorbidity", "Followup")

  # Loop through the sheets to be transposed
  for(sheet in transpose_sheet_names) {
    # Transpose the data to wide format
    wide_data <- transpose_sheet_to_wide(file_path, sheet, "EMBRACE_ID", "FollowupNumberID")

    # Join the transposed data with the existing data
    all_data <- dplyr::left_join(all_data, wide_data, by = "embrace_id")
  }

  # Add study name
  all_data <- all_data %>% mutate(study = 'embrace_ii')

  # Add new columns if necessary
  if (add_new_columns) {
    # Read the mapping file
    mapping <- readxl::read_excel(path = mapping_file)

    # Map columns for each dataset based on the mapping file
    all_data <- map_column_names(all_data, mapping, 'emii')
    all_data <- all_data %>% process_combined_data()
  }

  # Apply change log
  all_data <- apply_change_log(all_data)


  # Update exclusion list path with here::here
  exclusion_list <- openxlsx::read.xlsx(
    here::here("data_raw/embrace_II/2024-11-13_Flowchart_EII.xlsx")
  ) %>%
    as_tibble()

  all_data <- all_data %>%
    mutate(
      included_in_study = if_else(embrace_id %in% exclusion_list$embrace_id, F, T)
    ) %>%
    left_join(exclusion_list)

  # Filter for included patients if requested
  if (include_only_study_patients) {
    all_data <- all_data %>%
      filter(included_in_study)
  }

  if (replace_minusone_with_na) {
    all_data <- all_data %>%
      embraceR::replace_neg_one_with_NA()
  }

  return(all_data)
}



#' Transpose a long-format sheet to wide-format
#'
#' @param file_path The path to the Excel file.
#' @param sheet_name The name of the sheet to import.
#' @param id_col The column name that contains unique identifiers.
#' @param key_col The column name that will become the new columns in the wide format.
#'
#' @import dplyr
#'
#' @export
#'
#' @return A tibble in wide format.
transpose_sheet_to_wide <- function(file_path, sheet_name, id_col, key_col) {
  # Read the sheet
  long_data <- readxl::read_excel(file_path, sheet = sheet_name, guess_max = 15000)

  if (sheet_name == 'EmbraceIIEarlyMorbidity') {
    # Transpose the data to wide format
    wide_data <- long_data %>%
      tidyr::pivot_wider(id_cols = !!sym(id_col),
                         names_from = !!sym(key_col),
                         values_from = -c(!!sym(id_col), !!sym(key_col)),
                         names_glue = "{.value}_{ifelse(FollowupNumberID == '4', 'bm_4w', 'bm_eort')}"
      ) %>%
      janitor::clean_names()
  } else if (sheet_name == 'Followup') {
    # Transpose the data to wide format
    wide_data <- long_data %>%
      tidyr::pivot_wider(id_cols = !!sym(id_col),
                         names_from = !!sym(key_col),
                         values_from = -c(!!sym(id_col), !!sym(key_col)),
                         names_glue = "{.value}_{FollowupNumberID}m"
      ) %>%
      janitor::clean_names()
  }




  return(wide_data)
}


