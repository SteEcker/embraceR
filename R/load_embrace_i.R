#' Load and Process Embrace I Data
#'
#' This function loads data from two different sources for the Embrace I study,
#' processes them, and joins them by the `embrace_id` column.
#'
#' @param file_path The path to the directory containing the primary data files.
#' @param add_new_columns If TRUE, calculate and add new columns
#' @param mapping_file Path to the mapping table file
#' @param filter_cohort Filter 1318
#' @return A tibble containing the cleaned and joined data.
#' @export
#' @import dplyr
#' @import here
#'
#' @examples
#' \dontrun{
#'   df <- load_embrace_i()
#' }
load_embrace_i <- function(file_path = here::here('data_raw/embrace_I'),
                           add_new_columns = T,
                           mapping_file = here::here("data_raw/mapping_table/mapping_table.xlsx"),
                           filter_cohort = T
) {

  # Construct full paths for each file using here::here
  path_t_stage <- here::here(file_path, 'Embrace_1_TS.sav')
  path_main_data <- here::here(file_path, 'emi_2021_01.Rds')
  path_morbidity <- here::here(file_path, '2022-11-24_sofia_outcome_data.sav')

  message('Loading EMBRACE-I')

  # Read and preprocess the T stage data
  t_stage <- haven::read_sav(path_t_stage)
  t_stage <- sjlabelled::remove_all_labels(t_stage) %>%
    as_tibble() %>%
    select(
      embrace_id = EMBRACE_ID_Pat,
      TS_MRI,
      TS_CLIN,
      TS_diagnosis,
      TS_brachytherapy
    ) %>%
    janitor::clean_names()

  # Read and process morbidity outcome data
  morb <- haven::read_sav(path_morbidity)
  morb <- sjlabelled::remove_all_labels(morb) %>%
    as_tibble() %>%
    select(
      embrace_id = Key_ID,

      GU_NTCP_filter,
      FistulaBleedingCystits_G2_time,
      FistulaBleedingCystits_G2_event,

      GI_NTCP_filter,
      BleedingProctitis_G2_time,
      BleedingProctitis_G2_event,

      GU_NTCP_filter,
      UrinaryIncontinence_G2_time,
      UrinaryIncontinence_G2_event,

      Vag_NTCP_filter,
      VaginaStenosis_G2_time,
      VaginaStenosis_G2_event,

      GI_NTCP_filter,
      Flatulence_G2_time,
      Flatulence_G2_event
    ) %>%
    janitor::clean_names()

  # Read and preprocess the main Embrace I data
  main_data <- readRDS(path_main_data) %>%
    janitor::clean_names()

  # Join the main data and the T stage data by 'embrace_id'
  joined_data <- main_data %>%
    left_join(t_stage, by = 'embrace_id')

  joined_data <- joined_data %>%
    left_join(morb, by = 'embrace_id')

  # Add study name
  joined_data <- joined_data %>% mutate(study = 'embrace_i')


  # Preprocessing steps
  joined_data <- joined_data %>%
    process_emi_data()

  # Add new columns if necessary
  if (add_new_columns) {
    # Read the mapping file
    mapping <- readxl::read_excel(path = mapping_file)

    # Map columns for each dataset based on the mapping file
    joined_data <- map_column_names(joined_data, mapping)
    joined_data <- joined_data %>% process_combined_data()
  }

  # Apply change log
  joined_data <- apply_change_log(joined_data)

  if (filter_cohort){
    joined_data <- joined_data %>%
      filter(!is.na(event_localfailure))
  }


  return(joined_data)
}
