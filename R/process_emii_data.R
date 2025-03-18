#' Process and Transform EMBRACE-II Data
#'
#' Performs comprehensive preprocessing for EMBRACE-II data. This function adds
#' EMBRACE-II specific columns after the common columns have been added via
#' process_combined_data. The processing includes:
#'
#' 1. Adding T-Score data
#' 2. Calculating TRAK (Total Reference Air Kerma) absolute values
#' 3. Classifying needle arrangements (parallel/oblique)
#' 4. Computing overall treatment time (OTT)
#' 5. Adding elective target information
#' 6. Identifying patients lost to follow-up or who withdrew consent
#' 7. Adding disease event indicators (local/nodal/systemic failures)
#' 8. Counting common iliac lymph nodes
#'
#' @param data A tibble or dataframe containing the combined data to be processed
#' @param file_path Path to the EMBRACE-II master dump file (default uses here::here)
#'
#' @return A tibble with additional EMBRACE-II specific columns
#' @export
#' @import dplyr
#' @import here
#'
#' @examples
#' \dontrun{
#'   # Load raw data
#'   raw_data <- load_combined_embrace()
#'
#'   # Process with common transformations
#'   common_data <- process_combined_data(raw_data)
#'
#'   # Add EMBRACE-II specific transformations
#'   processed_data <- process_emii_data(common_data)
#' }
process_emii_data <- function(data,
                              file_path = here::here("data_raw/embrace_II/2023-01-18_emii_masterdump.Rds")) {

  # TODO: Verify T-Score source is correct
  # # Step 1. Add T-Score from official datadump
  # path_t_stage <- file_path
  # t_stage <- readRDS(path_t_stage)
  # t_stage <- sjlabelled::remove_all_labels(t_stage) %>%
  #   as_tibble() %>%
  #   janitor::clean_names() %>%
  #   select(
  #     embrace_id = "embrace_id_pat",
  #     't_score_mri_diagnosis' = 't_score_mri_diagnosis',
  #     't_score_clinical_diagnosis' = 't_score_clinical_diagnosis',
  #     't_score_mri_diagnosis' = 't_score_mri_diagnosis'
  #   )

  # data <- data %>%
  #   left_join(t_stage, by = "embrace_id")

  data %>%
    embraceR::emii_add_trak_absolute() %>%
    embraceR::add_parallel_oblique_needles() %>%
    embraceR::emii_add_ott() %>%
    embraceR::emii_add_elective_targets() %>%
    embraceR::add_lost_to_fu() %>%
    embraceR::add_disease_events() %>%
    embraceR::emii_add_number_common_iliac_ln_stat_d() %>%
    embraceR::add_number_paraaortic_ln_stat_d() %>%
    embraceR::emii_add_outcome() %>%
    embraceR::emii_add_max_morbidity()
}
