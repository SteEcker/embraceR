#' Process and Transform EM-II Data
#'
#' Preprocessing for EMBRACE-II
#'
#' @param data A tibble or dataframe containing the combined data to be processed.
#' @param file_path Path to the EMBRACE-II master dump file
#' @return A tibble or dataframe
#' @export
#' @import dplyr
#' @import here
#'
#' @examples
#' \dontrun{
#'   processed_data <- process_emii_data(combined_data)
#' }
process_emii_data <- function(data,
                              file_path = here::here("data_raw/embrace_II/2023-01-18_emii_masterdump.Rds")) {

  # Step 1. Add T-Score from official datadump
  # Should be replaced with correct method in future
  path_t_stage <- file_path
  t_stage <- readRDS(path_t_stage)
  t_stage <- sjlabelled::remove_all_labels(t_stage) %>%
    as_tibble() %>%
    janitor::clean_names() %>%
    select(
      embrace_id = "embrace_id_pat",
      't_imaging' = 'tnmt_stage_sta_d_recalc',
      'ts_mri' = 't_score_mri_diagnosis',
      'ts_clin' = 't_score_clinical_diagnosis',
      'ts_diagnosis' = 't_score_mri_diagnosis'
    )

  data <- data %>%
    left_join(t_stage, by = "embrace_id")

  data %>%
    embraceR::emii_add_trak_absolute() %>%
    embraceR::emii_add_ott()
}
