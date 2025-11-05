#' Get Clean EMBRACE-II Data for Analysis
#'
#' This function loads EMBRACE-II data and processes it to create a clean dataset
#' ready for final analysis. It applies all necessary transformations and filters.
#'
#' @param minimal Logical; if TRUE, returns only essential columns, if FALSE returns all columns
#' @param save_excel Logical; if TRUE, saves the data frame as an Excel file
#' @param excel_path Character; path where to save the Excel file (default: NULL)
#' @return A tibble containing the cleaned EMBRACE-II data
#' @export
#'
#' @import dplyr
#' @import here
#' @importFrom openxlsx write.xlsx
#'
#' @examples
#' \dontrun{
#'   clean_data <- emii_get_clean_data()
#'   clean_data <- emii_get_clean_data(save_excel = TRUE)
#'   clean_data <- emii_get_clean_data(save_excel = TRUE, excel_path = "my_data.xlsx")
#' }
emii_get_clean_data <- function(minimal = TRUE,
                               save_excel = FALSE,
                               excel_path = NULL) {
  # Set default excel_path with current date if not provided
  if (is.null(excel_path)) {
    current_date <- format(Sys.Date(), "%Y-%m-%d")
    excel_path <- glue::glue("{current_date}_clean_emii_data.xlsx")
  }

  # Load EMBRACE-II data using the existing load function
  data <- load_embrace_ii() %>%
    process_emii_data()

  # Define selected columns for minimal dataset
  selected_columns <- c(
    # Identifiers and administrative
    "embrace_id",
    "centre_id",
    "registration_date",
    "year_of_birth_sta_d",
    "age",


    

    # T-Score variables
    "tnmt_stage_sta_d",
    "tnmn_stage_sta_d",
    "tnmm_stage_sta_d",
    "figo_stage_sta_d",



    # Patient characteristics
    "who_score_sta_d",
    "height_sta_d",
    "weight_sta_d",
    "bmi_sta_d",
    "smoker_sta_d",
    "partner_sta_d",
    "menopause_sta_d",
    "previous_pelvic_abd_surgery_sta_d",

    # Disease characteristics
    "histopathological_type_sta_d",
    "histology_assesment_date",

    # Clinical findings
    "gyn_tumor_width_sta_d",
    "gyn_tumor_height_sta_d",
    "gyn_tumor_thickness_sta_d",
    "gyn_left_parametrium_sta_d",
    "gyn_right_parametrium_sta_d",
    "gyn_vagina_sta_d",
    "gyn_vagina_anterior_sta_d",
    "gyn_vagina_posterior_sta_d",
    "gyn_vagina_left_lateral_sta_d",
    "gyn_vagina_right_lateral_sta_d",
    "gyn_vagina_max_distal_extension_fornix_sta_d",
    "gyn_bladder_cystoscopy_sta_d",
    "gyn_rectum_exploration_sta_d",
    "gyn_rectum_endoscopy_sta_d",


    # MRI findings
    "mri_tumor_width_sta_d",
    "mri_tumor_height_sta_d",
    "mri_tumor_thickness_sta_d",
    "mri_cervix1_sta_d",
    "mri_cervix2_sta_d",
    "mri_necrosis_diameter_sta_d",
    "mri_left_parametrium_sta_d",
    "mri_right_parametrium_sta_d",
    "mri_tumor_infiltration_type_sta_d",
    "mri_corpus_uteri_sta_d",
    "mri_vagina_sta_d",
    "mri_bladder_sta_d",
    "mri_rectum_sta_d",


    # Diagnostic procedures
    "biopsy_sta_d",
    "cone_sta_d",
    "laparascopic_staging_sta_d",
    "fdgpetct_whole_body_sta_d",
    "c_talone_sta_d",

    # Pathological findings
    "pathological_nodes_sta_d",
    "pathological_nodes_present",
    "hydronephrosis_sta_d",

    # Blood tests
    "blood_test_hgb_unit_sta_d",
    "blood_test_hgb_mmol_l_sta_d",
    "blood_test_hgb_g_d_l_sta_d",
    "blood_test_wbc_sta_d",
    "blood_test_lymphocytes_sta_d",
    "blood_test_creatinine_unit_sta_d",
    "blood_test_creatinine_umol_l_sta_d",
    "blood_test_creatinine_mg_d_l_sta_d",
    "blood_test_creatinine_clearance_decimal_sta_d",

    # Treatment and DVH parameters (_tdvh)
    "overall_treatment_as_planned_tdvh",
    "overall_treatment_not_as_planned_which_tdvh",
    "overall_treatment_not_as_planned_tdvh",
    "overall_nadir_hgb_tdvh",
    "overall_nadir_hgb_unit_tdvh",
    "overall_transfusion_tdvh",
    "ebrt_start_date_tdvh",
    "ebrt_end_date_tdvh",
    "ebrt_gtv_t_volume_tdvh",
    "ebrt_ctv_hr_volume_tdvh",
    "ebrt_ctv_lr_volume_tdvh",
    "ebrt_itv_volume_tdvh",
    "ebrt_itv45_d98_tdvh",
    "ebrt_itv45_d50_tdvh",
    "ebrt_ptv45_volume_tdvh",
    "ebrt_bowel_v15gy_absolute_volume_tdvh",
    "ebrt_bowel_v30gy_absolute_volume_tdvh",
    "ebrt_bowel_v40gy_absolute_volume_tdvh",
    "ebrt_bladder_v30gy_relative_volume_tdvh",
    "ebrt_bladder_v40gy_relative_volume_tdvh",
    "ebrt_rectum_v30gy_relative_volume_tdvh",
    "ebrt_rectum_v40gy_relative_volume_tdvh",
    "ebrt_sigmoid_v30gy_relative_volume_tdvh",
    "ebrt_sigmoid_v40gy_relative_volume_tdvh",
    "ebrt_body_v43gy_absolute_volume_tdvh",
    "ebrt_body_v50gy_absolute_volume_tdvh",
    "ebrt_body_v36_tdvh",
    "ebrt_pibs_tdvh",
    "ebrt_pibs_minus2_tdvh",
    "ebrt_duodenum_tdvh",
    "ebrt_kidney_tdvh",
    "ebrt_liver_tdvh",
    "ebrt_pancreas_tdvh",
    "ebrt_adaptive_protocol_tdvh",
    "ebrt_elective_target_selected",


    # Chemotherapy
    "conc_chemo_given_tdvh",
    "conc_chemo_not_given_tdvh",
    "conc_chemo_schedule_tdvh",
    "conc_chemo_courses100pct_tdvh",
    "conc_chemo_courses_lt100pct_tdvh",
    "conc_chemo_courses0pct_tdvh",
    "conc_chemo_reduction_tdvh",
    "adjuvant_chemo_given_tdvh",
    "adjuvant_chemo_courses100pct_tdvh",
    "adjuvant_chemo_courses_lt100pct_tdvh",
    "adjuvant_chemo_courses0pct_tdvh",
    "bt_fractions_tdvh",

    # All fraction-specific variables (fraction01 through fraction07)
    paste0("fraction", sprintf("%02d", 1:7), "date_tdvh"),
    paste0("fraction", sprintf("%02d", 1:7), "applicator_tdvh"),
    paste0("fraction", sprintf("%02d", 1:7), "technique_tdvh"),
    paste0("fraction", sprintf("%02d", 1:7), "technique_icis_active_needles_tdvh"),
    paste0("fraction", sprintf("%02d", 1:7), "point_adxt_tdvh"),
    paste0("fraction", sprintf("%02d", 1:7), "point_asin_tdvh"),
    paste0("fraction", sprintf("%02d", 1:7), "dose_rate_tdvh"),
    paste0("fraction", sprintf("%02d", 1:7), "pdr_pulses_tdvh"),
    paste0("fraction", sprintf("%02d", 1:7), "pdr_pulses_interval_tdvh"),
    paste0("fraction", sprintf("%02d", 1:7), "pdr_pulse_irradiation_time_tdvh"),
    paste0("fraction", sprintf("%02d", 1:7), "trak_tdvh"),
    paste0("fraction", sprintf("%02d", 1:7), "trak_tandem_applicator_pct_tdvh"),
    paste0("fraction", sprintf("%02d", 1:7), "trak_vaginal_applicator_pct_tdvh"),
    paste0("fraction", sprintf("%02d", 1:7), "trak_needles_pct_tdvh"),
    paste0("fraction", sprintf("%02d", 1:7), "_tandem_applicator_abs"),
    paste0("fraction", sprintf("%02d", 1:7), "_vaginal_applicator_abs"),
    paste0("fraction", sprintf("%02d", 1:7), "_needles_abs"),
    paste0("fraction", sprintf("%02d", 1:7), "residual_gtv_present_tdvh"),
    paste0("fraction", sprintf("%02d", 1:7), "residual_gtv_volume_tdvh"),
    paste0("fraction", sprintf("%02d", 1:7), "gtvd98_tdvh"),
    paste0("fraction", sprintf("%02d", 1:7), "hrctv_volume_tdvh"),
    paste0("fraction", sprintf("%02d", 1:7), "hrctv_d98_tdvh"),
    paste0("fraction", sprintf("%02d", 1:7), "hrctv_d90_tdvh"),
    paste0("fraction", sprintf("%02d", 1:7), "hrctv_d50_tdvh"),
    paste0("fraction", sprintf("%02d", 1:7), "irctv_volume_tdvh"),
    paste0("fraction", sprintf("%02d", 1:7), "irctv_d98_tdvh"),
    paste0("fraction", sprintf("%02d", 1:7), "icru_bladder_tdvh"),
    paste0("fraction", sprintf("%02d", 1:7), "bladder01cc_tdvh"),
    paste0("fraction", sprintf("%02d", 1:7), "bladder2cc_tdvh"),
    paste0("fraction", sprintf("%02d", 1:7), "icru_rectum_tdvh"),
    paste0("fraction", sprintf("%02d", 1:7), "rectum01cc_tdvh"),
    paste0("fraction", sprintf("%02d", 1:7), "rectum2cc_tdvh"),
    paste0("fraction", sprintf("%02d", 1:7), "sigmoid01cc_tdvh"),
    paste0("fraction", sprintf("%02d", 1:7), "sigmoid2cc_tdvh"),
    paste0("fraction", sprintf("%02d", 1:7), "bowel_other2cc_tdvh"),
    paste0("fraction", sprintf("%02d", 1:7), "vagina_sin5mm_tdvh"),
    paste0("fraction", sprintf("%02d", 1:7), "vagina_dxt5mm_tdvh"),
    paste0("fraction", sprintf("%02d", 1:7), "pibs_plus2_tdvh"),
    paste0("fraction", sprintf("%02d", 1:7), "pibs_tdvh"),
    paste0("fraction", sprintf("%02d", 1:7), "pibs_minus2_tdvh"),
    paste0("fraction", sprintf("%02d", 1:7), "vaginal_reference_length_tdvh"),



    # Status at brachytherapy implants
    "bt_implants_sta_b",


    # Vital Status
    "vital_status",
    "vital_status_last_info_date",
    "vital_status_date_of_death_vital_status",
    "vital_status_cause_of_death_vital_status",
    



    # Total DVH Parameters in EQD2
    "total_hrctv_d90",
    "total_hrctv_d98",
    "total_irctv_d98",
    "total_gtv_d98",
    "total_point_a_dxt",
    "total_point_a_sin",
    "total_bladder_d2cc",
    "total_bladder_d01cc",
    "total_rectum_d2cc",
    "total_rectum_d01cc",
    "total_sigmoid_d2cc",
    "total_sigmoid_d01cc",
    "total_bowel_d2cc",
    "total_icru_rectum",
    "total_icru_bladder",
    "hr_ctv_d50_eqd2",
    "vagina_lateral_5mm_left_eqd2",
    "vagina_lateral_5mm_right_eqd2",
    "pibs_2cm_eqd2",
    "pibs_eqd2",
    "pibs_2cm_2_eqd2",

    # Custom calculated variables
    "included_in_study",
    "withdrew_consent",
    "is_lost_to_fu",
    ## Diagnostic
    "max_tumor_dimension_sta_d",
    "gyn_max_parametrium_sta_d",
    "gyn_max_tumor_dimension_sta_d",
    "mri_max_parametrium_sta_d",
    "mri_max_tumor_dimension_sta_d",
    ## BT Treatment and Technique
    "icis",
    "icis_parallel_oblique",
    "average_nr_active_needles",
    "time_to_bt",
    "time_to_bt_percent",
    "ott_ebrt",
    "ott",
    "last_treatment_date",
    "fraction01hrctv_volume_bins",
    "trak_total_sum",
    "trak_needles_sum",
    "trak_vaginal_applicator_sum",
    "trak_tandem_applicator_sum",
    "imp1gyn_max_parametrium_sta_b",
    "imp1image_max_parametrium_sta_b",
    ## EBRT
    "ebrt_elective_target_algorithm",
    "elective_high_risk",
    "elective_inguinal",
    "elective_low_risk",
    ## Lymph Nodes & Metastases
    "nodal_classification",
    "number_common_iliac_ln_stat_d",
    "number_paraaortic_ln_stat_d",
    "has_L.ext.iliac_followup",
    "has_L.int.iliac_followup",
    "has_L.com.iliac_followup",
    "has_R.ext.iliac_followup",
    "has_R.int.iliac_followup",
    "has_R.com.iliac_followup",
    "has_Para.Aortic_followup",
    "has_L.groin_followup",
    "has_R.groin_followup",
    "has_R.parame.paracervix_followup",
    "has_L.parame.paracervix_followup",
    "has_other_followup",
    "has_L.ext.iliac_diagnosis",
    "has_L.int.iliac_diagnosis",
    "has_L.com.iliac_diagnosis",
    "has_R.ext.iliac_diagnosis",
    "has_R.int.iliac_diagnosis",
    "has_R.com.iliac_diagnosis",
    "has_Para.Aortic_diagnosis",
    "has_L.groin_diagnosis",
    "has_R.groin_diagnosis",
    "has_R.parame.paracervix_diagnosis",
    "has_L.parame.paracervix_diagnosis",
    "has_other_diagnosis",
    "any_node_ci_pa",
    "has_paraaortic_nodes_above_l2",
    "has_supraclavicular_nodes",
    "has_mediastinal_nodes",
    "has_liver_metastases",
    "has_bone_metastases",
    "has_brain_metastases",
    "has_lung_metastases",
    "has_abdominal_carcinomatosis",
    "has_other_metastases",
    ## Event Variables
    "timetoevent_disease",
    "timetoevent_vitalstatus",
    "latest_vital_status_date",
    "event_localfailure",
    "event_nodalfailure",
    "event_nodalcontrol_incl_pao",
    "event_systemicfailure",
    "event_systemic_excl_pao",
    "event_vitalstatus",
    "event_pelvic_nodal",
    "event_pelvic",
    "event_paraaortic_nodal",
    "event_locoregional",
    "event_locoregional_alone",
    "event_cancer_specific",
    "event_disease_control",
    "event_progression_free",
    "event_distant_alone",
    "latest_assessment_date_disease",
    "latest_followup_id",
    
    # Toxicity and morbidity variables - values
    "max_value_bladder_cystitis",
    "max_value_bladder_frequency",
    "max_value_bladder_incontinence",
    "max_value_bladder_other",
    "max_value_bladder_urgency",
    "max_value_fistula",
    "max_value_gastro_abdominal_pain_or_cramping",
    "max_value_gastro_constipation",
    "max_value_gastro_diarrhea",
    "max_value_gastro_incontinence",
    "max_value_gastro_intestinal_other",
    "max_value_gastro_proctitis",
    "max_value_lymphatics_edema_limb",
    "max_value_lymphatics_edema_trunk_genital",
    "max_value_lymphatics_lymphocele",
    "max_value_lymphatics_thromboembolic_event",
    "max_value_muscle_fibrosis_left",
    "max_value_muscle_fibrosis_other",
    "max_value_muscle_fibrosis_right",
    "max_value_muscle_fracture_back_pain",
    "max_value_muscle_fracture_femoral_head",
    "max_value_muscle_fracture_pelvic_pain",
    "max_value_muscle_fracture_pelvic_ring",
    "max_value_other1",
    "max_value_other_fatigue",
    "max_value_other_hot_flashes",
    "max_value_other_insomnia",
    "max_value_vagina_bleeding",
    "max_value_vagina_discharge",
    "max_value_vagina_dryness",
    "max_value_vagina_mucositis",
    "max_value_vagina_other",
    "max_value_vagina_stenosis",
    "max_value_gastro_bleeding_anus",
    "max_value_gastro_bleeding_colon",
    "max_value_gastro_bleeding_other",
    "max_value_gastro_bleeding_rectum",
    "max_value_gastro_bleeding_sigmoid",
    "max_value_gastro_bleeding_small_bowel",
    "max_value_gastro_bleeding_unknown",
    "max_value_fistula_grading",
    "max_value_fistula_localization_anus",
    "max_value_fistula_localization_bladder",
    "max_value_fistula_localization_colon",
    "max_value_fistula_localization_other",
    "max_value_fistula_localization_rectum",
    "max_value_fistula_localization_sigmoid",
    "max_value_fistula_localization_small_bowel",
    "max_value_fistula_localization_ureter",
    "max_value_fistula_localization_urethra",
    "max_value_fistula_localization_vagina",
    "max_value_other2",
    "max_value_bladder_stenosis_stricture",
    "max_value_bladder_stenosis_stricture_ureter",
    "max_value_bladder_stenosis_stricture_urethra",
    "max_value_bladder_bleeding",
    "max_value_bladder_bleeding_ureter",
    "max_value_bladder_bleeding_urethra",
    "max_value_gastro_stenosis_stricture_anus",
    "max_value_gastro_stenosis_stricture_colon",
    "max_value_gastro_stenosis_stricture_other",
    "max_value_gastro_stenosis_stricture_rectum",
    "max_value_gastro_stenosis_stricture_sigmoid",
    "max_value_gastro_stenosis_stricture_small_bowel",
    "max_value_gastro_stenosis_stricture_unknown",
    "max_value_other3",
    "max_value_other4",
    "max_value_other5",
    
    # Toxicity and morbidity variables - timepoints
    "max_timepoint_bladder_cystitis",
    "max_timepoint_bladder_frequency",
    "max_timepoint_bladder_incontinence",
    "max_timepoint_bladder_other",
    "max_timepoint_bladder_urgency",
    "max_timepoint_fistula",
    "max_timepoint_gastro_abdominal_pain_or_cramping",
    "max_timepoint_gastro_constipation",
    "max_timepoint_gastro_diarrhea",
    "max_timepoint_gastro_incontinence",
    "max_timepoint_gastro_intestinal_other",
    "max_timepoint_gastro_proctitis",
    "max_timepoint_lymphatics_edema_limb",
    "max_timepoint_lymphatics_edema_trunk_genital",
    "max_timepoint_lymphatics_lymphocele",
    "max_timepoint_lymphatics_thromboembolic_event",
    "max_timepoint_muscle_fibrosis_left",
    "max_timepoint_muscle_fibrosis_other",
    "max_timepoint_muscle_fibrosis_right",
    "max_timepoint_muscle_fracture_back_pain",
    "max_timepoint_muscle_fracture_femoral_head",
    "max_timepoint_muscle_fracture_pelvic_pain",
    "max_timepoint_muscle_fracture_pelvic_ring",
    "max_timepoint_other1",
    "max_timepoint_other_fatigue",
    "max_timepoint_other_hot_flashes",
    "max_timepoint_other_insomnia",
    "max_timepoint_vagina_bleeding",
    "max_timepoint_vagina_discharge",
    "max_timepoint_vagina_dryness",
    "max_timepoint_vagina_mucositis",
    "max_timepoint_vagina_other",
    "max_timepoint_vagina_stenosis",
    "max_timepoint_gastro_bleeding_anus",
    "max_timepoint_gastro_bleeding_colon",
    "max_timepoint_gastro_bleeding_other",
    "max_timepoint_gastro_bleeding_rectum",
    "max_timepoint_gastro_bleeding_sigmoid",
    "max_timepoint_gastro_bleeding_small_bowel",
    "max_timepoint_gastro_bleeding_unknown",
    "max_timepoint_fistula_grading",
    "max_timepoint_fistula_localization_anus",
    "max_timepoint_fistula_localization_bladder",
    "max_timepoint_fistula_localization_colon",
    "max_timepoint_fistula_localization_other",
    "max_timepoint_fistula_localization_rectum",
    "max_timepoint_fistula_localization_sigmoid",
    "max_timepoint_fistula_localization_small_bowel",
    "max_timepoint_fistula_localization_ureter",
    "max_timepoint_fistula_localization_urethra",
    "max_timepoint_fistula_localization_vagina",
    "max_timepoint_other2",
    "max_timepoint_bladder_stenosis_stricture",
    "max_timepoint_bladder_stenosis_stricture_ureter",
    "max_timepoint_bladder_stenosis_stricture_urethra",
    "max_timepoint_bladder_bleeding",
    "max_timepoint_bladder_bleeding_ureter",
    "max_timepoint_bladder_bleeding_urethra",
    "max_timepoint_gastro_stenosis_stricture_anus",
    "max_timepoint_gastro_stenosis_stricture_colon",
    "max_timepoint_gastro_stenosis_stricture_other",
    "max_timepoint_gastro_stenosis_stricture_rectum",
    "max_timepoint_gastro_stenosis_stricture_sigmoid",
    "max_timepoint_gastro_stenosis_stricture_small_bowel",
    "max_timepoint_gastro_stenosis_stricture_unknown",
    "max_timepoint_other3",
    "max_timepoint_other4",
    "max_timepoint_other5",
    
    # Overall maximum morbidity grade
    "overall_max_morbidity_grade"
  )

  # Select columns based on minimal flag
  if (minimal) {
    # Return only selected columns
    existing_columns <- intersect(selected_columns, names(data))
    missing_columns <- setdiff(selected_columns, names(data))
  } else {
    # Return all columns from the dataset
    existing_columns <- names(data)
    missing_columns <- character(0)
  }

  # Warning for missing columns
  if (length(missing_columns) > 0) {
    warning("The following columns were not found in the dataset: ",
            paste(missing_columns, collapse = ", "))
  }

  clean_data <- data %>%
    select(all_of(existing_columns)) %>%
    recode_and_convert_all_columns()

  # Replace NA with 0 for event variables where all three are NA
  event_vars <- c("event_localfailure", "event_nodalfailure", "event_systemicfailure")
  if (all(event_vars %in% names(clean_data))) {
    clean_data <- clean_data %>%
      mutate(
        # Identify patients where all three event variables are NA
        all_events_na = is.na(event_localfailure) & 
                        is.na(event_nodalfailure) & 
                        is.na(event_systemicfailure),
        # Replace NA with 0 for those patients only
        event_localfailure = if_else(all_events_na, 0, event_localfailure),
        event_nodalfailure = if_else(all_events_na, 0, event_nodalfailure),
        event_systemicfailure = if_else(all_events_na, 0, event_systemicfailure)
      ) %>%
      select(-all_events_na)  # Remove temporary helper column
  }

  # Save as Excel if requested
  if (save_excel) {
    file_suffix <- if(minimal) "preset" else "full"
    if (is.null(excel_path)) {
      current_date <- format(Sys.Date(), "%Y-%m-%d")
      excel_path <- glue::glue("{current_date}_clean_emii_data_{file_suffix}.xlsx")
    }
    message(glue::glue("Saving data to {excel_path}"))
    openxlsx::write.xlsx(clean_data, excel_path)
  }

  clean_data
}


#' Create Summary Statistics Table for EMBRACE-II Data
#'
#' This function creates a summary statistics table for EMBRACE-II data with
#' customized formatting. It combines mean, SD, median, IQR, and range statistics
#' into a single row per variable and includes section information based on variable suffixes.
#'
#' @param data A tibble containing EMBRACE-II data
#' @param save_excel Logical; if TRUE, saves the data frame as an Excel file
#' @param excel_path Character; path where to save the Excel file (default: NULL)
#' @return A tibble containing summarized statistics with one row per variable, including section information
#' @export
#'
#' @import dplyr
#' @import gtsummary
#' @importFrom openxlsx write.xlsx
#'
#' @examples
#' \dontrun{
#'   clean_data <- emii_get_clean_data()
#'   summary_stats <- emii_get_summary_statistics(clean_data)
#'   summary_stats <- emii_get_summary_statistics(clean_data, save_excel = TRUE)
#' }
emii_get_summary_statistics <- function(data,
                                      save_excel = FALSE,
                                      excel_path = NULL) {
  # Set default excel_path with current date if not provided
  if (is.null(excel_path)) {
    current_date <- format(Sys.Date(), "%Y-%m-%d")
    excel_path <- glue::glue("{current_date}_emii_summary_statistics.xlsx")
  }

  # Define suffix mapping
  suffix_mapping <- c(
    "sta_d" = "Status at Diagnosis",
    "tdvh" = "Treatment and DVH",
    "sta_b" = "Status at Brachytherapy",
    "pat" = "Patient",
    "reg" = "Registration",
    "vital_status" = "Vital Status"
  )

  # Create initial summary table
  summary_tbl <- data %>%
    gtsummary::tbl_summary(
      type = all_continuous() ~ "continuous2",
      statistic = all_continuous() ~ c(
        "{mean} ({sd})",
        "{median} ({p25}, {p75})",
        "{min}, {max}"
      )
    ) %>%
    gtsummary::as_gt()

  # Transform to single row format and add section information
  result <- summary_tbl$`_data` %>%
    group_by(variable, var_type, var_label) %>%
    reframe(
      n_missing = {
        missing_val <- stat_0[row_type == "missing"]
        if(length(missing_val) > 0) missing_val else "0"
      },
      statistics = case_when(
        var_type == "continuous2" ~ paste(
          "Mean (SD):", first(stat_0[row_type == "level" & str_detect(label, "Mean")]),
          "; Median (IQR):", first(stat_0[row_type == "level" & str_detect(label, "Median")]),
          "; Range:", first(stat_0[row_type == "level" & str_detect(label, "Min")])
        ),
        var_type == "categorical" ~ paste(
          str_c(
            label[row_type == "level"],
            ": ",
            stat_0[row_type == "level"],
            collapse = "\n"
          )
        ),
        var_type == "dichotomous" ~ paste(
          "Yes:", first(stat_0[row_type == "label"])
        ),
        TRUE ~ NA_character_
      )
    ) %>%
    mutate(
      statistics = str_remove_all(statistics, "level: "),
      # Extract suffix and map to section
      Suffix = str_extract(variable, paste0("(", paste(names(suffix_mapping), collapse = "|"), ")$")),
      Section = case_when(
        !is.na(Suffix) ~ suffix_mapping[Suffix],
        TRUE ~ ""
      )
    ) %>%
    select(-Suffix) %>%  # Remove temporary suffix column
    distinct() %>%
    arrange(Section, variable)  # Sort by section and variable name

  # Save as Excel if requested
  if (save_excel) {
    message(glue::glue("Saving summary statistics to {excel_path}"))
    openxlsx::write.xlsx(result, excel_path)
  }

  result
}

#' Identify Extreme Outliers in EMBRACE-II Data
#'
#' This function analyzes all numeric columns in the dataset and identifies extreme outliers
#' using the interquartile range (IQR) method. An extreme outlier is defined as a value that
#' is more than 3 times the IQR below Q1 or above Q3.
#'
#' @param data A tibble containing EMBRACE-II data
#' @param iqr_multiplier Numeric; multiplier for IQR to define outlier threshold (default: 3)
#' @param save_excel Logical; if TRUE, saves the outliers as an Excel file
#' @param excel_path Character; path where to save the Excel file (default: NULL)
#' @return A tibble containing outlier information (embrace_id, variable, value, threshold, median)
#' @export
#'
#' @import dplyr
#' @import tidyr
#' @importFrom stats quantile IQR median
#' @importFrom openxlsx write.xlsx
#'
#' @examples
#' \dontrun{
#'   clean_data <- emii_get_clean_data()
#'   outliers <- emii_find_outliers(clean_data)
#'   outliers <- emii_find_outliers(clean_data, save_excel = TRUE)
#' }
emii_find_outliers <- function(data,
                              iqr_multiplier = 3,
                              save_excel = FALSE,
                              excel_path = NULL) {
  # Set default excel_path with current date if not provided
  if (is.null(excel_path)) {
    current_date <- format(Sys.Date(), "%Y-%m-%d")
    excel_path <- glue::glue("{current_date}_emii_outliers.xlsx")
  }

  # Ensure embrace_id is present
  if (!"embrace_id" %in% names(data)) {
    stop("Data must contain 'embrace_id' column")
  }

  # Get numeric columns
  numeric_cols <- names(data)[sapply(data, is.numeric)]

  # Initialize empty list to store outliers
  outliers_list <- list()

  # Process each numeric column
  for (col in numeric_cols) {
    # Get column values
    values <- data[[col]]

    # Skip if all values are NA
    if (all(is.na(values))) next

    # Calculate quartiles, IQR, and median
    q1 <- quantile(values, 0.25, na.rm = TRUE)
    q3 <- quantile(values, 0.75, na.rm = TRUE)
    iqr <- IQR(values, na.rm = TRUE)
    med <- median(values, na.rm = TRUE)

    # Define outlier thresholds
    lower_bound <- q1 - iqr_multiplier * iqr
    upper_bound <- q3 + iqr_multiplier * iqr

    # Find outliers
    outlier_indices <- which(values < lower_bound | values > upper_bound)

    # If outliers found, add to list
    if (length(outlier_indices) > 0) {
      outliers_list[[col]] <- tibble(
        embrace_id = data$embrace_id[outlier_indices],
        variable = col,
        value = values[outlier_indices],
        threshold = ifelse(
          values[outlier_indices] < lower_bound,
          paste("< ", round(lower_bound, 3), " (low)"),
          paste("> ", round(upper_bound, 3), " (high)")
        ),
        median = round(med, 3)
      )
    }
  }

  # Combine all outliers into one tibble
  result <- bind_rows(outliers_list) %>%
    arrange(variable, embrace_id)

  # Save as Excel if requested
  if (save_excel && nrow(result) > 0) {
    message(glue::glue("Saving outliers to {excel_path}"))
    openxlsx::write.xlsx(result, excel_path)
  }

  result
}
