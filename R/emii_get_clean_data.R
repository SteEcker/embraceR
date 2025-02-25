#' Get Clean EMBRACE-II Data for Analysis
#'
#' This function loads EMBRACE-II data and processes it to create a clean dataset
#' ready for final analysis. It applies all necessary transformations and filters.
#'
#' @param save_excel Logical; if TRUE, saves the data frame as an Excel file
#' @param excel_path Character; path where to save the Excel file (default: NULL)
#' @return A tibble containing the cleaned EMBRACE-II data with selected columns
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
emii_get_clean_data <- function(save_excel = FALSE,
                               excel_path = NULL) {
  # Set default excel_path with current date if not provided
  if (is.null(excel_path)) {
    current_date <- format(Sys.Date(), "%Y-%m-%d")
    excel_path <- glue::glue("{current_date}_clean_emii_data.xlsx")
  }

  # Load EMBRACE-II data using the existing load function
  data <- load_embrace_ii() %>%
    process_emii_data()

  # Select columns from the provided list
  selected_columns <- c(
    # Identifiers and administrative
    "embrace_id",
    "centre_id",
    "registration_date",
    "birth_date_sta_d",
    "year_of_birth_sta_d",
    "age",

    # Custom calculated variables
    "icis",
    "average_active_needles",
    "time_to_bt",
    "parametrial_involvement",
    "hrctv_volume_bins",
    "max_tumor_dimension",

    # T-Score variables
    "t_imaging",
    "ts_mri",
    "ts_clin",
    "ts_diagnosis",

    # Patient characteristics
    "who_score_sta_d",
    "height_sta_d",
    "weight_sta_d",
    "bmi_sta_d",
    "smoker_sta_d",
    "partner_sta_d",
    "menopause_sta_d",
    "previous_pelvic_abd_surgery_sta_d",

    # Symptoms
    "vaginal_bleeding_sta_d",
    "pain_sta_d",
    "other_symptoms_sta_d",

    # Comorbidities
    "co_morbidity_sta_d",
    "myocardial_infarction_sta_d",
    "congestive_cardiac_failure_sta_d",
    "peripheral_vascular_disease_sta_d",
    "cerebrovascular_disease_sta_d",
    "dementia_sta_d",
    "chronic_obtructive_pulm_disease_sta_d",
    "rheumatologic_disease_sta_d",
    "peptic_ulcer_disease_sta_d",
    "mild_liver_disease_sta_d",
    "diabetes_without_chronic_complications_sta_d",
    "diabetes_with_chronic_complications_sta_d",
    "hemiplegia_or_paraplegia_sta_d",
    "renal_disease_sta_d",
    "moderate_severe_liver_disease_sta_d",
    "aidshiv_sta_d",
    "charlson_comorbidity_index_sta_d",

    # Disease characteristics
    "histopathological_type_sta_d",
    "degree_of_differentiation_sta_d",
    "lvi_sta_d",
    "figo_stage_sta_d",
    "tnmt_stage_sta_d",
    "tnmn_stage_sta_d",
    "tnmm_stage_sta_d",

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
    "gyn_max_parametrium_sta_d",
    "gyn_max_tumor_dimension_sta_d",

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
    "mri_max_parametrium_sta_d",
    "mri_max_tumor_dimension_sta_d",

    # Diagnostic procedures
    "biopsy_sta_d",
    "cone_sta_d",
    "laparascopic_staging_sta_d",
    "fdgpetct_whole_body_sta_d",
    "c_talone_sta_d",

    # Pathological findings
    "pathological_nodes_sta_d",
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
    "ebrt_pibs_tdvh",
    "ebrt_pibs_minus2_tdvh",
    "ebrt_duodenum_tdvh",
    "ebrt_kidney_tdvh",
    "ebrt_liver_tdvh",
    "ebrt_pancreas_tdvh",
    "ebrt_adaptive_protocol_tdvh",
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

    #TRAK
    "trak_total_sum",
    "trak_needles_sum",
    "trak_vaginal_applicator_sum",
    "trak_tandem_applicator_sum",

    # Additional treatment parameters
    "erbt_ptv_treatment01_tdvh",
    "erbt_ptv_treatment02_tdvh",
    "erbt_ptv_treatment03_tdvh",
    "erbt_ptv_treatment04_tdvh",
    "erbt_ptv_treatment05_tdvh",
    "erbt_ptv_treatment06_tdvh",
    "erbt_ptv_treatment07_tdvh",
    "erbt_ptv_treatment08_tdvh",
    "erbt_ptv_treatment09_tdvh",
    "erbt_ptv_treatment10_tdvh",
    "ebrt_body_v36_tdvh",

    # Vital Status
    "vital_status",
    "vital_status_last_info_date",
    "vital_status_date_of_death_vital_status",
    "vital_status_cause_of_death_vital_status",
    "vital_status_cause_of_death_text_vital_status",

    # Histology and Disease Status
    "histology_assesment_date",
    "pathological_nodes_present",

    # Total DVH Parameters
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
    "total_bowel_d01cc",
    "total_icru_rectum",
    "total_icru_bladder",

    # Events
    "event_localfailure",
    "event_nodalfailure",
    "event_systemicfailure",
    "event_vitalstatus",

    # Time to Event
    "timetoevent_disease",
    "timetoevent_vitalstatus"
  )

  # Return only selected columns, with a warning for any missing columns
  existing_columns <- intersect(selected_columns, names(data))
  missing_columns <- setdiff(selected_columns, names(data))

  if (length(missing_columns) > 0) {
    warning("The following columns were not found in the dataset: ",
            paste(missing_columns, collapse = ", "))
  }

  clean_data <- data %>%
    select(all_of(existing_columns)) %>%
    recode_and_convert_all_columns()

  # Save as Excel if requested
  if (save_excel) {
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
#' @keywords internal
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
