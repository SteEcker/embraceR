#' Apply Threshold to CTCAE Grade
#'
#' Determines if a CTCAE grade value meets or exceeds a specified threshold.
#'
#' @param x Numeric, the CTCAE grade value to check
#' @param grade_threshold Numeric, the threshold value to compare against
#' @return Logical, TRUE if the value equals the threshold, otherwise FALSE
#' @examples
#' threshold(3, grade_threshold = 3)  # TRUE
#' threshold(2, grade_threshold = 3)  # FALSE
#' threshold(NA, grade_threshold = 3) # FALSE
#' @export
threshold <- function(x, grade_threshold) {
  if (!is.na(x) && !is.character(x) && x != 9) {
    return(x == grade_threshold)
  } else {
    return(FALSE)
  }
}



#' Filter Data for Specific CTCAE Grade Events
#'
#' Filters a dataframe to identify rows with side effect values meeting a specified 
#' grade threshold, retaining relevant clinical information for analysis.
#'
#' @param df DataFrame containing the patient data
#' @param side_effect_cols Character vector specifying the side effect columns to check
#' @param grade_threshold Numeric, the CTCAE grade threshold to filter by (default: 4)
#' @return DataFrame containing only rows with events meeting the threshold criteria
#' @export
#' @import tidyr dplyr stringr lubridate
filter_grade_threshold <- function(df, side_effect_cols, grade_threshold = 4) {

  message(glue::glue("Filtering for CTCAE grade greater than: {grade_threshold}"))

  # Clean side effect names
  threshold_cols <- embraceR::clean_side_effect_names(df, "_3m")
  print("===========================")
  print("Looking for events in:")
  print("===========================")
  print(threshold_cols)

  # Used for threshold
  outcome_columns <- names(df) %>%
    purrr::keep(~any(str_detect(., paste0("^", threshold_cols, ".*")))) %>%
    purrr::discard(~str_detect(., "baseline_morbidity$|bm_4w$|bm_eort$")) %>% # Followup
    # purrr::discard(~str_detect(., "bm_4w$|bm_eort$|m$")) %>% # Baseline
    # purrr::discard(~str_detect(., "baseline_morbidity$|m$")) %>% # Acute
    purrr::discard(~str_detect(., "text"))  # Exclude columns containing "text"

  print("===========================")
  print("===========================")
  print(outcome_columns)

  # Listed in excel sheet
  selection_columns <- names(df) %>%
    purrr::keep(~any(str_detect(., paste0("^", side_effect_cols, ".*")))) %>% #  Keep only the columns that start with any of the patterns in 'side_effect_cols'
    purrr::discard(~str_detect(., "baseline_morbidity$|bm_4w$|bm_eort$")) # Discard columns that end with 'baseline_morbidity', 'bm_4w', or 'bm_eort'

  # baseline info (longitudinal data)
  baseline_columns <- names(df) %>%
    purrr::keep(~str_detect(., "baseline_morbidity$|bm_4w$|bm_eort$")) %>%
    purrr::keep(~any(str_detect(., paste0("^", side_effect_cols, ".*"))))


  # baseline info (longitudinal data (no longitudinal data))
  baseline_columns <- c(baseline_columns,
                        "age",
                        "smoker_sta_d",
                        "previous_pelvic_abd_surgery_sta_d",

                        "figo_stage_sta_d",
                        "tnmt_stage_sta_d",

                        "who_score_sta_d",
                        "bmi_sta_d",
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
                        "vaginal_bleeding_sta_d",
                        "pain_sta_d",
                        "other_symptoms_text_sta_d",

                        "gyn_tumor_width_sta_d",
                        "gyn_tumor_height_sta_d",
                        "gyn_tumor_thickness_sta_d",
                        "gyn_right_parametrium_sta_d",
                        "gyn_left_parametrium_sta_d",
                        "gyn_vagina_sta_d",
                        "gyn_bladder_cystoscopy_sta_d",
                        "gyn_rectum_endoscopy_sta_d",
                        "gyn_rectum_exploration_sta_d",

                        "mri_tumor_infiltration_type_sta_d",
                        "mri_corpus_uteri_sta_d",
                        "mri_vagina_sta_d",
                        "mri_bladder_sta_d",
                        "mri_rectum_sta_d",
                        "mri_left_parametrium_sta_d",
                        "mri_right_parametrium_sta_d",

                        "pathological_nodes_present",
                        "pathological_nodes_sta_d",
                        "number_common_iliac_ln_stat_d",
                        # "number_paraaortic_ln_stat_d",

                        "hydronephrosis_sta_d",
                        "laparascopic_staging_sta_d",
                        "lvi_sta_d",
                        "vital_status",
                        "vital_status_cause_of_death_vital_status",
                        "vital_status_cause_of_death_text_vital_status",
                        "vital_status_date_of_death_vital_status"
                        )

  baseline_df <- df %>% select(embrace_id, all_of(baseline_columns))

  # Filter the dataframe for any event >= grade_threshold in the identified columns
  df_filtered <- df %>%
    mutate(across(where(is.POSIXct), as.character)) %>%
    rowwise() %>%
    filter(if_any(all_of(outcome_columns), ~ threshold(., grade_threshold))) %>%
    ungroup() %>%
    select(embrace_id, selection_columns) %>%
    left_join(baseline_df)

  print(paste0("Found ", nrow(df_filtered), " events"))

  df_filtered <- embraceR::replace_neg_one_with_NA(df_filtered)

  return(df_filtered)
}


#' Reorder Columns Based on Assessment Dates
#'
#' Reorders columns in a dataframe chronologically based on their corresponding 
#' assessment dates, facilitating temporal analysis of side effects.
#'
#' @param df DataFrame with columns to be reordered
#' @return DataFrame with columns reordered chronologically
#' @export
reorder_columns_based_on_date <- function(df) {
  # Extract the column names to be reordered
  columns_to_reorder <- colnames(df)[4:ncol(df)]

  # Extract the dates from the 'assessment_date' row
  assessment_dates <- df %>%
    filter(side_effect == "assessment_date") %>%
    select(all_of(columns_to_reorder)) %>%
    as.character()

  # Create a named vector of the dates
  date_vector <- setNames(assessment_dates, columns_to_reorder)

  # Sort the columns based on the dates
  sorted_columns <- names(sort(date_vector, na.last = TRUE))

  # Reorder the columns in the dataframe
  df <- df %>%
    select(embrace_id, side_effect, `Diagnosis & Treatment`, all_of(sorted_columns))

  return(df)
}


#' Move a Specific Row to a New Position
#'
#' This function moves a specified row in a dataframe to a new position.
#'
#' @param df A dataframe containing the data.
#' @param row_from The index of the row to move.
#' @param row_to The new index for the row.
#' @return A dataframe with the row moved to the new position.
#' @examples
#' \dontrun{
#' df <- tibble::tibble(
#'   a = 1:5,
#'   b = letters[1:5]
#' )
#' new_df <- move_row(df, 5, 2)
#' print(new_df)
#' }
#' @export
move_row <- function(df, row_from, row_to) {
  if (row_from < 1 || row_from > nrow(df) || row_to < 1 || row_to > nrow(df)) {
    stop("Invalid row indices.")
  }

  # Extract the row to move
  row_to_move <- df[row_from, ]

  # Remove the row from the original position
  df <- df[-row_from, ]

  # Insert the row at the new position
  df <- rbind(df[1:(row_to - 1), ], row_to_move, df[row_to:nrow(df), ])

  return(df)
}


#' Reshape Side Effects Data
#'
#' Converts the side effects data from wide to long format, separating side effect names and time points.
#'
#' @param df DataFrame, the data to be reshaped.
#' @return DataFrame in long format with separated side effect names and time points.
#' @export
reshape_side_effects <- function(df) {
  # Convert all columns except 'embrace_id' to character
  df <- df %>%
    mutate(across(-embrace_id, as.character))

  # Pivot longer
  df_long <- df %>%
    pivot_longer(
      -embrace_id,
      names_to = "side_effect_time",
      values_to = "value"
    )

  # Separate the base name and time point
  df_long <- df_long %>%
    separate(side_effect_time, into = c("side_effect", "time"),
             sep = "_(?=\\d+m$|baseline_morbidity$|bm_4w$|bm_eort$)")
             # sep="_(?=[^_]+$)")

  df_wide <- df_long %>%
    pivot_wider(
      names_from = time,
      values_from = value
    ) %>% rename("Diagnosis & Treatment" = "NA")

  df_wide <- reorder_columns_based_on_date(df_wide)
  df_wide <- df_wide %>% select(
    embrace_id,
    side_effect,
    "Diagnosis & Treatment",
    baseline_morbidity,
    bm_4w,
    bm_eort,
    everything()
  )

  # df_wide <- move_row(df_wide, 12, 9) # Fix for assessment date
  # df_wide <- move_row(df_wide, 8, 1)
  # df_wide <- move_row(df_wide, 1, 2)
  # df_wide <- move_row(df_wide, 10, 2)



  return(df_wide)
}




#' Create Styled Excel Report for Patient Side Effects
#'
#' Generates a formatted Excel report with conditional formatting based on side effect 
#' grades, highlighting severe events for easy identification.
#'
#' @param data DataFrame containing the patient side effect data
#' @param filePath Character, the path where the Excel file will be saved
#' @export
#' @import openxlsx
createStyledExcelSheet <- function(data, filePath, sheetName = "Side Effects") {

  # Create a new workbook
  wb <- createWorkbook()

  # Add a worksheet with the given name
  addWorksheet(wb, sheetName)

  # Define styles
  colorScale <- c("#b7e4c7", "#74c476", "#31a354", "#fdd0a2", "#fd8d3c", "#e6550d") # Gradient from light green to deep red
  styles <- lapply(colorScale, function(color) {
    createStyle(fgFill = color, fontColour = "#000000", border = "TopBottomLeftRight", borderColour = "black", wrapText = TRUE)
  })

  naStyle <- createStyle(fgFill = "#F0F0F0", fontColour = "#000000", border = "TopBottomLeftRight", borderColour = "black", wrapText = TRUE)
  nineStyle <- createStyle(fgFill = "#F0F0F0", fontColour = "#000000", border = "TopBottomLeftRight", borderColour = "black", wrapText = TRUE)
  textStyle <- createStyle(fgFill = "#B0C4DE", fontColour = "#000000", border = "TopBottomLeftRight", borderColour = "black", wrapText = TRUE)

  # Set column widths and apply text wrapping
  setColWidths(wb, sheet = sheetName, cols = 2, widths = "auto") # Specifically widen the second column

  # Apply styles based on cell content
  applyStyles <- function(value, styles, naStyle, nineStyle, textStyle) {
    if (!is.na(value) && value != "") {
      if (value %in% c("0", "1", "2", "3", "4", "5")) {
        return(styles[[as.numeric(value) + 1]])
      } else if (value == "9") {
        return(nineStyle)
      } else {
        return(textStyle)
      }
    } else {
      return(naStyle)
    }
  }

  for (row in 1:nrow(data)) {
    for (col in 3:ncol(data)) { # Skip the first two columns
      value <- as.character(data[row, col])
      style <- applyStyles(value, styles, naStyle, nineStyle, textStyle)
      addStyle(wb, sheet = sheetName, style = style, rows = row + 1, cols = col, gridExpand = FALSE)
    }
  }

  # Write data to the worksheet
  writeData(wb, sheet = sheetName, x = data)

  # Freeze the first row and the first column
  freezePane(wb, sheet = sheetName, firstRow = TRUE, firstCol = TRUE)

  # Save the workbook
  saveWorkbook(wb, filePath, overwrite = TRUE)

  cat("Workbook saved as", filePath, "\n")
}


#' Generate Patient Excel Reports
#'
#' Creates individual Excel reports for each patient in the filtered dataset,
#' organizing files by center ID in appropriate subfolders.
#'
#' @param df_filtered DataFrame containing filtered patient data
#' @param root_folder Character, the root directory for saving reports
#' @export
generatePatientExcelReports <- function(df_filtered, root_folder) {

  # Get the current date to prefix the filenames
  currentDate <- format(Sys.Date(), "%Y-%m-%d")

  for (i in 1:nrow(df_filtered)) {
    # Reshape side effects for the current row/patient
    single_row <- df_filtered[i, ]
    reshaped_df <- reshape_side_effects(single_row)


    # Construct the subfolder and filename using the current date and patient ID
    patientID <- as.character(reshaped_df$embrace_id[1])
    center <- substr(patientID, 1, 3)
    subfolder_path <- file.path(root_folder, center)

    # Check if the subfolder exists, and create it if it doesn't
    if (!dir.exists(subfolder_path)) {
      dir.create(subfolder_path, recursive = TRUE)
    }

    # Construct the filename
    fileName <- paste0(currentDate, "_", patientID, "_side_effects.xlsx")
    filePath <- file.path(subfolder_path, fileName)

    # Generate the Excel report for the current patient
    createStyledExcelSheet(reshaped_df, filePath)
  }

  cat("Reports generated successfully in the root folder:", root_folder, "\n")
}



#' Generate Excel Reports for Patients with Grade-Specific Events
#'
#' Processes patient data to identify those with side effects meeting a specified 
#' grade threshold, then generates formatted Excel reports for clinical review.
#'
#' @param data DataFrame containing the complete patient dataset
#' @param outputDir Character, the output directory for Excel reports
#' @param grade_threshold Numeric, the CTCAE grade threshold to filter by
#' @export
generateExcelReportsForPatients <- function(data, outputDir, grade_threshold) {

  # Clean side effect names
  side_effect_cols <- embraceR::clean_side_effect_names(data, "_3m")

  # Append other ctcae questions to the side effect columns
  side_effect_cols <- c(
    "assessment_date",
    "ctcae_assessment_date",
    "general_comments",
    side_effect_cols,
    "vagina_hormonal_therapy",
    "vagina_regular_dilatation",
    "disease_local_status",
    "disease_nodal_status",
    "disease_systemic_status",
    "report_adverse_event_text",
    "gastro_fecal_urgency",
    "gastro_hemorrhage_bleeding_gi",
    "gastro_stenosis_stricture_gi",
    "bladder_hemorrhage_bleeding_gu",
    "bladder_stricture_stenosis_gu",
    "vag_adhesions",
    "vag_adhesions_reopen",
    "fistula",
    "fistula_localization",
    "bladder_other_text",
    "gastro_bleeding_other_text",
    "gastro_stenosis_stricture_other_text",
    "gastro_intestinal_other_text",
    "vagina_other_text",
    "muscle_fibrosis_other_text",
    "other1text",
    "other2text",
    "other3text",
    "other4text",
    "other5text",
    "other_relevant_findings"
    )

  # Filter the dataframe based on a grade threshold
  df_filtered <- filter_grade_threshold(data, side_effect_cols, grade_threshold)



  # Generate Excel reports for each filtered row
  generatePatientExcelReports(df_filtered, outputDir)
}




#' Add Grade-Specific CTCAE Event Indicator
#'
#' Adds a binary indicator column showing whether each patient has experienced
#' any side effect meeting the specified grade threshold.
#'
#' @param df DataFrame containing patient data
#' @param grade_threshold Numeric, the CTCAE grade threshold to check for
#' @return DataFrame with an additional indicator column for grade-specific events
#' @export
#' @import dplyr purrr stringr glue
add_grade_ctcae_event_column <- function(df, grade_threshold) {

  threshold_cols <- embraceR::clean_side_effect_names(df, "_3m")

  # Determine outcome columns based on the patterns in threshold_cols
  outcome_columns <- names(df) %>%
    purrr::keep(~any(str_detect(., paste0("^", threshold_cols, ".*")))) %>%
    purrr::discard(~str_detect(., "baseline_morbidity$|bm_4w$|bm_eort$")) %>%
    purrr::discard(~str_detect(., "text"))  # Exclude columns containing "text"

  # Dynamic column name based on the grade threshold
  event_column_name <- glue::glue("has_grade{grade_threshold}_ctcae_event")

  # Create the new column dynamically named after the grade threshold
  df <- df %>%
    rowwise() %>%
    mutate("{event_column_name}" := any(c_across(all_of(outcome_columns)) %>% map_lgl(~ threshold(., grade_threshold)))) %>%
    ungroup()

  return(df)
}


#' Generate Excel Reports for Grade-Specific Morbidity Events
#'
#' Processes EMBRACE-II data to identify patients with side effects meeting a specified 
#' grade threshold, then generates formatted Excel reports for clinical review.
#'
#' @param grade_threshold Numeric, the CTCAE grade threshold to filter by (default: 4)
#' @param output_dir Character, the output directory for Excel reports (default: "Grade{threshold}/")
#'
#' @return Invisible NULL, called for its side effect of generating Excel reports
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Generate reports for Grade 4 events
#' generate_morbidity_reports(4)
#' 
#' # Generate reports for Grade 3 events with custom output directory
#' generate_morbidity_reports(3, "Grade3_Reports/")
#' }
generate_morbidity_reports <- function(grade_threshold = 4, 
                                       output_dir = paste0("Grade", grade_threshold, "/")) {
  # Load and prepare EMBRACE-II data
  emii <- embraceR::load_embrace_ii() %>% 
    embraceR::recode_and_convert_all_columns()
  
  # Generate Excel reports for patients with events at the specified grade threshold
  generateExcelReportsForPatients(emii, output_dir, grade_threshold)
  
}




