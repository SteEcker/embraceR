# library(tidyverse)
# library(tidyr)
# library(dplyr)
# library(openxlsx)
# library(lubridate)

#' Apply Threshold to Data
#'
#' This function applies a threshold to a given data set to determine if the grade
#' is above a specified level.
#'
#' @param x Numeric, the value to be checked against the threshold.
#' @param grade_threshold Numeric, the threshold value to compare against.
#' @return Logical, TRUE if the value is above the threshold, otherwise FALSE.
#' @examples
#' threshold(5, grade_threshold = 4)
#' threshold(NA, grade_threshold = 4)
#' @export
threshold <- function(x, grade_threshold) {
  if (!is.na(x) && !is.character(x) && x != 9) {
    if (x >= grade_threshold) {
      print(x)
    }
    return(x == grade_threshold)
  } else {
    return(FALSE)
  }
}



#' Filter Data Based on Grade Threshold
#'
#' Filters a dataframe based on specified side effect columns and a grade threshold,
#' retaining rows that meet the threshold criteria.
#'
#' @param df DataFrame containing the data to be filtered.
#' @param side_effect_cols Character vector specifying the columns to check against the threshold.
#' @return DataFrame filtered based on the grade threshold.
#' @export
#' @import tidyr
#' @import dplyr
#' @import stringr
#' @import lubridate
filter_grade_threshold <- function(df, side_effect_cols,  grade_threshold = 4) {

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
    # purrr::discard(~str_detect(., "baseline_morbidity$|bm_4w$|bm_eort$")) %>% # Followup
    # purrr::discard(~str_detect(., "bm_4w$|bm_eort$|m$")) %>% # Baseline
    purrr::discard(~str_detect(., "baseline_morbidity$|m$")) %>% # Acute
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
#' This function reorders all columns in a dataframe after the third column
#' based on their corresponding assessment dates.
#'
#' @param df A dataframe containing the data to be reordered. The first three
#' columns should be `embrace_id`, `side_effect`, and `Diagnosis & Treatment`.
#' The subsequent columns should be named with time points (e.g., `3m`, `6m`).
#' @return A dataframe with the columns reordered based on the assessment dates.
#' @details The function assumes that there is a row in the dataframe where the
#' `side_effect` column is "assessment_date" and the subsequent columns contain
#' the corresponding assessment dates. The function extracts these dates, sorts
#' the columns accordingly, and returns the reordered dataframe.
#' @examples
#' \dontrun{
#' df <- tibble::tibble(
#'   embrace_id = c("AAR2005", "AAR2005"),
#'   side_effect = c("general_comments", "assessment_date"),
#'   `Diagnosis & Treatment` = c(NA, NA),
#'   `3m` = c(NA, "2017-01-01"),
#'   `6m` = c(NA, "2017-07-01"),
#'   `9m` = c(NA, "2017-10-01")
#' )
#' reordered_df <- reorder_columns_based_on_date(df)
#' }
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




#' Create Styled Excel Sheet
#'
#' Creates an Excel sheet with styled cells based on the data provided.
#'
#' @param data DataFrame to be written to the Excel sheet.
#' @param filePath Character, path where the Excel file will be saved.
#' @param sheetName Character, name of the sheet within the Excel file.
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
#' For each row in the filtered dataframe, generates an Excel report with a filename based on the patient ID.
#'
#' @param df_filtered DataFrame, the filtered data for which reports will be generated.
#' @param root_folder Character, the root folder where subfolders and Excel files will be saved.
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



#' Generate Excel Reports for Patients
#'
#' Cleans side effect names, filters the data based on a grade threshold, and generates Excel reports for each patient.
#'
#' @param data DataFrame, the data to be processed.
#' @param outputDir Character, the output directory for the Excel reports.
#' @param grade_threshold Grade threshold
#'
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
    "bladder_stenosis_stricture",
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




#' Add a Column for Grade XX CTCAE Event
#'
#' This function selects all outcome columns and adds a new column named dynamically
#' based on the grade threshold (e.g., "grade4_ctcae_event").
#'
#' @param df DataFrame containing the data.
#' @param grade_threshold Numeric, the threshold value to compare against.
#' @return DataFrame with an additional column "has_gradeX_ctcae_event" where X is the grade threshold.
#' @export
#' @import dplyr
#' @import purrr
#' @import stringr
#' @import glue
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

# # Example usage
# emii <- emii %>% recode_and_convert_all_columns(mapping_path = "./data_raw/embrace_II/factor_labels.json") %>% emii_add_number_common_iliac_ln_stat_d() %>% emii_add_number_paraaortic_ln_stat_d() %>% replace_neg_one_with_NA()


# generateExcelReportsForPatients(emii, "Grade4/", 4)




#
#
# #
# get_morbidity_completeness <- function(df, side_effect_cols, group_name) {
#   side_effect_cols <- c(
#     "assessment_date",
#     "planned_followup",
#     "disease_local_status",
#     "disease_nodal_status",
#     "disease_systemic_status",
#     side_effect_cols)
#
#   # Determine outcome columns
#   outcome_columns <- names(df) %>%
#     purrr::keep(~any(str_detect(., paste0("^", side_effect_cols, ".*")))) %>%
#     purrr::discard(~str_detect(., "text_"))
#
#   outcome_columns <- outcome_columns[!sapply(df[outcome_columns], is.character)]
#
#   # Select relevant columns
#   df <- df %>% select(embrace_id, any_of(outcome_columns))
#
#   # Pivot and filter the dataframe
#   df_long <- df %>%
#     pivot_longer(
#       -embrace_id,
#       names_to = c(".value", "Timepoint"),
#       names_pattern = "^(.*?)(?:_baseline_morbidity|_bm_eort|_bm_4w|_([0-9]+m))$"
#     ) %>%
#     filter(Timepoint != "") %>%
#     filter(!(is.na(assessment_date) & is.na(planned_followup))) %>%
#     filter(!if_any(c("disease_local_status", "disease_nodal_status", "disease_systemic_status"), ~ (. == 2) & !is.na(.)))
#
#   # Calculate column completeness and filter by mandatory threshold
#   column_completeness <- colSums(!is.na(df_long)) / nrow(df_long) * 100
#   mandatory_threshold <- 90
#   essential_cols <- names(column_completeness[column_completeness >= mandatory_threshold])
#
#   df_filtered <- df_long %>%
#     select(all_of(essential_cols), -assessment_date) %>%
#     embraceR::replace_neg_one_with_NA()
#
#   # Calculate completeness for each row
#   df_completeness <- df_filtered %>%
#     rowwise() %>%
#     mutate(completeness = mean(!is.na(c_across(starts_with(side_effect_cols))), na.rm = TRUE) * 100) %>%
#     ungroup()
#
#   # Summarize to get the average completeness per patient across all follow-ups
#   average_completeness <- df_completeness %>%
#     group_by(embrace_id) %>%
#     summarise(average_completeness = mean(completeness, na.rm = TRUE)) %>%
#     mutate(group = group_name)
#
#   return(average_completeness)
# }
#
#
# # USAGE
# #
# #
# df <- emii
#
# side_effect_cols <- embraceR::clean_side_effect_names(df, "_3m")
# average_completeness_ctcae <- get_morbidity_completeness(df, side_effect_cols, "CTCAE")
#
# side_effect_cols <- embraceR::clean_eortc_names(df, "_3m")
# average_completeness_eortc <- get_morbidity_completeness(df, side_effect_cols, "EORTC")
#
#
# openxlsx::write.xlsx(rbind(average_completeness_ctcae, average_completeness_eortc), file = "2024_ctcae_eortc_comlpeteness.xlsx")
#
#
# avg_completeness_ctcae <- emii %>%
#   select(embrace_id, centre_id) %>%
#   left_join(average_completeness_ctcae) %>%
#   group_by(centre_id) %>%
#   summarise(average_completeness = mean(average_completeness, na.rm = TRUE)) %>%
#   ungroup() %>%
#   mutate(group = "CTCAE")
#
# avg_completeness_eortc <- emii %>%
#   select(embrace_id, centre_id) %>%
#   left_join(average_completeness_eortc) %>%
#   group_by(centre_id) %>%
#   summarise(average_completeness = mean(average_completeness, na.rm = TRUE)) %>%
#   ungroup() %>%
#   mutate(group = "EORTC")
#
# avg_completeness <- rbind(avg_completeness_ctcae, avg_completeness_eortc) %>%
#   mutate(centre_id = as.factor(centre_id))
#
#
#
#
# library(ggplot2)
#
# # Adjusting the ggplot function as per your requirements
# ggplot(avg_completeness, aes(x = centre_id, y = group, fill = average_completeness)) +
#   geom_tile(color = "white", size = 0.1) +  # Adds a white border for better tile distinction
#   scale_fill_gradientn(colors = c("red", "darkorange", "green"),
#                        values = scales::rescale(c(0, 50, 100)),
#                        name = "Avg Completeness (%)",
#                        limits = c(0, 100)) +
#   labs(title = "",
#        x = "Centre ID",
#        y = "Category") +
#   theme_minimal(base_size = 18) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),
#         axis.title.y = element_blank()) +
#   coord_fixed(ratio = 2)
#
#


