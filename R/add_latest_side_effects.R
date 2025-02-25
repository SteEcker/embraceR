#' Clean Side Effect Column Names
#'
#' Extracts and cleans side effect column names by removing time suffixes.
#'
#' @param df The data frame containing the side effect columns
#' @param time_suffix The suffix indicating the time point (e.g., "_3m")
#'
#' @return A vector of cleaned side effect column names
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' df <- data.frame(bladder_pain_3m = c(1,2,3), gastro_diarrhea_3m = c(0,1,2))
#' clean_names <- clean_side_effect_names(df, "_3m")
#' }
clean_side_effect_names <- function(df, time_suffix = "_3m") {
  # Select columns based on the provided time_suffix
  selected_cols <- df %>%
    select(
      starts_with("bladder_") & ends_with(time_suffix),
      starts_with("gastro_") & ends_with(time_suffix),
      starts_with("vagina_") & ends_with(time_suffix),
      starts_with("fistula") & ends_with(time_suffix),
      starts_with("muscle_") & ends_with(time_suffix),
      starts_with("other") & ends_with(time_suffix),
      starts_with("lymphatics_") & ends_with(time_suffix)
    ) %>%
    # Discard endpoints that do not have CTCAE Grade scale
    select(- c(starts_with("vagina_hormonal_therapy"),
               starts_with("vagina_regular_dilatation"),
               starts_with("other_metastases"),
               starts_with("gastro_fecal_urgency"),
               starts_with("gastro_hemorrhage_bleeding_gi"),
               starts_with("gastro_stenosis_stricture_gi"),
               starts_with("bladder_hemorrhage_bleeding_gu"),
               starts_with("bladder_stricture_stenosis"),
               )

           ) %>%
    select_if(function(x) is.numeric(x) | is.factor(x)) %>%
    colnames()

  # Remove the time_suffix from the column names
  cleaned_names <- gsub(paste0(time_suffix, "$"), "", selected_cols)

  return(cleaned_names)
}


#' Clean EORTC Column Names
#'
#' Extracts and cleans EORTC column names by removing time suffixes.
#'
#' @param df The data frame containing the EORTC columns
#' @param time_suffix The suffix indicating the time point (e.g., "_3m")
#'
#' @return A vector of cleaned EORTC column names
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' df <- data.frame(eortc_q1_3m = c(1,2,3), eortc_q2_3m = c(3,4,5))
#' clean_names <- clean_eortc_names(df, "_3m")
#' }
clean_eortc_names <- function(df, time_suffix) {
  # Select columns based on the provided time_suffix
  selected_cols <- df %>%
    select(
      starts_with("eortc") & ends_with(time_suffix)
    ) %>%
    colnames()

  # Remove the time_suffix from the column names
  cleaned_names <- gsub(paste0(time_suffix, "$"), "", selected_cols)

  return(cleaned_names)
}


#' Get Latest Side Effects
#'
#' Extracts the most recent side effect values for each patient based on follow-up data.
#'
#' @param df The data frame containing side effect and follow-up columns
#'
#' @return A data frame with new columns for the latest side effects
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   embrace_id = 1:3,
#'   latest_followup_id = c("3", "6", "12"),
#'   bladder_pain_3m = c(1,2,3),
#'   bladder_pain_6m = c(2,1,NA),
#'   bladder_pain_12m = c(NA,NA,1)
#' )
#' result <- get_latest_side_effects(df)
#' }
get_latest_side_effects <- function(df) {

  message("Getting latest side effects...")

  # List of base column names for the side effects
  side_effect_cols <- clean_side_effect_names(df, "_3m")


  get_latest_side_effect <- function(row, side_effect_base_name) {
    followup_id <- row["latest_followup_id"]
    col_name <- paste0(side_effect_base_name, "_", followup_id, "m")

    if (col_name %in% names(row)) {
      return(row[[col_name]])
    } else {
      return(NA)
    }
  }

  # Create new columns for the latest side effect values
  for (side_effect in side_effect_cols) {
    new_col_name <- paste0(side_effect, "_latest_fu")
    df[[new_col_name]] <- apply(df, 1, function(row) get_latest_side_effect(row, side_effect))
  }

  df <- df %>%
    mutate(across(ends_with("_latest_fu"), as.numeric)) %>%
    mutate(across(ends_with("_latest_fu"), ~ replace(., . == -1, NA)))

  return(df)
}


#' Get Maximum Side Effects
#'
#' Identifies the maximum grade of each side effect and its corresponding timepoint.
#'
#' @param df A data frame with side effect columns across multiple timepoints
#' @param endpoints A character vector of endpoint basenames
#' @param max_grade Numeric threshold for filtering results
#' @param export_to_excel Whether to export results to Excel
#' @param file_name Name of the Excel file if exporting
#'
#' @return A data frame with maximum side effect values and timepoints
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' endpoints <- c("bladder_pain", "gastro_diarrhea")
#' result <- get_max_side_effects(patient_data, endpoints, max_grade=2)
#' }
get_max_side_effects <- function(df, endpoints, max_grade=0,
                                 export_to_excel = FALSE,
                                 file_name = "max_side_effects.xlsx") {

  # Hardcoded timepoints vector based on your data
  timepoints <- c("3m", "6m", "9m", "12m", "18m", "24m", "30m", "36m",
                  "48m", "60m", "72m", "84m", "96m", "1000m", "1001m",
                  "1002m", "1003m", "1004m", "1005m", "1006m", "1007m")

  # Generate the full column names for all endpoints and timepoints
  all_cols <- unlist(lapply(endpoints, function(endpoint) paste0(endpoint, "_", timepoints)))

  # Check that all columns exist in the data frame
  existing_cols <- intersect(all_cols, colnames(df))

  # Reshape data from wide to long format, including the "embrace_id" column
  long_df <- df %>%
    dplyr::select(embrace_id, all_of(existing_cols)) %>%
    tidyr::pivot_longer(
      cols = -embrace_id,
      names_to = c("endpoint", "timepoint"),
      names_pattern = "(.+)_(\\d+m)$",
      values_to = "value"
    ) %>%
    dplyr::mutate(value = as.numeric(value))  # Ensure values are numeric

  # For each patient and endpoint, find the maximum value and corresponding timepoint
  max_df <- long_df %>%
    dplyr::group_by(embrace_id, endpoint) %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::slice_max(order_by = value, with_ties = FALSE, na_rm = TRUE) %>%
    dplyr::mutate(
      max_value = ifelse(is.finite(value), value, NA),
      max_timepoint = timepoint
    ) %>%
    dplyr::select(embrace_id, endpoint, max_value, max_timepoint) %>%
    dplyr::ungroup()

  # Reshape data back to wide format
  result_df <- max_df %>%
    tidyr::pivot_wider(names_from = endpoint, values_from = c(max_value, max_timepoint))

  # Calculate the overall maximum across all max_value columns for each patient
  result_df <- result_df %>%
    dplyr::mutate(overall_max_morbidity_grade = dplyr::select(., dplyr::starts_with("max_value_")) %>% apply(1, max, na.rm = TRUE))

  # Filter rows where at least one max_value column meets or exceeds max_grade
  result_df <- result_df %>%
    dplyr::filter(dplyr::if_any(dplyr::starts_with("max_value_"), ~ . >= max_grade))

  # Export to Excel if specified
  if (export_to_excel) {
    # Create a new workbook and add a worksheet
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "Max Side Effects")

    # Write the data frame to the worksheet
    openxlsx::writeData(wb, "Max Side Effects", result_df)

    # Apply conditional formatting to highlight cells that meet or exceed max_grade
    max_value_cols <- grep("^max_value_", names(result_df), value = TRUE)

    for (col in max_value_cols) {
      col_index <- which(names(result_df) == col)
      openxlsx::conditionalFormatting(
        wb, "Max Side Effects",
        cols = col_index,
        rows = 2:(nrow(result_df) + 1),  # +1 to account for header row
        rule = paste0(">= ", max_grade),
        style = openxlsx::createStyle(fontColour = "#FFFFFF", bgFill = "#FF5733")  # White font on orange background
      )
    }

    # Save the workbook
    openxlsx::saveWorkbook(wb, file_name, overwrite = TRUE)
  }

  return(result_df)
}
