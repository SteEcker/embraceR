#' Clean up side effect column names
#'
#' @param df The data frame containing the side effect columns.
#' @param time_suffix The suffix indicating the time point (e.g., "_3m").
#'
#' @return A vector of cleaned-up side effect column names.
#'
#' @export
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
               starts_with("bladder_stenosis_stricture"),
               )

           ) %>%
    select_if(is.numeric) %>%
    colnames()

  # Remove the time_suffix from the column names
  cleaned_names <- gsub(paste0(time_suffix, "$"), "", selected_cols)

  return(cleaned_names)
}


#' Clean up eortc column names
#'
#' @param df The data frame containing the side effect columns.
#' @param time_suffix The suffix indicating the time point (e.g., "_3m").
#'
#' @return A vector of cleaned-up side effect column names.
#'
#' @export
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


#' Get the latest side effects for each patient
#'
#' @param df The data frame containing the side effect and follow-up columns.
#'
#' @return A data frame with new columns for the latest side effects at the latest follow-up ID.
#'
#' @export
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


#' Get maximum side effect values and corresponding timepoints and export to Excel with conditional formatting
#'
#' @param df A data frame containing side effect columns with multiple timepoints and a patient ID column "embrace_id".
#' @param endpoints A character vector of endpoint basenames (e.g., from embraceR::clean_side_effect_names()).
#' @param max_grade Numeric threshold; only patients with at least one max_value >= max_grade will be retained.
#' @param export_to_excel Logical; if TRUE, exports the result to an Excel file.
#' @param file_name Character; the name of the Excel file to export if export_to_excel is TRUE.
#'
#' @return A data frame with maximum side effect values and timepoints for each endpoint and patient.
#'
#' @export
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
