#' Check for Specific Nodal Location
#'
#' Checks whether patients have lymph nodes in specific anatomical locations,
#' either at diagnosis, during follow-up, or at any time point.
#'
#' @param .data A data frame containing patient node data
#' @param location Character string specifying the anatomical location to check
#' @param time_point Character string specifying which timepoint to check:
#'   "diagnosis", "followup", or "any" (default)
#'
#' @return A tibble with patient IDs and boolean indicators for the specified location
#'
#' @keywords internal
#'
#' @import dplyr
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' # Check for right internal iliac nodes at any time
#' result <- check_nodal_location(patient_data, "R int iliac")
#' 
#' # Check only at diagnosis
#' diagnosis_nodes <- check_nodal_location(patient_data, "R int iliac", 
#'                                        time_point = "diagnosis")
#' }
check_nodal_location <- function(.data, location, time_point = "any") {
  # Input validation
  valid_locations <- get_valid_locations()
  if (!location %in% valid_locations) {
    stop(
      "Invalid location specified. Must be one of: ",
      paste(valid_locations, collapse = ", ")
    )
  }

  if (!time_point %in% c("diagnosis", "followup", "any")) {
    stop("time_point must be one of: 'diagnosis', 'followup', 'any'")
  }

  # Get nodes at diagnosis once and recode positions
  nodes_diagnosis <- emii_get_lymph_nodes_statd(.data) %>%
    select(embrace_id, anatomical_position, node_num = node_number) %>%
    mutate(time = "diagnosis")

  # Get nodes at followup using optimized processing
  nodes_followup <- process_followup_nodes(.data, nodes_diagnosis) %>%
    rename(anatomical_position = final_position) %>%
    mutate(time = "followup")

  # Combine based on time_point parameter
  nodes_combined <- if (time_point == "diagnosis") {
    nodes_diagnosis
  } else if (time_point == "followup") {
    nodes_followup
  } else {
    bind_rows(nodes_diagnosis, nodes_followup)
  }

  # Check for specified location and recode NAs to FALSE
  result <- nodes_combined %>%
    group_by(embrace_id) %>%
    summarise(
      has_location = any(anatomical_position == location, na.rm = FALSE),
      .groups = "drop"
    )

  return(result)
}

#' Check for Multiple Nodal Locations
#'
#' Extends check_nodal_location to check for multiple locations simultaneously
#' and adds the results as new columns to the original data frame.
#'
#' @param .data A data frame containing patient node data
#' @param locations Character vector of anatomical locations to check
#' @param time_point Character string specifying which timepoint to check
#'
#' @return Original data frame with additional boolean columns for each location
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Check for multiple locations
#' locations <- c("R int iliac", "L int iliac", "Para Aortic")
#' result <- check_multiple_locations(patient_data, locations)
#' }
check_multiple_locations <- function(.data, locations, time_point = "any") {
  # Map over locations and combine results
  results <- purrr::map_dfc(locations, function(loc) {
    check_nodal_location(.data, loc, time_point) %>%
      select(has_location) %>%
      rename_with(~ paste0("has_", make.names(loc), "_", time_point))
  })

  # Add patient ID
  results <- check_nodal_location(.data, locations[1], time_point) %>%
    select(embrace_id) %>%
    bind_cols(results)

  # Join results back to original data and replace NAs with FALSE
  left_join(.data, results, by = "embrace_id") %>%
    mutate(
      across(
        starts_with("has_") & ends_with(time_point),
        ~replace_na(., FALSE)
        # ~ if_else(
        #   .data$event_nodalfailure == 0, # Condition to check
        #   replace_na(., FALSE),           # Replace NA if condition is true
        #   .                              # Keep original value if condition is false
        # )
      )
    )
}

#' Process Follow-up Node Data
#'
#' Internal helper function to efficiently process follow-up node data by
#' extracting and transforming node information from multiple follow-up visits.
#'
#' @param .data A data frame containing follow-up node information
#' @param diagnosis_nodes Lookup table of nodes at diagnosis
#'
#' @return A tibble with processed follow-up node data
#'
#' @keywords internal
process_followup_nodes <- function(.data, diagnosis_nodes) {
  followup_data <- .data %>%
    select(
      embrace_id,
      matches("^mri_recurrent_nodes\\d+_\\d+m$"),
      matches("^node_anatomical_position\\d+_\\d+m$"),
      matches("^mri_node_no_at_diagnosis\\d+_\\d+m$")
    ) %>%
    pivot_longer(
      cols = -embrace_id,
      names_to = "variable",
      values_to = "value"
    ) %>%
    mutate(
      node_num = str_extract(variable, "\\d+(?=_\\d+m)"),
      visit = str_extract(variable, "\\d+m$"),
      type = case_when(
        str_detect(variable, "^mri_recurrent_nodes") ~ "recurrent",
        str_detect(variable, "^node_anatomical_position") ~ "position",
        str_detect(variable, "^mri_node_no_at_diagnosis") ~ "diagnosis_ref"
      )
    ) %>%
    filter(!is.na(value)) %>%
    select(-variable) %>%
    pivot_wider(
      names_from = type,
      values_from = value
    ) %>%
    # Recode the position values
    mutate(
      position = recode(position, !!!NODAL_POSITION_MAPPING)
    )

  # Join with diagnosis lookup and determine final position
  result <- followup_data %>%
    left_join(
      diagnosis_nodes %>% mutate(node_num = as.numeric(node_num)),
      by = c("embrace_id", "diagnosis_ref" = "node_num")
    ) %>%
    mutate(
      final_position = case_when(
        recurrent == 1 ~ anatomical_position,
        recurrent == 0 ~ position,
        TRUE ~ NA_character_
      )
    ) %>%
    # filter(!is.na(final_position)) %>%
    select(embrace_id, final_position) %>%
    distinct()

  return(result)
}

#' Add Recurrent Node Locations
#'
#' Adds boolean columns for all possible nodal locations at follow-up to identify
#' recurrent disease patterns in EMBRACE-II patients.
#'
#' @param .data A data frame containing patient node data
#'
#' @return Data frame with additional boolean columns for each nodal location at follow-up
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' emii_data <- load_embrace_ii()
#' result <- emii_add_recurrent_nodes(emii_data)
#' }
emii_add_recurrent_nodes <- function(.data) {
  check_multiple_locations(
    .data,
    locations = get_valid_locations(),
    time_point = "followup"
  )
}

#' Add Diagnostic Node Locations
#'
#' Adds boolean columns for all possible nodal locations at diagnosis to identify
#' initial disease patterns in EMBRACE-II patients.
#'
#' @param .data A data frame containing patient node data
#'
#' @return Data frame with additional boolean columns for each nodal location at diagnosis
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' emii_data <- load_embrace_ii()
#' result <- emii_add_diagnostic_nodes(emii_data)
#' }
emii_add_diagnostic_nodes <- function(.data) {
  check_multiple_locations(
    .data,
    locations = get_valid_locations(),
    time_point = "diagnosis"
  )
}

#' Add Nodal Classification
#'
#' Adds a new column classifying nodal status as 'N0', 'N1PAN', or 'N1pelvic'
#' based on pathological nodes present and Para-Aortic lymph node status at diagnosis.
#'
#' @param .data A data frame containing patient node data
#'
#' @return Data frame with an additional 'nodal_classification' column
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' emii_data <- load_embrace_ii()
#' result <- emii_add_nodal_classification(emii_data)
#' }
emii_add_nodal_classification <- function(.data) {
  # First ensure we have Para Aortic status at diagnosis
  data_with_nodes <- .data %>%
    check_nodal_location(location = "Para Aortic", time_point = "diagnosis") %>%
    right_join(.data, by = "embrace_id") %>%
    mutate(
      nodal_classification = case_when(
        pathological_nodes_present == 0 ~ "N0",
        pathological_nodes_present == 1 & has_location ~ "N1PAN",
        pathological_nodes_present == 1 & !has_location ~ "N1pelvic",
        TRUE ~ NA_character_
      )
    ) %>%
    select(-has_location)  # Remove the temporary column

  return(data_with_nodes)
}

#' Generate Recurrence and Metastases Summary
#'
#' Creates a formatted summary table showing the number and percentage of patients
#' who have recurrent lymph nodes or metastases at follow-up from EMBRACE-II.
#'
#' @param df A data frame containing EMBRACE-II data with follow-up information
#'
#' @return A gt table object showing the distribution of recurrences and metastases
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' emii_data <- load_embrace_ii()
#' summary_table <- get_recurrence_and_metastases_summary(emii_data)
#' }
get_recurrence_and_metastases_summary <- function(df) {
    # Get recurrent nodes data
    nodes_data <- check_multiple_locations(
        df,
        locations = get_valid_locations(),
        time_point = "followup"
    )

    # Get metastases data
    metastases_data <- add_metastases(df)

    # Combine the data
    combined_data <- nodes_data %>%
        select(embrace_id, matches("has_.*_followup$")) %>%
        left_join(
            metastases_data %>%
            select(embrace_id, starts_with("has_")),
            by = "embrace_id"
        ) %>%
        select(-embrace_id)

    # Create summary table
    combined_data %>%
        gtsummary::tbl_summary(
            # Clean up labels for both nodes and metastases
            label = create_combined_labels()
        ) %>%
        gtsummary::modify_header(
            label = "**Location**",
            stat_0 = "**N = {N}**"
        ) %>%
        gtsummary::add_n() %>%
        gtsummary::bold_labels() %>%
        gtsummary::as_gt() %>%
        gt::tab_header(
            title = "Distribution of Recurrences and Metastases at Follow-up",
            subtitle = "N = Number of patients with follow-up data"
        )
}

#' Create Combined Labels for Summary Table
#'
#' Helper function to create formatted labels for the recurrence and metastases
#' summary table.
#'
#' @return A named list of labels for gtsummary
#'
#' @keywords internal
create_combined_labels <- function() {
    # Node labels
    node_locations <- get_valid_locations()
    node_names <- paste0("has_", make.names(node_locations), "_followup")
    node_values <- paste0("LN: ", gsub("\\.", " ", node_locations))
    node_labels <- stats::setNames(as.list(node_values), node_names)

    # Metastases labels
    metastases_patterns <- c(
        "paraaortic_nodes_above_l2" = "Para-aortic nodes above L2",
        "supraclavicular_nodes" = "Supraclavicular nodes",
        "mediastinal_nodes" = "Mediastinal nodes",
        "liver_metastases" = "Liver Metastases",
        "bone_metastases" = "Bone Metastases",
        "brain_metastases" = "Brain Metastases",
        "lung_metastases" = "Lung Metastases",
        "abdominal_carcinomatosis" = "Abdominal Carcinomatosis",
        "other_metastases" = "Other Metastases"
    )
    met_names <- paste0("has_", names(metastases_patterns))
    met_labels <- stats::setNames(as.list(metastases_patterns), met_names)

    # Combine both label lists
    c(node_labels, met_labels)
}
