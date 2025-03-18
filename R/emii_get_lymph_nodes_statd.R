#' Transform Lymph Node Data to Long Format
#'
#' Takes a dataframe with wide-format lymph node data, pivots specified columns to a long format,
#' recodes anatomical positions, and returns a new dataframe where each row represents a single
#' positive lymph node from EMBRACE-II at diagnosis.
#'
#' @param df A dataframe containing patient data with lymph node information
#' 
#' @return A dataframe in long-format with one row per positive lymph node,
#'   including recoded anatomical position descriptions
#'
#' @export
#'
#' @examples
#' \dontrun{
#' emii_data <- load_embrace_ii()
#' ln_data <- emii_get_lymph_nodes_statd(emii_data)
#' }
emii_get_lymph_nodes_statd <- function(df) {

  node_cols <- grep("^node\\d{2}anatomical_position_sta_d$", names(df), value = TRUE)
  ebrt_cols <- grep("^ebrt_(ctv|ptv)_n\\d{2}_(volume|d98)_tdvh$", names(df), value = TRUE)

  # Step 1: Pivot Node Position Information
  node_positions <- df %>%
    select(embrace_id, all_of(node_cols)) %>%
    tidyr::pivot_longer(cols = all_of(node_cols),
                 names_to = "node_number",
                 values_to = "anatomical_position",
                 names_transform = list(node_number = ~ stringr::str_extract(.,"\\d{2}"))) %>%
    filter(!is.na(anatomical_position))

  # Step 2: Pivot Volume and d98 Information
  ebrt_info <- df %>%
    select(embrace_id, all_of(ebrt_cols)) %>%
    tidyr::pivot_longer(cols = all_of(ebrt_cols),
                 names_to = c("treatment", "node_prefix", "node_number", "measure", "other"),
                 names_pattern = "(ebrt)_(ctv|ptv)_n(\\d{2})_(volume|d98)_(tdvh)",
                 values_to = "value") %>%
    filter(!is.na(value)) %>%
    mutate(node_number = as.character(node_number)) %>%  # Ensure node_number is character type
    tidyr::pivot_wider(names_from = c('treatment', 'node_prefix', 'measure'), values_from = 'value', names_prefix = 'node_')

  # Step 3: Extract additional variable from specific columns
  additional_cols <- grep("^erbt_ptv_treatment\\d{2}_tdvh$", names(df), value = TRUE)  # Updated pattern
  additional_info <- df %>%
    select(embrace_id, all_of(additional_cols)) %>%
    tidyr::pivot_longer(cols = all_of(additional_cols),
                        names_to = c("treatment", "node_number", "other"),
                        names_pattern = "^(erbt_ptv_treatment)(\\d{2})_(tdvh)$",
                        values_to = "treatment_value") %>%
    filter(!is.na(treatment_value)) %>%
    mutate(node_number = as.character(node_number)) %>%   # Ensure node_number is character type
    rename(ln_boost = treatment_value)

  # Step 4: Recode the anatomical positions
  node_positions <- node_positions %>%
    mutate(anatomical_position = recode(anatomical_position,
                                        !!!NODAL_POSITION_MAPPING))

  # Step 5: Join the Dataframes
  final_df <- node_positions %>%
    left_join(ebrt_info, by = c('embrace_id', "node_number")) %>%
    left_join(additional_info, by = c('embrace_id', "node_number")) %>%   # Match on embrace_id and node_number
    select(-c(treatment, other.y))

  return(final_df)
}


#' Generate Summary Table of Nodal Anatomical Locations
#'
#' Creates a formatted summary table showing the frequency and percentage
#' of each anatomical location in the nodal dataset from EMBRACE-II at diagnosis.
#'
#' @param df A dataframe containing the EMBRACE-II dataset
#' 
#' @return A gt table object showing the distribution of lymph node locations
#'
#' @export
#'
#' @examples
#' \dontrun{
#' emii_data <- load_embrace_ii()
#' summary_table <- get_nodal_location_summary(emii_data)
#' }
get_nodal_location_summary <- function(df) {
    emii_get_lymph_nodes_statd(df) %>%
        select(anatomical_position) %>%
        gtsummary::tbl_summary(
            label = list(
                anatomical_position = "Anatomical Position"
            )
        ) %>%
        gtsummary::modify_header(
            label = "**Location**",
            stat_0 = "**N = {N}**"  # shows total N in header
        ) %>%
        gtsummary::bold_labels() %>%
        gtsummary::as_gt() %>%
        gt::tab_header(
            title = "Proportion of LN-Anatomical Site at Diagnosis",
            subtitle = "N = Positive Lymph Nodes"
        )
}


#' Generate Patient-Level Summary of Nodal Anatomical Locations
#'
#' Creates a formatted summary table showing the number and percentage of patients
#' who have at least one lymph node in each anatomical location from EMBRACE-II at diagnosis.
#'
#' @param df A dataframe containing the EMBRACE-II dataset
#' 
#' @return A gt table object showing the distribution of patients with lymph nodes by location
#'
#' @export
#'
#' @examples
#' \dontrun{
#' emii_data <- load_embrace_ii()
#' patient_summary <- get_nodal_location_patient_summary(emii_data)
#' }
get_nodal_location_patient_summary <- function(df) {
  # Patients IDs with missing nodal anatomical location:
  # [1] "LON2001" "UTR2041" "AAR2073" "LJU2052" "LON2014"



    emii_get_lymph_nodes_statd(df) %>%
        # Create binary indicators for each location
        distinct(embrace_id, anatomical_position) %>%
        mutate(present = 1) %>%
        tidyr::pivot_wider(
            id_cols = embrace_id,
            names_from = anatomical_position,
            values_from = present,
            values_fill = 0
        ) %>%
        select(-embrace_id) %>%
        gtsummary::tbl_summary(
        ) %>%
        gtsummary::modify_header(
            label = "**Location**",
            stat_0 = "**N = {N}**"
        ) %>%
        gtsummary::bold_labels() %>%
        gtsummary::as_gt() %>%
        gt::tab_header(
            title = "Proportion LN-Anatomical Site at Diagnosis",
            subtitle = "N = N+ patients (excl. 5 with missing anatomical locations)"
        )
}
