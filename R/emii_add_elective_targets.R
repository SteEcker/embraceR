


#' Determine Availability of Node Data
#'
#' This internal function examines the input data frame to identify rows where node data is available based on specific criteria. It pivots node-related columns to a long format, counts available node data for each `embrace_id`, and then merges this information back into the original data frame to indicate whether node data is available for each case.
#'
#' @param .data A data frame containing patient and node information, including `embrace_id` and columns that start with 'node' and end with 'anatomical_position_sta_d'.
#'
#' @return A data frame with an additional logical column `elective_has_nodal_info` indicating whether node data is fully available for the case.
#'
#' @noRd
node_data_available <- function(.data) {
  # Pivot node-related columns to long format and count available data per 'embrace_id'
  tmp <- .data %>%
    tidyr::pivot_longer(cols = starts_with('node') & ends_with('anatomical_position_sta_d'),
                        names_to = "node", values_to = "value") %>%
    filter(!is.na(value)) %>%
    group_by(embrace_id) %>%
    count(name = "elective_nodes_with_info") %>%
    ungroup()

  # Merge back with original data and indicate availability of nodal info
  .data <- .data %>%
    left_join(tmp, by = 'embrace_id') %>%
    mutate(elective_has_nodal_info = elective_nodes_with_info == pathological_nodes_sta_d,
           elective_nodes_with_info = NULL) # Clean up by removing the temporary count column

  return(.data)
}

#' Evaluate if Case Has Three or More Pathological Nodes
#'
#' This internal function adds a logical column to the input data frame indicating whether a case has three or more pathological nodes. It simplifies the conditional logic by directly assigning the result of the comparison to the new column without using `ifelse`, enhancing performance and readability.
#'
#' @param .data A data frame containing the column `pathological_nodes_sta_d` which denotes the number of pathological nodes identified for each case.
#'
#' @return A data frame with an additional logical column `elective_ge_3_nodes` indicating whether each case has three or more pathological nodes.
#'
#' @noRd
elective_ge_3_nodes <- function(.data) {
  .data %>%
    mutate(elective_ge_3_nodes = pathological_nodes_sta_d >= 3)
}



#' Determine Presence of Critical Anatomical Position Nodes
#'
#' This internal function evaluates whether any nodes are located in critical anatomical positions (right common iliac, left common iliac, or para-aortic). It adds a logical column indicating the presence of such nodes for each case in the dataset.
#'
#' @param .data A data frame containing columns that start with 'node' and end with 'anatomical_position_sta_d', representing the anatomical positions of nodes.
#'
#' @return A data frame with an additional logical column `any_node_ci_pa` indicating whether any node is in a critical anatomical position.
#'
#' @noRd
elective_any_node_ci_pa <- function(.data) {

  # Define the helper function within the main function for clarity and encapsulation
  helper <- function(x) {
    x %in% c("R com iliac", "L com iliac", "Para Aortic")
  }

  # Apply the helper function across relevant columns and calculate the presence of critical anatomical position nodes
  .data <- .data %>%
    mutate(
      across(
        .cols = starts_with('node') & ends_with('anatomical_position_sta_d'),
        .fns = list(ci_pa = helper),
        .names = "{.col}.{.fn}"
      )
    ) %>%
    rowwise() %>%
    mutate(any_node_ci_pa = sum(c_across(ends_with('position_sta_d.ci_pa')), na.rm = TRUE) >= 1) %>%
    ungroup()

  return(.data)
}


#' Determine Presence of Inguinal Node
#'
#' This internal function evaluates whether any nodes are located in the inguinal region (right or left groin). It adds a logical column to the dataset indicating the presence of such nodes for each case.
#'
#' @param .data A data frame containing columns for node anatomical positions, which start with 'node' and end with 'anatomical_position_sta_d'.
#'
#' @return A data frame with an additional logical column `any_node_inguinal` indicating whether any node is in the inguinal region.
#'
#' @noRd
elective_inguinal_node <- function(.data) {
  # Define the helper function within the main function to keep it scoped and concise
  helper <- function(x) {
    x %in% c("R groin", "L groin")
  }

  # Apply the helper function across relevant columns and calculate the presence of inguinal nodes
  .data <- .data %>%
    mutate(
      across(
        .cols = starts_with('node') & ends_with('anatomical_position_sta_d'),
        .fns = list(inguinal = helper),
        .names = "{.col}.{.fn}"
      )
    ) %>%
    rowwise() %>%
    mutate(any_node_inguinal = sum(c_across(ends_with('position_sta_d.inguinal')), na.rm = TRUE) >= 1) %>%
    ungroup()

  return(.data)
}



#' Determine Small TNM Stage
#'
#' This internal function evaluates TNM stages of cases and identifies those that fall into specific 'small' categories (stages 1, 2, 3, and 5). It adds a logical column to the dataset indicating whether each case meets the criteria for having a 'small' stage.
#'
#' @param .data A data frame containing the TNM stage of cases, specified in the `tnmt_stage_sta_d` column.
#'
#' @return A data frame with an additional logical column indicating whether each case is considered to have a 'small' TNM stage according to the defined criteria.
#'
#' @noRd
elective_small_stage <- function(.data) {
  # Helper function to check if the stage is considered 'small'
  helper <- function(x) {
    as.numeric(x) %in% c(1, 2, 3, 5)
  }

  .data %>%
    mutate(
      tnmt_stage_sta_d_small = across(
        .cols = tnmt_stage_sta_d,
        .fns = list(small = helper),
        .names = "{.col}.{.fn}"
      )
    ) %>%
    # Ensure the newly created column is logical indicating 'small' stage presence
    mutate(tnmt_stage_sta_d_small = tnmt_stage_sta_d_small == 1)
}


#' Calculate Maximum Vagina Measurement
#'
#' This internal function computes the maximum measurement across all specified columns related to vagina measurements in the dataset. It adds a new column to the dataframe indicating the maximum value found for each case.
#'
#' @param .data A data frame containing multiple columns with measurements related to the vagina, identified by their suffix '_vagina_sta_d'.
#'
#' @return A data frame with an additional column `max_vagina` representing the maximum vagina measurement found across the relevant columns for each case.
#'
#' @noRd
elective_max_vagina <- function(.data) {
  # Dynamically select columns and compute the maximum across them for each row
  .data <- .data %>%
    mutate(max_vagina = pmax(!!!select(.data, ends_with('vagina_sta_d')), na.rm = TRUE))

  return(.data)
}





#' Filter Out Cases with Incomplete Information
#'
#' This internal function removes rows from the dataset that have missing (NA) values in any of the key columns specified. It ensures that only cases with complete information across critical fields are retained for further analysis.
#'
#' @param .data A data frame containing patient and tumor information, including pathological nodes, tumor dimensions, TNM stage, histopathological type, maximum vagina measurement, and MRI corpus uteri status.
#'
#' @return A data frame with rows containing missing values in any of the specified columns removed.
#'
#' @noRd
filter_incomplete_information <- function(.data) {
  .data %>%
    filter(
      !is.na(pathological_nodes_present),
      !is.na(max_tumor_dimension_sta_d),
      !is.na(tnmt_stage_sta_d),
      !is.na(histopathological_type_sta_d),
      !is.na(max_vagina),
      !is.na(mri_corpus_uteri_sta_d)
    )
}




#' Assess High-Risk Criteria Based on Pathological Nodes and Anatomical Positions
#'
#' This internal function evaluates each case in the dataset against high-risk criteria, considering the presence of pathological nodes, the number of such nodes, and whether any nodes are located in critical anatomical positions. It adds a logical column indicating whether each case is considered high risk.
#'
#' @param .data A data frame containing information on pathological nodes presence (`pathological_nodes_present`), results from `elective_ge_3_nodes` indicating if there are three or more pathological nodes, and results from `any_node_ci_pa` indicating the presence of nodes in critical anatomical positions.
#'
#' @return A data frame with an additional logical column `elective_high_risk` indicating high-risk status for each case based on the specified criteria.
#'
#' @noRd
elective_high_risk <- function(.data) {
  .data %>%
    mutate(elective_high_risk = if_else(
      ((pathological_nodes_present == 1) & elective_ge_3_nodes) | any_node_ci_pa,
      TRUE,
      FALSE
    ))
}


#' Assess Low-Risk Criteria Based on Various Factors
#'
#' This internal function evaluates each case against specified low-risk criteria, including the absence of pathological nodes, tumor dimensions, TNM stage, histopathological type, and MRI corpus uteri status. It adds a logical column to the dataset indicating whether each case is considered low risk based on these criteria.
#'
#' @param .data A data frame containing columns for pathological node presence (`pathological_nodes_present`), maximum tumor dimension (`max_tumor_dimension_sta_d`), TNM stage (`tnmt_stage_sta_d` with a `.small` indicator), histopathological type (`histopathological_type_sta_d`), and MRI corpus uteri status (`mri_corpus_uteri_sta_d`).
#'
#' @return A data frame with an additional logical column `elective_low_risk` indicating low-risk status for each case.
#'
#' @noRd
elective_low_risk <- function(.data) {
  .data %>%
    mutate(elective_low_risk = if_else(
      (pathological_nodes_present == 0) &
        (max_tumor_dimension_sta_d <= 40) &
        tnmt_stage_sta_d_small &
        (histopathological_type_sta_d == 1) &
        mri_corpus_uteri_sta_d == 0,
      TRUE,
      FALSE
    ))
}


#' Assess Inguinal Involvement Criteria
#'
#' This internal function evaluates each case in the dataset for inguinal involvement based on specific criteria: whether the max vagina measurement equals 3 or there's any inguinal node present. It adds a logical column indicating whether each case meets these inguinal involvement criteria.
#'
#' @param .data A data frame containing at least two columns: `max_vagina`, indicating the maximum vagina measurement for each case, and `any_node_inguinal`, a logical column indicating the presence of any inguinal node.
#'
#' @return A data frame with an additional logical column `elective_inguinal` indicating whether each case meets the criteria for inguinal involvement.
#'
#' @noRd
elective_inguinal <- function(.data) {
  .data %>%
    mutate(elective_inguinal = if_else(
      max_vagina == 3 | any_node_inguinal,
      TRUE,
      FALSE
    ))
}


#' Determine Elective Target Category
#'
#' Based on the assessment of high-risk and low-risk criteria, as well as the presence of inguinal nodes, this internal function assigns an elective target category to each case. The categories include various combinations of pelvic and inguinal regions, reflecting the patient's treatment targeting needs.
#'
#' @param .data A data frame containing the logical columns `elective_high_risk`, `elective_low_risk`, and `elective_inguinal` that indicate the risk assessment and presence of inguinal nodes.
#'
#' @return A data frame with an additional column `ebrt_elective_target_algorithm` indicating the assigned elective target category for each case.
#'
#' @noRd
determine_elective_target <- function(.data) {
  .data %>%
    mutate(
      ebrt_elective_target_algorithm = if_else(
        elective_high_risk,
        if_else(
          elective_inguinal,
          'Large Pelvis + Para-aortic + Inguinal',
          'Large Pelvis + Para-aortic'
        ),
        if_else(
          elective_low_risk,
          'Small Pelvis',
          if_else(
            elective_inguinal,
            'Large Pelvis + Inguinal',
            'Large Pelvis'
          )
        )
      )
    )
}



#' Recode Anatomical Positions of Nodes
#'
#' This internal function recodes numerical codes in node anatomical position columns to their corresponding descriptive names based on predefined mappings. It targets columns that start with 'node' and end with 'anatomical_position_sta_d', converting them from numeric codes to textual descriptions.
#'
#' @param df A data frame containing columns with numerical codes for node anatomical positions, identified by their prefix 'node' and suffix 'anatomical_position_sta_d'.
#'
#' @return A data frame with node anatomical position columns recoded to descriptive names.
#'
#' @noRd
recode_node_anatomical_positions <- function(df) {
  old_levels <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "12", "13", "99", "-1")
  new_levels <- c("L ext iliac", "L int iliac", "L com iliac", "R ext iliac", "R int iliac",
                  "R com iliac", "Para Aortic", "L groin", "R groin", "R parame/paracervix",
                  "L parame/paracervix", "other", NA_character_)

  df %>% mutate(across(starts_with('node') & ends_with('anatomical_position_sta_d'), function(x) {
    # Ensure x is character to handle NA_character_ properly in recoding
    x <- as.character(x)
    recode_factor_levels(x, old_levels, new_levels)
  }))
}


#' Recode Elective Target Categories to Descriptive Names
#'
#' This internal function recodes numeric elective target categories in the specified column
#' to their corresponding descriptive names based on predefined mappings. It first replaces
#' `-1` values with `NA`, then recodes the numeric levels to descriptive text, and finally
#' renames the column to indicate the selected elective target.
#'
#' @param df A data frame containing a column with numeric codes for elective target categories.
#'
#' @return A data frame with the elective target categories column recoded to descriptive names and renamed.
#'
#' @noRd
recode_elective_targets <- function(df) {
  old_levels <- c("1", "2", "3", "4", "5")
  new_levels <- c('Small Pelvis', "Large Pelvis", "Large Pelvis + Para-aortic",
                  "Large Pelvis + Inguinal", "Large Pelvis + Para-aortic + Inguinal")

  df %>%
    replace_neg_one_with_NA() %>%
    mutate(across(ebrt_itv45_elective_nodes_incl_tdvh, function(x) {
      # Ensure x is character for recoding
      x <- as.character(x)
      recode_factor_levels(x, old_levels, new_levels)
    })) %>%
    rename(ebrt_elective_target_selected = ebrt_itv45_elective_nodes_incl_tdvh)
}




#' Elective Targets Analysis
#'
#' This function processes the input data frame to analyze and determine elective targets based on various criteria including nodal information, tumor dimensions, and risk assessments. It integrates multiple internal functions to perform the analysis and finally adds the elective target analysis results to the input data frame.
#'
#' @param .data EMII dataframe
#'
#' @return A data frame with additional columns indicating the elective target analysis results, including whether the case is high risk, has inguinal involvement, and the selected elective target algorithm outcome.
#'
#' @export
#'
#' @examples
#' # Assuming `df` is your data frame with the necessary columns:
#' # result <- elective_targets(df)
emii_add_elective_targets <- function(.data){
  tmp <- .data

  tmp <- tmp %>%
    recode_node_anatomical_positions() %>%
    elective_max_vagina() %>%
    filter_incomplete_information() %>%
    node_data_available() %>%
    elective_ge_3_nodes() %>%
    elective_any_node_ci_pa() %>%
    elective_inguinal_node() %>%
    elective_small_stage() %>%
    elective_high_risk() %>%
    elective_low_risk() %>%
    elective_inguinal() %>%
    determine_elective_target() %>%
    select(embrace_id, ebrt_elective_target_algorithm, elective_high_risk, elective_inguinal, elective_low_risk, any_node_ci_pa) %>%
    mutate(across(ebrt_elective_target_algorithm, as.factor))

  .data %>%
    left_join(tmp, by = c('embrace_id')) %>%
    recode_elective_targets()
}

# library(gtsummary)
# emii <- load_embrace_ii() %>% emii_add_elective_targets()
#
# df <- emii %>%
#   select(
#     embrace_id,
#     ebrt_elective_target_algorithm,
#     ebrt_elective_target_selected
#     ) %>%
#   replace_neg_one_with_NA() %>%
#   mutate(
#     ebrt_elective_target_algorithm = factor(ebrt_elective_target_algorithm, levels = c('Small Pelvis', "Large Pelvis", "Large Pelvis + Para-aortic",
#                                                                                                 "Large Pelvis + Inguinal", "Large Pelvis + Para-aortic + Inguinal")))
#
# df %>% gtsummary::tbl_cross(row = "ebrt_elective_target_algorithm",
#                             col = "ebrt_elective_target_selected",
#                             percent = "column",
#                             label = list(
#                               ebrt_elective_target_algorithm ~ "Based on diag. inf.",
#                               ebrt_elective_target_selected ~ "Selected by center"
#                             ),
#
#
#                             ) %>%
#   gtsummary::bold_labels() %>%
#   as_gt() %>%
#   gt::tab_options(table.width = "100%")
#
#
# openxlsx::write.xlsx(x = df, file = '2025-01-13_emii_elective_targets.xlsx')
