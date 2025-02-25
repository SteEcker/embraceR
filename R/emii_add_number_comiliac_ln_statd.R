#' Count Common Iliac Lymph Nodes
#'
#' Calculates the number of common iliac lymph nodes (left and right) per patient
#' from staging data and adds this count as a new column to the dataset.
#'
#' @param df A data frame containing lymph node information with anatomical positions
#'
#' @return A data frame with added `number_common_iliac_ln_stat_d` column
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' emii_data <- load_embrace_ii()
#' result <- emii_add_number_common_iliac_ln_stat_d(emii_data)
#' }
emii_add_number_common_iliac_ln_stat_d <- function(df) {

  ln_df <- emii_get_lymph_nodes_statd(df)

  # Create the new DataFrame with one row per patient and a count of "Common Iliac" lymph nodes
  df_summary <- ln_df %>%
    group_by(embrace_id) %>%  # Group by patient ID
    summarise(
      number_common_iliac_ln_stat_d = sum(anatomical_position %in% c("L com iliac", "R com iliac")),  # Count "Common Iliac" nodes
      .groups = "drop"  # Drop grouping after summarizing
    ) %>%
    mutate(number_common_iliac_ln_stat_d = as.numeric(number_common_iliac_ln_stat_d))

  # Join the summary back to the original DataFrame
  return(df %>% left_join(df_summary, by = "embrace_id"))
}
