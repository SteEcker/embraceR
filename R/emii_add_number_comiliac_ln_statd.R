#' Add Number of Common Iliac Lymph Nodes per Patient
#'
#' @export
emii_add_number_paraaortic_ln_statd <- function(df) {

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
