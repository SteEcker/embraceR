#' Add Number of Para Aortic Lymph Nodes per Patient
#'
#' @export
add_number_paraaortic_ln_stat_d <- function(df) {

  ln_df <- emii_get_lymph_nodes_statd(df)

  # Create the new DataFrame with one row per patient and a count of "Para Aortic" lymph nodes
  df_summary <- ln_df %>%
    group_by(embrace_id) %>%  # Group by patient ID
    summarise(
      number_paraaortic_ln_stat_d = sum(anatomical_position == "Para Aortic"),  # Count "Para Aortic" nodes
      .groups = "drop"  # Drop grouping after summarizing
    ) %>%
    mutate(number_paraaortic_ln_stat_d = as.numeric(number_paraaortic_ln_stat_d))

  return(df %>% left_join(df_summary))
}
