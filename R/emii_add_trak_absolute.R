

# Helper function to calculate absolute contribution
trak_calculate_absolute_contribution <- function(df, fraction_num) {
  # Create the column names dynamically
  total_col <- paste0("fraction0", fraction_num, "trak_tdvh")
  tandem_pct_col <- paste0("fraction0", fraction_num, "trak_tandem_applicator_pct_tdvh")
  vaginal_pct_col <- paste0("fraction0", fraction_num, "trak_vaginal_applicator_pct_tdvh")
  needles_pct_col <- paste0("fraction0", fraction_num, "trak_needles_pct_tdvh")

  # Create new columns for absolute contributions
  df %>%
    mutate(
      !!paste0("fraction0", fraction_num, "_tandem_applicator_abs") := !!sym(total_col) * !!sym(tandem_pct_col) / 100,
      !!paste0("fraction0", fraction_num, "_vaginal_applicator_abs") := !!sym(total_col) * !!sym(vaginal_pct_col) / 100,
      !!paste0("fraction0", fraction_num, "_needles_abs") := !!sym(total_col) * !!sym(needles_pct_col) / 100
    )
}

# Apply the function to multiple fractions dynamically
trak_calculate_all_fractions <- function(df, num_fractions) {
  for (i in 1:num_fractions) {
    df <- trak_calculate_absolute_contribution(df, i)
  }
  return(df)
}

# Calculate the total sum for each component and trak over all fractions, while keeping other columns
trak_calculate_totals <- function(df, num_fractions) {
  # Generate column names for absolute values of each component across all fractions
  tandem_cols <- paste0("fraction0", 1:num_fractions, "_tandem_applicator_abs")
  vaginal_cols <- paste0("fraction0", 1:num_fractions, "_vaginal_applicator_abs")
  needles_cols <- paste0("fraction0", 1:num_fractions, "_needles_abs")
  trak_cols <- paste0("fraction0", 1:num_fractions, "trak_tdvh")

  # Add new columns with total sums, while keeping all other columns
  df <- df %>%
    mutate(
      trak_tandem_applicator_sum = rowSums(across(all_of(tandem_cols)), na.rm = TRUE),
      trak_vaginal_applicator_sum = rowSums(across(all_of(vaginal_cols)), na.rm = TRUE),
      trak_needles_sum = rowSums(across(all_of(needles_cols)), na.rm = TRUE),
      trak_total_sum = rowSums(across(all_of(trak_cols)), na.rm = TRUE)
    )

  return(df)
}

# Helper function to clean data based on conditions
trak_clean_data <- function(df) {
  df <- df %>%
    mutate(trak_tandem_applicator_sum = if_else(trak_tandem_applicator_sum == 0, NA, trak_tandem_applicator_sum)) %>%
    mutate(trak_vaginal_applicator_sum = if_else(trak_vaginal_applicator_sum == 0, NA, trak_vaginal_applicator_sum)) %>%
    mutate(trak_needles_sum = if_else(trak_needles_sum == 0, NA, trak_needles_sum)) %>%
    mutate(trak_total_sum = if_else(trak_total_sum == 0, NA, trak_total_sum))


  return(df)
}


#' Add Trak Absolute Values and Clean Data
#'
#' This function calculates the absolute contributions for `tandem_applicator`, `vaginal_applicator`, `needles`,
#' and `trak` totals across multiple fractions
#'
#' @param df A dataframe containing the trak and percentage columns for multiple fractions.
#' @param num_fractions An integer specifying the number of fractions to process. Default is 7.
#'
#' @details
#' The function works in three main steps:
#' 1. **trak_calculate_all_fractions()**: Calculates absolute contributions for all components (tandem, vaginal, needles) for each fraction.
#' 2. **trak_calculate_totals()**: Calculates total sums for all fractions.
#' 3. **trak_clean_data()**: Cleans the data, setting the sum columns to `NA` if volume is greater than 150 or any trak component is zero.
#'
#' @return The dataframe with the new absolute values for each trak component, cleaned based on the given rules.
#'
#'
#' @export
emii_add_trak_absolute <- function(df, num_fractions = 7) {
  df %>%
    trak_calculate_all_fractions(num_fractions) %>%
    trak_calculate_totals(num_fractions) %>%
    trak_clean_data()
}



