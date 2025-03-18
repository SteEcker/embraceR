#' Calculate Absolute TRAK Component Contribution
#'
#' Internal helper function that calculates absolute TRAK values for tandem applicator,
#' vaginal applicator, and needles for a specific fraction.
#'
#' @param df A data frame containing TRAK percentage columns
#' @param fraction_num The fraction number to process
#'
#' @return A data frame with added absolute TRAK component columns
#'
#' @export
#' @noRd
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

#' Calculate TRAK Components for All Fractions
#'
#' Internal helper function that applies absolute TRAK calculations to all fractions.
#'
#' @param df A data frame containing TRAK percentage columns
#' @param num_fractions The number of fractions to process
#'
#' @return A data frame with absolute TRAK values for all fractions
#'
#' @export
#' @noRd
trak_calculate_all_fractions <- function(df, num_fractions) {
  for (i in 1:num_fractions) {
    df <- trak_calculate_absolute_contribution(df, i)
  }
  return(df)
}

#' Calculate Total TRAK Values
#'
#' Internal helper function that sums the absolute TRAK values across all fractions
#' for each component (tandem, vaginal, needles) and the total.
#'
#' @param df A data frame with absolute TRAK component values
#' @param num_fractions The number of fractions to include in totals
#'
#' @return A data frame with added total TRAK columns
#'
#' @export
#' @noRd
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

#' Clean TRAK Data
#'
#' Internal helper function that converts zero TRAK values to NA.
#'
#' @param df A data frame with TRAK sum columns
#'
#' @return A data frame with cleaned TRAK values
#'
#' @export
#' @noRd
trak_clean_data <- function(df) {
  df <- df %>%
    mutate(trak_tandem_applicator_sum = if_else(trak_tandem_applicator_sum == 0, NA, trak_tandem_applicator_sum)) %>%
    mutate(trak_vaginal_applicator_sum = if_else(trak_vaginal_applicator_sum == 0, NA, trak_vaginal_applicator_sum)) %>%
    mutate(trak_needles_sum = if_else(trak_needles_sum == 0, NA, trak_needles_sum)) %>%
    mutate(trak_total_sum = if_else(trak_total_sum == 0, NA, trak_total_sum))


  return(df)
}

#' Calculate Absolute TRAK Values
#'
#' Calculates the absolute Total Reference Air Kerma (TRAK) values for tandem applicator,
#' vaginal applicator, and needles across all brachytherapy fractions. Converts percentage
#' contributions to absolute values in cGy at 1m and provides fraction-specific and total
#' TRAK metrics.
#'
#' @param df A data frame containing TRAK percentage columns for multiple fractions
#' @param num_fractions Number of fractions to process (default: 7)
#'
#' @return A data frame with added columns:
#'   - Fraction-specific absolute values for each component
#'   - Total sum columns: trak_tandem_applicator_sum, trak_vaginal_applicator_sum,
#'     trak_needles_sum, and trak_total_sum
#'
#' @export
#'
#' @examples
#' \dontrun{
#' emii_data <- load_embrace_ii()
#' result <- emii_add_trak_absolute(emii_data)
#' }
emii_add_trak_absolute <- function(df, num_fractions = 7) {
  df %>%
    trak_calculate_all_fractions(num_fractions) %>%
    trak_calculate_totals(num_fractions) %>%
    trak_clean_data()
}

# emii %>%
#   select(embrace_id, fraction01dose_rate_tdvh, ends_with('_sum'), fraction01hrctv_volume_tdvh) %>%
#   filter(fraction01dose_rate_tdvh == 1, trak_vaginal_applicator_sum < 2) %>%
#   select(-fraction01dose_rate_tdvh) %>%
#   pivot_longer(-c(embrace_id, fraction01hrctv_volume_tdvh)) %>%
#   # Create a named vector for better labels
#   mutate(name = factor(name,
#                        levels = c("trak_tandem_applicator_sum",
#                                   "trak_vaginal_applicator_sum",
#                                   "trak_needles_sum",
#                                   "trak_total_sum"),
#                        labels = c("Tandem",
#                                   "Vaginal",
#                                   "Needles",
#                                   "Total"))) %>%
#   ggplot(aes(x = fraction01hrctv_volume_tdvh, y = value, color = name)) +
#   geom_point(alpha = 0.2) +
#   xlim(0, 150) +
#   ylim(0, 3) +
#   geom_smooth(method = 'lm') +
#   # Add better axis labels and title
#   labs(
#     x = "CTV-HR Volume (cm3)",
#     y = "TRAK [cGy]",
#     color = "",
#     title = "Relationship Between CTV-HR Volume and total TRAK",
#     subtitle = "EMBRACE-II (HDR)"
#   ) +
#   theme_bw()

