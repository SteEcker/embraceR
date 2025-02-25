#' Add Maximum Tumor Dimension Columns
#'
#' Calculates maximum tumor dimensions from MRI and GYN measurements in the dataset.
#' Creates three new columns: maximum MRI dimension, maximum GYN dimension, and overall
#' maximum tumor dimension.
#'
#' @param .data A dataframe containing MRI and GYN tumor dimension columns
#'
#' @return A dataframe with added columns: `mri_max_tumor_dimension_sta_d`,
#'   `gyn_max_tumor_dimension_sta_d`, and `max_tumor_dimension_sta_d`
#'
#' @importFrom dplyr select mutate across
#' @importFrom tidyr replace_na
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Assuming `df` is your dataframe with MRI and GYN tumor dimension columns:
#' df <- data.frame(
#'   mri_tumor_width_sta_d = c(2, Inf, 4),
#'   mri_tumor_height_sta_d = c(3, 5, NA),
#'   gyn_tumor_width_sta_d = c(NA, 6, 2),
#'   gyn_tumor_thickness_sta_d = c(1, 2, 3)
#' )
#'
#' df <- add_max_tumor_dimension(df)
#' print(df)
#' }
add_max_tumor_dimension <- function(.data) {
  # Pre-select MRI and GYN tumor columns
  mri_tumor_cols <- select(.data, starts_with('mri_tumor') & ends_with('sta_d') & !ends_with("type_sta_d"))
  gyn_tumor_cols <- select(.data, starts_with('gyn_tumor') & ends_with('sta_d'))

  # Compute max dimensions without rowwise, using vectorized operations
  .data$mri_max_tumor_dimension_sta_d <- do.call(pmax, c(mri_tumor_cols, na.rm = TRUE))
  .data$gyn_max_tumor_dimension_sta_d <- do.call(pmax, c(gyn_tumor_cols, na.rm = TRUE))

  # Assuming max_tumor_dimension needs to be recalculated from these new columns
  .data$max_tumor_dimension_sta_d <- pmax(.data$mri_max_tumor_dimension_sta_d, .data$gyn_max_tumor_dimension_sta_d, na.rm = TRUE)

  # Replace Infinite values with NA across specific columns
  .data <- .data %>%
    mutate(across(contains("max_tumor_"), embraceR::replace_infinite_with_NA))

  return(.data)
}
