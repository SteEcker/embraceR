#' Bin HRCTV Volume Values
#'
#' Categorizes HRCTV volumes into three bins: <30, 30-45, and >45.
#'
#' @param df A dataframe containing the 'fraction01hrctv_volume_tdvh' column
#' @return A dataframe with an additional 'fraction01hrctv_volume_bins' column (0, 1, or 2)
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' sample_df <- data.frame(fraction01hrctv_volume_tdvh = c(10, 35, 50, 22, 40, NA))
#' binned_df <- add_hrctv_volume_bins(sample_df)
#' }
#' @export
#'
add_hrctv_volume_bins <- function(df) {
  # Check if the input is a dataframe and if the specified column exists
  if (!is.data.frame(df)) {
    stop("Input must be a dataframe.")
  }

  if (!"fraction01hrctv_volume_tdvh" %in% names(df)) {
    stop("The specified column does not exist in the dataframe.")
  }

  # Using dplyr to mutate the new column
  df <- df %>%
    mutate(fraction01hrctv_volume_bins = case_when(
      is.na(fraction01hrctv_volume_tdvh) ~ NA_integer_, # Handle NA values
      fraction01hrctv_volume_tdvh < 30 ~ 0,
      fraction01hrctv_volume_tdvh >= 30 & fraction01hrctv_volume_tdvh <= 45 ~ 1,
      fraction01hrctv_volume_tdvh > 45 ~ 2
    ))

  return(df)
}

