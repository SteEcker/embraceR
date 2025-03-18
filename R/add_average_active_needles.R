#' Calculate Average Number of Active Needles
#'
#' Computes the average number of active needles across all fractions for each subject.
#' Identifies columns containing fraction-specific active needle counts and calculates
#' their mean value.
#'
#' @param df A data frame containing columns with pattern "fraction_*_active_needles"
#'        that store the number of active needles for each treatment fraction
#'
#' @return The input data frame with an additional column `average_nr_active_needles`
#'         containing the rounded average. NaN values are converted to NA.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' df <- data.frame(fraction_1_active_needles = c(1, 2, 3),
#'                  fraction_2_active_needles = c(3, 4, 5),
#'                  some_other_column = c('a', 'b', 'c'))
#' average_active_needles(df)
#' }
add_average_active_needles <- function(df) {
  df %>%
    mutate(
      average_nr_active_needles = round(
        rowMeans(
          select(., starts_with("fraction") & contains("active_needles")),
          na.rm = TRUE
        )
      )
    ) %>%
    mutate(
      average_nr_active_needles = ifelse(is.nan(average_nr_active_needles), NA, average_nr_active_needles)
    )
}

