#' Calculate the Average Number of Active Needles for Each Row in a Data Frame
#'
#' This function takes a data frame that contains columns starting with "fraction" and containing "active_needles".
#' It calculates the average number of active needles for each row, rounds it, and adds this as a new column to the data frame.
#' If the average is NaN, it is replaced with NA.
#'
#' @param df A data frame containing columns that start with "fraction" and contain "active_needles".
#'           These columns should contain numerical values.
#'
#' @return A data frame with an additional column `average_nr_active_needles` that contains the rounded average
#'         number of active needles for each row. If the average was NaN, it is replaced with NA.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(fraction_1_active_needles = c(1, 2, 3),
#'                  fraction_2_active_needles = c(3, 4, 5),
#'                  some_other_column = c('a', 'b', 'c'))
#' average_active_needles(df)
#' }
#' @export
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

