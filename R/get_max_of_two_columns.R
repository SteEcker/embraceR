#' Get the maximum of two numeric columns for each observation.
#'
#' This function computes the maximum of two numeric columns for each observation.
#' NA values are handled such that if one value is NA and the other is not, the non-NA value is returned.
#' If both values are NA, NA is returned.
#'
#' @param df A data frame with at least two numeric columns.
#' @param col1 The name of the first numeric column.
#' @param col2 The name of the second numeric column.
#'
#' @return A numeric vector containing the maximum value for each observation.
#'
#' @export
get_max_of_two_columns <- function(df, col1, col2) {
  if (!is.data.frame(df)) {
    stop("Input should be a data frame.")
  }

  if (!all(c(col1, col2) %in% names(df))) {
    stop("Both column names should be present in the data frame.")
  }

  max_values <- pmax(df[[col1]], df[[col2]], na.rm = TRUE)

  return(max_values)
}
