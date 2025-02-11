#' Replace -1 values in numeric columns with NA
#'
#' @param df The data frame containing numeric columns.
#'
#' @return A data frame where -1 values in numeric columns are replaced with NA.
#'
#' @export
replace_neg_one_with_NA <- function(df) {
  # Loop through each column to check if it's numeric
  for(col_name in names(df)) {
    # If the column is numeric, replace -1 with NA
    if(is.numeric(df[[col_name]])) {
      df[[col_name]][df[[col_name]] == -1] <- NA
    }
  }
  return(df)
}



#' Replace Infinite values in numeric columns with NA
#'
#' @param df The data frame containing numeric columns.
#'
#' @return A data frame where Infinite values in numeric columns are replaced with NA.
#'
#' @export
replace_infinite_with_NA <- function(df) {
  # Loop through each column to check if it's numeric
  for(col_name in names(df)) {
    # If the column is numeric, replace Infinite with NA
    if(is.numeric(df[[col_name]])) {
      # Replace positive and negative infinity with NA
      df[[col_name]][is.infinite(df[[col_name]])] <- NA
    }
  }
  return(df)
}
