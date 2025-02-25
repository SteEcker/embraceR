#' Process and Transform EMBRACE-I Data
#'
#' Standardizes column naming conventions for EMBRACE-I data by reformatting 
#' fraction numbers to ensure consistent two-digit representation.
#'
#' @param data A tibble or dataframe containing the EMBRACE-I data
#'
#' @return A tibble with standardized column names
#' @keywords internal
#' @import dplyr
#'
#' @examples
#' \dontrun{
#'   emi_data <- load_embrace_i()
#'   processed_data <- process_emi_data(emi_data)
#' }
process_emi_data <- function(data){
  data %>%
    rename_with(~gsub("fraction(\\d)([a-zA-Z0-9_]+)", "fraction0\\1\\2", .)) # rename fractions
}
