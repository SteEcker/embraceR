#' Process and Transform Combined EMBRACE Data
#'
#' Applies a series of data transformations to the combined EMBRACE dataset to create
#' derived variables needed for analysis. This function serves as a pipeline that
#' sequentially applies multiple data processing functions to:
#'
#' 1. Add ICIS (International Cervical Cancer Imaging Score) values
#' 2. Calculate average active needles used in brachytherapy
#' 3. Compute time from diagnosis to brachytherapy
#' 4. Determine parametrial involvement status
#' 5. Create HRCTV volume bins for stratification
#' 6. Calculate maximum tumor dimension
#' 7. Add patient age at diagnosis
#'
#' @param data A tibble or dataframe containing the combined EMBRACE data to be processed
#'
#' @return A tibble with additional derived columns for analysis
#' @keywords internal
#' @import dplyr
#'
#' @examples
#' \dontrun{
#'   # Load combined data
#'   combined_data <- load_combined_embrace()
#'   
#'   # Apply all transformations
#'   processed_data <- process_combined_data(combined_data)
#'   
#'   # Check new columns
#'   names(processed_data)[!names(processed_data) %in% names(combined_data)]
#' }
process_combined_data <- function(data){
  data %>%
    add_icis() %>%
    add_average_active_needles() %>%
    add_time_to_bt() %>%
    add_parametrial_involvement() %>%
    add_hrctv_volume_bins() %>%
    add_max_tumor_dimension() %>%
    add_age()
}



