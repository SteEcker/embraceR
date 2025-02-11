#' Process and Transform Combined Data
#'
#' This function takes a combined dataset and applies transformations
#'
#' @param data A tibble or dataframe containing the combined data to be processed.
#'
#' @return A tibble or dataframe
#' @export
#'
#' @examples
#' \dontrun{
#'   processed_data <- process_combined_data(combined_data)
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



