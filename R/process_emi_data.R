#' Process and Transform EM-I Data
#'
#' Preprocessing for EMBRACE-I
#'
#' @param data A tibble or dataframe containing the combined data to be processed.
#'
#' @return A tibble or dataframe
#' @export
#'
#' @examples
#' \dontrun{
#'   processed_data <- process_emi_data(combined_data)
#' }
process_emi_data <- function(data){
  data %>%
    rename_with(~gsub("fraction(\\d)([a-zA-Z0-9_]+)", "fraction0\\1\\2", .)) # rename fractions

    # mutate(across(starts_with("overall_disease_systemic_recurrence_") & !contains("method"),
    #               ~ if_else(.x == 1, 2, .x)
    #               )
    #        )
}
