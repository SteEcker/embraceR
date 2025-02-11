#' Add Vital Status Events to DataFrame
#'
#' This function adds or updates a column named `vital_status_os` in the given data frame.
#' It sets the value of `vital_status_os` based on the existing value and the value of `latest_assessment_date_disease`.
#' If `vital_status_os` is NA and `latest_assessment_date_disease` is not NA, `vital_status_os` is set to 0.
#'
#' @param df A data frame that contains at least the columns `vital_status_os` and `latest_assessment_date_disease`.
#'
#' @return A data frame with the updated or new `vital_status_os` column.
#'
#' @examples
#' \dontrun{
#' # Given a DataFrame df with columns `vital_status_os` and `latest_assessment_date_disease`
#' # df <- add_vitalstatus_event(df)
#' }
#'
#' @export
add_vitalstatus_event <- function(df) {
  message('Adding vital status events')

  df <- df %>% 
    mutate(
      event_vitalstatus = case_when(
        !is.na(vital_status) ~ vital_status,
        !is.na(latest_assessment_date_disease) ~ 0,
        TRUE ~ NA_real_
      )
    )

  return(df)
}
