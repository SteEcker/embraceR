#' Add Vital Status Event Column
#'
#' Creates or updates the `event_vitalstatus` column based on existing vital status
#' and disease assessment data. Uses existing vital status when available, otherwise
#' sets to 0 if disease assessment date exists.
#'
#' @param df A data frame with `vital_status` and `latest_assessment_date_disease` columns
#'
#' @return A data frame with added or updated `event_vitalstatus` column
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   vital_status = c(1, NA, NA),
#'   latest_assessment_date_disease = c(NA, as.Date("2022-01-01"), NA)
#' )
#' result <- add_vitalstatus_event(df)
#' }
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
