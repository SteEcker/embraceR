#' Calculate Time to Last Vital Status
#'
#' Adds `timetoevent_vitalstatus` column measuring months from histology assessment
#' to the latest vital status date. For EMBRACE-I, uses the latest of assessment dates
#' or last info date. For EMBRACE-II, also considers date of death when available.
#'
#' @param df A data frame with histology assessment date and vital status information
#'
#' @return A data frame with added `timetoevent_vitalstatus` column
#'
#' @export
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   embrace_id = 1,
#'   study = "embrace_i",
#'   histology_assesment_date = as.Date("2020-01-01"),
#'   vital_status_last_info_date = as.Date("2022-06-01"),
#'   assessment_date_3m = as.Date("2020-04-01")
#' )
#' result <- add_time_to_last_vitalstatus(df)
#' }
add_time_to_last_vitalstatus <- function(df) {
  message('Calculating time to last vital status')
  
  # Determine columns to select based on study type
  date_cols <- c(
    'embrace_id',
    'study',
    'vital_status_last_info_date'
  )
  
  # Add death date for EMBRACE II if it exists
  if ("vital_status_date_of_death_vital_status" %in% names(df)) {
    date_cols <- c(date_cols, 'vital_status_date_of_death_vital_status')
  }
  
  # Add assessment date columns
  assessment_cols <- names(df)[startsWith(names(df), 'assessment_date') & endsWith(names(df), 'm')]
  date_cols <- c(date_cols, assessment_cols)
  
  # Calculate latest date using vectorized operations
  tmp <- df %>%
    select(all_of(date_cols)) %>%
    mutate(
      across(
        -c(embrace_id, study),
        ~as.Date(., origin = "1970-01-01")
      )
    ) %>%
    rowwise() %>%
    mutate(
      latest_vital_status_date = max(
        c_across(-c(embrace_id, study)),
        na.rm = TRUE
      ),
      # Handle case where all dates are NA
      latest_vital_status_date = if_else(
        is.infinite(latest_vital_status_date),
        lubridate::NA_Date_,
        as.Date(latest_vital_status_date, origin = "1970-01-01")
      )
    ) %>%
    ungroup() %>%
    select(embrace_id, latest_vital_status_date)
  
  # Join and calculate time interval
  df %>%
    left_join(tmp, by = "embrace_id") %>%
    mutate(
      timetoevent_vitalstatus = lubridate::interval(
        histology_assesment_date,
        latest_vital_status_date
      ) %/% months(1)
    )
}
