#' Calculate Time to Disease Event
#'
#' Determines the latest assessment date for a disease event and calculates
#' the follow-up time in months from histology assessment to this date. If 
#' no disease event is found, the follow-up time is calculated from the 
#' histology assessment to the latest assessment date. If histology assessment
#' date is missing, uses ebrt_start_date_tdvh as the baseline date. If no
#' assessment date can be found, uses last_treatment_date as the end date.
#'
#' @param df A data frame with assessment dates and disease status information
#'
#' @return A data frame with added columns: `latest_assessment_date_disease`,
#'   `timetoevent_disease`, and `latest_followup_id`
#'
#' @export
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   embrace_id = 1,
#'   assessment_date_3m = as.Date("2022-03-01"),
#'   assessment_date_6m = as.Date("2022-06-01"),
#'   histology_assesment_date = as.Date("2022-01-01"),
#'   disease_local_status_3m = 0,
#'   disease_local_status_6m = 2,
#'   disease_nodal_status_3m = 0,
#'   disease_nodal_status_6m = 0,
#'   disease_systemic_status_3m = 0,
#'   disease_systemic_status_6m = 0
#' )
#' result <- add_time_to_diseaseevent(df)
#' }
add_time_to_diseaseevent <- function(df) {
  message('Calculating maximum followup time until disease event...')
  
  # Ensure last_treatment_date exists by calling emii_add_ott if needed
  if (!"last_treatment_date" %in% names(df)) {
    df <- df %>% emii_add_ott()
  }
  
  # Define assessment_cols
  assessment_cols <- names(df)[startsWith(names(df), 'assessment_date') & endsWith(names(df), 'm')]
  
  # Create a temporary data frame with only necessary columns
  tmp_df <- df %>% 
    select(
      embrace_id,
      all_of(assessment_cols),
      histology_assesment_date,
      ebrt_start_date_tdvh,
      last_treatment_date,
      matches("disease_(local|nodal|systemic)_status_\\d+m$")
    )
  
  # Process each row to find the latest assessment date
  result_df <- tmp_df %>%
    mutate(across(all_of(assessment_cols), as.Date)) %>%
    rowwise() %>%
    mutate(
      # Find the first disease event or latest assessment date
      latest_data = list({
        latest_date <- lubridate::NA_Date_
        latest_id <- NA_character_
        found_event <- FALSE
        
        for (i in seq_along(assessment_cols)) {
          cur_date <- get(assessment_cols[i])
          timepoint <- gsub("\\D", "", assessment_cols[i])
          
          # Get disease status for current timepoint
          local <- get(paste0("disease_local_status_", timepoint, "m"))
          nodal <- get(paste0("disease_nodal_status_", timepoint, "m"))
          systemic <- get(paste0("disease_systemic_status_", timepoint, "m"))
          
          # Skip if all are NA or 9
          if (all(c(local, nodal, systemic) %in% c(NA, 9))) {
            next
          }
          
          # Check for disease event
          if (any(c(local, nodal, systemic) == 2, na.rm = TRUE)) {
            latest_date <- cur_date
            latest_id <- timepoint
            found_event <- TRUE
            break
          }
          
          # Update latest date if no event found yet
          if (!found_event && !is.na(cur_date) && 
              (is.na(latest_date) || cur_date > latest_date)) {
            latest_date <- cur_date
            latest_id <- timepoint
          }
        }
        
        list(date = latest_date, id = latest_id)
      })
    ) %>%
    ungroup() %>%
    mutate(
      latest_assessment_date_disease = map_chr(latest_data, ~as.character(.x$date)) %>% as.Date(),
      latest_followup_id = map_chr(latest_data, ~.x$id),
      # Use histology_assesment_date if available, otherwise use ebrt_start_date_tdvh
      baseline_date = if_else(
        is.na(histology_assesment_date),
        as.Date(ebrt_start_date_tdvh),
        as.Date(histology_assesment_date)
      ),
      # Use last_treatment_date if latest_assessment_date_disease is NA
      latest_assessment_date_disease = if_else(
        is.na(latest_assessment_date_disease),
        as.Date(last_treatment_date),
        latest_assessment_date_disease
      ),
      timetoevent_disease = lubridate::interval(
        baseline_date, 
        latest_assessment_date_disease
      ) %/% months(1)
    ) %>%
    select(-latest_data, -baseline_date)
  
  # Join the new columns back to the original data frame
  df %>% 
    left_join(
      result_df %>% 
        select(
          embrace_id,
          latest_assessment_date_disease,
          timetoevent_disease,
          latest_followup_id
        ),
      by = 'embrace_id'
    )
}



