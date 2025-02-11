#' Calculate the latest assessment date and follow-up time to the disease event
#'
#' This function processes the dataframe to find the latest assessment date before a disease event.
#' If a disease event (as defined in the corresponding columns) occurs, this date is considered the latest.
#'
#' @param df A data frame with columns that include assessment dates, histology assessment dates,
#' and disease recurrence information.
#'
#' @return A data frame with added columns for the latest assessment date and the time to the event in months.
#'
#' @export
add_time_to_diseaseevent <- function(df) {
  message('Calculating maximum followup time until disease event...')
  
  # Define assessment_cols
  assessment_cols <- names(df)[startsWith(names(df), 'assessment_date') & endsWith(names(df), 'm')]
  
  # Create a temporary data frame with only necessary columns
  tmp_df <- df %>% 
    select(
      embrace_id,
      all_of(assessment_cols),
      histology_assesment_date,
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
      timetoevent_disease = lubridate::interval(
        histology_assesment_date, 
        latest_assessment_date_disease
      ) %/% months(1)
    ) %>%
    select(-latest_data)
  
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



