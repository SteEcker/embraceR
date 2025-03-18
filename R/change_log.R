# # Initialize the change log
# change_log <- data.frame(
#   ChangeID = integer(),
#   RecordID = integer(),
#   Field = character(),
#   OldValue = character(),
#   NewValue = character(),
#   ChangeDate = as.Date(character()),
#   Reason = character(),
#   stringsAsFactors = FALSE
# )
# openxlsx::write.xlsx(change_log, "data_raw/change_log/data_change_log.xlsx")


#' Update Record and Log Changes
#'
#' Updates a specific field value for a record and logs the change in the change log file.
#'
#' @param data A data frame containing the records to update
#' @param log A data frame containing the change log
#' @param record_id The ID of the record to update (embrace_id)
#' @param field The field name to update
#' @param new_value The new value to set
#' @param reason The reason for making the change
#'
#' @return An updated change log data frame with the new entry
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data <- data.frame(embrace_id = c("001", "002"), value = c(10, 20))
#' log <- data.frame(ChangeID = integer(), RecordID = character(),
#'                  Field = character(), OldValue = character(),
#'                  NewValue = character(), ChangeDate = as.Date(character()),
#'                  Reason = character())
#' log <- update_record(data, log, "001", "value", 15, "Correction")
#' }
update_record <- function(data, log, record_id, field, new_value, reason) {
  old_value <- as.character(data[data$embrace_id == record_id, field])

  # Update the record
  data[data$embrace_id == record_id, field] <- new_value

  # Record the change in the log
  change_entry <- data.frame(
    ChangeID = nrow(log) + 1,
    RecordID = record_id,
    Field = field,
    OldValue = old_value,
    NewValue = as.character(new_value),
    ChangeDate = Sys.Date(),
    Reason = reason,
    stringsAsFactors = FALSE
  )

  # Append the new entry to the log
  log <- rbind(log, change_entry)

  # Define the file paths using here::here()
  backup_file <- here::here("data_raw", "change_log", "data_change_log_backup.xlsx")
  old_backup_file <- here::here("data_raw", "change_log", "data_change_log_old_backup.xlsx")
  change_log_file <- here::here("data_raw", "change_log", "data_change_log.xlsx")

  # Backup the old change log
  if (file.exists(backup_file)) {
    file.copy(change_log_file, old_backup_file, overwrite = TRUE)
  }
  file.copy(change_log_file, backup_file, overwrite = TRUE)

  # Save the updated log to an xlsx file
  openxlsx::write.xlsx(log, change_log_file)

  return(log)
}


#' Apply Changes from Change Log
#'
#' Reads the change log file and applies all recorded changes to the provided data frame.
#' Automatically converts values to the appropriate data types based on the target column.
#'
#' @param data A data frame containing the master data
#'
#' @return A data frame with all changes from the change log applied
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data <- data.frame(embrace_id = c("001", "002"), value = c(10, 20))
#' updated_data <- apply_change_log(data)
#' }
apply_change_log <- function(data) {
  change_log_path <- here::here("data_raw", "change_log", "data_change_log.xlsx")
  change_log <- openxlsx::read.xlsx(change_log_path)

  changes_made <- 0

  # Check if the change log is empty
  if (nrow(change_log) == 0) {
    print("No changes to apply. The change log is empty.")
    return(data)
  }

  for (i in 1:nrow(change_log)) {
    record_id <- change_log$RecordID[i]
    field <- change_log$Field[i]
    new_value <- change_log$NewValue[i]

    # Convert the new value to the correct type based on the master dataframe column type
    new_value_converted <- type.convert(new_value, as.is = TRUE)

    if (is.numeric(data[[field]])) {
      new_value_converted <- as.numeric(new_value_converted)
    } else if (is.Date(data[[field]])) {
      new_value_converted <- as.Date(new_value_converted)
    } else if (is.factor(data[[field]])) {
      new_value_converted <- as.factor(new_value_converted)
    } else {
      new_value_converted <- as.character(new_value_converted)
    }

    # Apply the change
    if (record_id %in% data$embrace_id) {
      message(paste("Applying change for record", record_id, "field", field, "from", data[data$embrace_id == record_id, field], "to", new_value_converted))
      if (data[data$embrace_id == record_id, field] != new_value_converted) {
        data[data$embrace_id == record_id, field] <- new_value_converted
        changes_made <- changes_made + 1
      }
    }

  }


  message(paste(changes_made, "changes applied to the dataframe."))

  return(data)
}
