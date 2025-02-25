#' Save Dataframe to RDS and XLSX Formats
#'
#' This function saves a given dataframe to both RDS and XLSX formats.
#' The files are saved in a folder named 'data', and the filenames include the current date.
#'
#' @param df The dataframe to be saved.
#' @keywords internal
#'
#' @examples
#' \dontrun{
#'   data <- data.frame(x = 1:5, y = 6:10)
#'   save_embrace_data(data)
#' }

save_embrace_data <- function(df) {
  # Generate filename with current date
  current_date <- Sys.Date()
  filename <- paste(current_date, "embrace_data", sep = "_")

  # Create 'data' directory if it doesn't exist
  if (!dir.exists("data")) {
    dir.create("data")
  }

  # Save to RDS
  tryCatch({
    saveRDS(df, glue::glue("data/{filename}.Rds"))
    message(glue::glue("Successfully saved {filename}.Rds"))
  }, warning = function(w) {
    message("Warning while saving RDS: ", conditionMessage(w))
  }, error = function(e) {
    message("Error while saving RDS: ", conditionMessage(e))
  })

  # Save to XLSX
  tryCatch({
    openxlsx::write.xlsx(df, file = glue::glue("data/{filename}.xlsx"), overwrite = TRUE)
    message(glue::glue("Successfully saved {filename}.xlsx"))
  }, warning = function(w) {
    message("Warning while saving XLSX: ", conditionMessage(w))
  }, error = function(e) {
    message("Error while saving XLSX: ", conditionMessage(e))
  })
}

# Example usage
# save_dataframe(em_data)
