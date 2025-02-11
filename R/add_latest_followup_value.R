
# Unverified! Do not use yet!

# General function to get the latest value for each patient based on a base column name
add_latest_followup_value <- function(df, base_column_name) {

  message(paste("Getting latest values for", base_column_name, "..."))

  # Function to get the latest value for a given base column name and a single row
  get_latest_column_value <- function(row, base_column_name) {
    followup_id <- row["latest_followup_id"]
    col_name <- paste0(base_column_name, "_", followup_id, "m")

    print(followup_id)
    print(col_name)

    if (col_name %in% names(row)) {
      print(row[[col_name]])
      return(row[[col_name]])
    } else {
      return(NA)
    }
  }

  # Create new column for the latest value
  new_col_name <- paste0("latest_", base_column_name)
  df[[new_col_name]] <- apply(df, 1, function(row) get_latest_column_value(row, base_column_name))

  return(df)
}

