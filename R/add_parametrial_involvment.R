#' Add new columns to a data frame that contain the maximum parametrial involvement
#'
#' This function adds several new columns to a given data frame, each containing the
#' maximum value between two existing columns related to parametrial involvement.
#'
#' @param data A data frame containing at least two sets of numeric columns representing parametrial involvement on the left and right side.
#' @return A data frame with new columns added.
#'
#' @export
add_parametrial_involvement <- function(data) {
  if (!is.data.frame(data)) {
    stop("Input should be a data frame.")
  }

  # Define a list of column pairs to get the maximum values from
  col_pairs <- list(
    c("imp1image_left_parametrium_sta_b", "imp1imaging_right_parametrium_sta_b"),
    c("imp1gyn_left_parametrium_sta_b", "imp1gyn_right_parametrium_sta_b"),
    c("mri_left_parametrium_sta_d", "mri_right_parametrium_sta_d"),
    c("gyn_left_parametrium_sta_d", "gyn_right_parametrium_sta_d")
  )

  # Define corresponding new column names
  new_col_names <- c("imp1image_max_parametrium_sta_b",
                     "imp1gyn_max_parametrium_sta_b",
                     "mri_max_parametrium_sta_d",
                     "gyn_max_parametrium_sta_d")

  # Loop over each pair of columns and add a new column with the max values
  for (i in seq_along(col_pairs)) {
    pair <- col_pairs[[i]]
    new_col <- new_col_names[i]

    if (all(pair %in% names(data))) {
      data <- data %>% mutate(!!new_col := get_max_of_two_columns(., pair[1], pair[2]))
    } else {
      warning(paste("Skipping ", new_col, " because not all columns are present", sep = ""))
    }
  }

  return(data)
}
