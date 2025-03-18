#' Add Maximum Parametrial Involvement Columns
#'
#' Creates new columns containing the maximum parametrial involvement values 
#' between left and right sides for different assessment methods.
#'
#' @param data A data frame with parametrial involvement columns for left and right sides
#'
#' @return A data frame with added maximum parametrial involvement columns
#'
#' @export
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   mri_left_parametrium_sta_d = c(0, 1, 2),
#'   mri_right_parametrium_sta_d = c(1, 0, 1)
#' )
#' result <- add_parametrial_involvement(df)
#' }
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
