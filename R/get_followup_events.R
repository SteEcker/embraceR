#' Get Follow-Up Events
#'
#' This function filters patients based on specified criteria in columns that match a given basename pattern.
#'
#' #' @examples
#' \dontrun{
#' # Example dataframe
#' df <- data.frame(
#'   embrace_id = 1:5,
#'   score_3m = c(1, 0, 3, 2, 1),
#'   score_6m = c(2, 3, 1, 0, 0),
#'   score_12m = c(0, 1, 4, 2, 1)
#' )
#'
#' # Filter patients with values 1 or 2 in any of the score columns
#' filtered_patients <- get_followup_events(df, "score", c(1, 2))
#' print(filtered_patients)
#' }
#'
#' @export
get_followup_events <- function(df, basename, criteria) {
  # Create a pattern for the relevant columns
  pattern <- paste0("^", basename, ".*m$")
  dynamic_col_name <- paste0("highest_", basename, "_value")

  # Filter rows where any of the relevant columns match the criteria using rowwise()
  filtered_df <- df %>%
    filter(if_any(matches(pattern), ~ . %in% criteria)) %>%
    mutate(!!sym(dynamic_col_name) := pmax(!!!select(., matches(pattern)), na.rm = TRUE)) %>%
    select(embrace_id, matches(pattern), !!sym(dynamic_col_name))

  return(filtered_df)
}

#
# emii %>%
#   replace_neg_one_with_NA() %>%
#   get_followup_events("muscle_fracture", c(1 ,2 ,3 , 4, 5))
