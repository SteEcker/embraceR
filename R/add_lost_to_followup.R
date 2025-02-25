#' Check if any follow-up is lost
#'
#' This function examines follow-up status columns in a data frame to identify:
#' 1. Patients lost to follow-up (indicated by a value of 2)
#' 2. Patients who withdrew consent (indicated by a value of 3)
#' EMBRACE-II study only.
#'
#' @param df A data frame or tibble containing follow-up columns with values:
#'   - 1: Normal follow-up
#'   - 2: Lost to follow-up
#'   - 3: Withdrew consent
#'   - NA: Missing data
#' @param pattern A character string specifying the pattern to identify follow-up columns. 
#'   Default is "followup_". Columns must end with "m" (e.g., followup_3m, followup_6m).
#'
#' @return A data frame with two additional logical columns:
#'   - is_lost_to_fu: TRUE if patient was lost to follow-up
#'   - withdrew_consent: TRUE if patient withdrew consent
#' @keywords internal
#'
#' @examples
#' df <- tibble::tibble(
#'   embrace_id = c("AAR2001", "VIE2001", "VIE2002"),
#'   followup_3m = c(1, 1, 1),
#'   followup_6m = c(1, 1, 1),
#'   followup_9m = c(1, 1, 1),
#'   followup_12m = c(1, -1, 1),
#'   followup_18m = c(1, NA, 1),
#'   followup_24m = c(NA, NA, -1)
#' )
#' add_lost_to_fu(df)
add_lost_to_fu <- function(df, pattern = "followup_") {
  message("Looking for lost to FU and withdrew consent patients.")

  # Subset the relevant columns
  subset_df <- df %>%
    select(embrace_id, starts_with(pattern) & ends_with("m")) %>%
    rowwise() %>%
    mutate(is_lost_to_fu = any(replace_na(c_across(starts_with(pattern)) == 2, FALSE)),
           withdrew_consent = any(replace_na(c_across(starts_with(pattern)) == 3, FALSE))) %>%
    ungroup() %>%
    select(embrace_id, is_lost_to_fu, withdrew_consent)

  # Join the result back to the original data frame
  df <- df %>%
    left_join(subset_df, by = "embrace_id")

  return(df)
}
