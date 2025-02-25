#' Add Overall Treatment Time (OTT) to a Data Frame
#'
#' This function calculates the overall treatment time (OTT) in days for a dataset containing
#' EBRT start and end dates, and fraction dates. The OTT is computed from the EBRT start date
#' to the last available treatment date, considering both the `ebrt_end_date_tdvh` and any fraction
#' date columns.
#'
#' @param data A data frame containing treatment data. Must include `ebrt_start_date_tdvh`,
#'   `ebrt_end_date_tdvh`, and columns representing fraction dates with names containing both
#'   "fraction" and "date".
#' @return A data frame with two additional columns:
#' \itemize{
#'   \item `last_treatment_date`: The latest treatment date for each row.
#'   \item `ott_days`: The overall treatment time in days (numeric).
#' }
#' @importFrom dplyr mutate across
#' @importFrom lubridate interval as.duration ddays
#' @keywords internal
#' @examples
#' library(dplyr)
#' library(lubridate)
#' emii <- tibble(
#'   embrace_id = c("AAR2001", "VIE2001"),
#'   ebrt_start_date_tdvh = as.POSIXct(c("2016-11-04", "2016-03-30")),
#'   ebrt_end_date_tdvh = as.POSIXct(c("2016-12-08", "2016-05-03")),
#'   fraction01date_tdvh = as.POSIXct(c("2016-12-19", "2016-05-10")),
#'   fraction02date_tdvh = as.POSIXct(c("2016-12-27", "2016-05-11"))
#' )
#' emii_add_ott(emii)
emii_add_ott <- function(data) {
  # Identify fraction columns that include 'date' in their names
  fraction_date_cols <- grep("^fraction.*date", names(data), value = TRUE)

  # Compute the last treatment date using vectorized operations
  data %>%
    mutate(
      last_treatment_date = do.call(pmax, c(across(all_of(c("ebrt_end_date_tdvh", fraction_date_cols))), na.rm = TRUE)),
      ott = as.numeric(as.duration(interval(ebrt_start_date_tdvh, last_treatment_date)) / ddays(1))
    )
}
