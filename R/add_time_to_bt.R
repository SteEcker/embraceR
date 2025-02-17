#' Add Time-to-BT and OTT-EBRT Features to Data Frame
#'
#' Adds `time_to_bt`, `ott_ebrt`, and `time_to_bt_percent` columns to a data frame based on EBRT start and end dates.
#'
#' @param x Data frame containing `embrace_id`, `ebrt_start_date_tdvh`, `ebrt_end_date_tdvh`, and `fraction01date_tdvh`.
#' @return Data frame with additional columns `time_to_bt`, `ott_ebrt`, and `time_to_bt_percent`.
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   embrace_id = c(1, 2),
#'   ebrt_start_date_tdvh = as.Date(c("2022-01-01", "2022-01-15")),
#'   ebrt_end_date_tdvh = as.Date(c("2022-01-31", "2022-02-15")),
#'   fraction01date_tdvh = as.Date(c("2022-01-05", "2022-01-18"))
#' )
#' add_time_to_bt(df)
#' }
#' @export
add_time_to_bt <- function(x){
  out <- x %>%
    select(embrace_id,
           ebrt_start_date = ebrt_start_date_tdvh,
           ebrt_end_date = ebrt_end_date_tdvh,
           fraction_01_date = fraction01date_tdvh
    ) %>%

    mutate(time_to_bt = lubridate::interval(ebrt_start_date, fraction_01_date)/lubridate::ddays(1)  ) %>%
    mutate(time_to_bt = round(time_to_bt)) %>%


    mutate(ott_ebrt = lubridate::interval(ebrt_start_date, ebrt_end_date)/lubridate::ddays(1)  ) %>%
    mutate(ott_ebrt = round(ott_ebrt))

    # filter(ott_ebrt >= 0) %>%
    # filter(ott_ebrt < 80) %>%
    # filter(time_to_bt >= 0) # safety for incorrect dates

  x %>% left_join(
    out %>%
      select(embrace_id, time_to_bt, ott_ebrt),
    by = 'embrace_id'
  ) %>%
    mutate(time_to_bt_percent = time_to_bt/ott_ebrt )
    # filter(time_to_bt_percent < 2)
}
