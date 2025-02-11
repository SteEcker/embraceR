#' Add ICIS Indicator Based on Technique
#'
#' This function takes a data frame that includes an `embrace_id` column and other columns ending with "technique_tdvh"
#' and starting with "fraction". It processes these columns to determine if ICIS should be "TRUE" or "FALSE" for each `embrace_id`,
#' converting it into a numerical indicator (1 or 0) in a new column named `icis`.
#'
#' @param .data A data frame containing an `embrace_id` column and other columns that end with "technique_tdvh" and
#'             start with "fraction". These should contain values that can be transformed into factors.
#'
#' @return A data frame with an additional column `icis`, which is a numerical indicator (1 or 0) based on the
#'         conditions applied to columns ending with "technique_tdvh" and starting with "fraction".
#'
#' @examples
#' \dontrun{
#' df <- data.frame(embrace_id = c(1, 2, 3),
#'                  fraction_1_technique_tdvh = c("0", "1", "0"),
#'                  fraction_2_technique_tdvh = c("0", "1", "1"))
#' add_icis(df)
#' }
#' @export
add_icis <- function(.data) {
  df <- .data %>%
    select(embrace_id, ends_with("technique_tdvh")&starts_with("fraction")) %>%
    replace_neg_one_with_NA() %>%
    mutate(across(-embrace_id, as.factor)) %>%
    tidyr::pivot_longer(-embrace_id) %>%
    tidyr::drop_na(value) %>%
    group_by(embrace_id) %>%
    mutate(icis = forcats::as_factor(forcats::fct_match(value, "1"))) %>%
    mutate(icis = "FALSE" %in% icis) %>%
    tidyr::pivot_wider() %>%
    select(embrace_id, icis) %>%
    ungroup() %>%
    mutate(icis = if_else(icis == TRUE, 1, 0))

  .data %>% dplyr::left_join(df, by = "embrace_id")
}


#' Add parallel or oblique classification for icis
#'
#' @export
add_parallel_oblique_needles <- function(.data) {
  df <- .data %>%
    select(embrace_id, ends_with("technique_tdvh")&starts_with("fraction")) %>%
    replace_neg_one_with_NA() %>%
    mutate(across(-embrace_id, as.factor)) %>%
    tidyr::pivot_longer(-embrace_id) %>%
    tidyr::drop_na(value) %>%
    group_by(embrace_id) %>%
    summarise(
      icis_parallel_oblique = case_when(
        all(value == 1) ~ "no needles",
        any(value == 2) ~ "parallel needles",
        any(value == 3) ~ "oblique needles",
        all(value == 4) ~ "other",
        TRUE ~ "other"
      )
    )

  .data %>% dplyr::left_join(df, by = "embrace_id")
}
