#' Add ICIS Indicator Based on Technique
#'
#' Creates a binary indicator (1/0) for interstitial component of intracavitary/interstitial (ICIS) 
#' brachytherapy based on technique data.
#'
#' @param .data A data frame with embrace_id and fraction technique columns
#'
#' @return A data frame with an additional 'icis' column (1 or 0)
#' @export
#'
#' @examples
#' \dontrun{
#' df <- data.frame(embrace_id = c(1, 2, 3),
#'                  fraction_1_technique_tdvh = c("0", "1", "0"),
#'                  fraction_2_technique_tdvh = c("0", "1", "1"))
#' add_icis(df)
#' }
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


#' Add Parallel or Oblique Needle Classification
#'
#' Classifies needle arrangements as "no needles", "parallel needles", "oblique needles", or "other"
#' based on technique data.
#'
#' @param .data A data frame with embrace_id and fraction technique columns
#'
#' @return A data frame with an additional 'icis_parallel_oblique' column
#' @export
#'
#' @examples
#' \dontrun{
#' df <- data.frame(embrace_id = c(1, 2, 3),
#'                  fraction_1_technique_tdvh = c("1", "2", "3"),
#'                  fraction_2_technique_tdvh = c("1", "2", "4"))
#' add_parallel_oblique_needles(df)
#' }
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
