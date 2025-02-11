#' Recode factor levels for multiple columns in a data frame
#'
#' This function takes a data frame and recodes the factor levels of multiple columns
#' based on the supplied old and new levels. It is designed to be used with dplyr's
#' mutate(across()) function.
#'
#' @param x A factor vector from a data frame column
#' @param old_levels A character vector of old factor levels
#' @param new_levels A character vector of new factor levels
#' @return The factor vector with recoded factor levels
#'
#' @export
#'
recode_factor_levels <- function(x, old_levels, new_levels) {
  if (is.factor(x) || is.character(x)) {
    recode_map <- setNames(new_levels, old_levels)
    x <- as.character(x)
    x <- dplyr::recode(x, !!!recode_map)
  }
  return(factor(x, levels = unique(c(new_levels, na.omit(x)))))
}
