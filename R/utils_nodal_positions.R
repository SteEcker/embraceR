#' Node Position Mapping Constants
#'
#' @description
#' Defines the standard mapping between numeric codes and anatomical positions
#' for lymph nodes in the EMBRACE studies.
#'
#' @export
NODAL_POSITION_MAPPING <- c(
  "1" = "L ext iliac",
  "2" = "L int iliac",
  "3" = "L com iliac",
  "4" = "R ext iliac",
  "5" = "R int iliac",
  "6" = "R com iliac",
  "7" = "Para Aortic",
  "8" = "L groin",
  "9" = "R groin",
  "12" = "R parame/paracervix",
  "13" = "L parame/paracervix",
  "99" = "other",
  "-1" = NA_character_
)

#' Get Valid Node Locations
#'
#' @description
#' Returns a vector of valid anatomical locations
#'
#' @return Character vector of valid anatomical locations
#' @export
get_valid_locations <- function() {
  unique(NODAL_POSITION_MAPPING[!is.na(NODAL_POSITION_MAPPING)])
}

