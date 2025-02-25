#' @keywords internal
#' @import dplyr
guess_factor <- function(x) {
  if (readr::guess_parser(x, guess_integer = TRUE) == "integer") {
    return(as.factor(x))
  } else {
    return(x)
  }
}


#' @keywords internal
#' @import dplyr
apply_guess_factor <- function(data) {
  message('Guessing factors')
  data %>%
    dplyr::mutate(across(where(is.numeric), guess_factor))
}
