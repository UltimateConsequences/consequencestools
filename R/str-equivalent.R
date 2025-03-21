#' Check if strings are equivalent (discarding accents and capitalization)
#'
#' @param x string
#' @param y string
#'
#' @returns Boolean TRUE/FALSE indicating if the strings are equivalent
#' @export
#'
#' @examples
#' str_equivalent("Evo Morales", "evo morales")
#' str_equivalent("Carlos\u00a0Mesa", "Carlos Mesa")
str_equivalent <- function(x, y) {
  x <- trimws(x) # trim whitespace
  y <- trimws(y)

  x <- stringr::str_replace_all(x, "\u00a0", " ") # replace non-breaking space
  y <- stringr::str_replace_all(y, "\u00a0", " ")

  x <- stringi::stri_trans_general(x, "Latin-ASCII") # eliminate accents
  y <- stringi::stri_trans_general(y, "Latin-ASCII")
  stringr::str_equal(x, y, ignore_case = TRUE)
}
