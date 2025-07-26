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

#' Check for Equivalent Strings in a List
#'
#' This function checks if any string in a list is equivalent to a target string, 
#' considering case insensitivity, accent removal, whitespace trimming, and non-breaking space replacement.
#'
#' @param string A character string to compare.
#' @param string_list A character vector of potential matches.
#'
#' @return A logical value: \code{TRUE} if any string in the list is equivalent to the target string, 
#' or \code{FALSE} otherwise.
#'
#' @details
#' The function uses \code{\link{str_equivalent}} to determine string equivalence and applies it 
#' to each element of the list.
#'
#' @examples
#' str_equivalent_list("café", c("cafe", "tea", "coffee")) # Returns TRUE
#' str_equivalent_list("hello", c("world", "hi"))          # Returns FALSE
#'
#' @seealso \code{\link{str_equivalent}}
#' @export
str_equivalent_list <- function(string, string_list) {
  # Check if any member of string_list is equivalent to string using str_equivalent
  any(sapply(string_list, function(x) str_equivalent(string, x)))
}