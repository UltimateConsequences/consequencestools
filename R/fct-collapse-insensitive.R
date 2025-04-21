#' Collapse factors in a case-insensititve manner
#'
#' This function collapses factor levels in a case-insensitive manner,
#' acting as a wrapper around `forcats::fct_collapse()`.
#'
#' Accordingly, the capitalization of the old values of the factors
#' can be flexible, but the new levels must be specified with the
#' capitalization as desired.
#'
#' @param f A factor to collapse.
#' @param ... Named arguments where the names are the new levels and the
#'  values are vectors of old levels to be collapsed into the new level.
#'
#' @export
#'
#' @examples
#' df2 <- data.frame(
#'   id_numbers = 1:10,
#'   quality = c("yes", "LIKELY YES", "no", "Presumed yes", "likely no")
#' )
#' df2$quality <- factor(df2$quality, levels = c("yes", "LIKELY YES", "no",
#'   "Presumed yes", "likely no"))
#' df2$quality <- fct_collapse_insensitive(df2$quality,
#'                                Yes = c("Yes", "Likely Yes", "Presumed Yes"),
#'                                No = c("No", "Likely No", "Presumed No"))
fct_collapse_insensitive <- function(f, ...) {
  mapping <- list(...)

  # Convert factor to character
  f_char <- as.character(f)

  # Create a new list for case-insensitive matching
  new_mapping <- list()

  for (new_level in names(mapping)) {
    pattern <- paste0("^", paste(mapping[[new_level]], collapse = "$|^"), "$")
    matches <- stringr::str_which(f_char, stringr::regex(pattern, ignore_case = TRUE))
    new_mapping[[new_level]] <- unique(f_char[matches])
  }

  # Apply fct_collapse with the new mapping
  forcats::fct_collapse(f, !!!new_mapping)
}
