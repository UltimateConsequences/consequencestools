#' Sort variable description by widest appearance across groups
#'
#' Reorders the levels and colors in a variable description list based on how
#' widely each level appears across a grouping variable. Levels that appear in
#' more groups are ranked higher. For example, sorting protest domains by how
#' many presidential administrations each domain appears in.
#'
#' @param var_description A list containing variable metadata with elements:
#'   \itemize{
#'     \item \code{r_variable}: Name of the variable in the dataframe
#'     \item \code{levels}: Character vector of level names
#'     \item \code{colors}: Named vector of colors for each level
#'     \item \code{levels_es}: (Optional) Spanish translations of levels
#'     \item \code{colors_es}: (Optional) Spanish-named color vector
#'   }
#' @param def The dataframe containing the data to analyze
#' @param grouping_var The variable to group by when calculating width of
#'   appearance (e.g., pres_admin, year). Use unquoted variable name.
#'
#' @return A modified variable description list with levels and colors reordered
#'   by widest appearance. Includes a new \code{$ranked} element with the
#'   sorted levels.
#'
#' @export
#' @examples
#' \dontrun{
#' # Sort protest domains by how many presidential administrations they appear in
#' deaths <- assign_protest_domain_levels(deaths_aug24)
#' pd_sorted <- sorted_by_widest_appearance(
#'   var_description = lev$protest_domain,
#'   def = deaths,
#'   grouping_var = pres_admin
#' )
#' }
sorted_by_widest_appearance <- function(var_description, def, grouping_var) {
  var_name <- var_description$r_variable
  var_description$ranked <-
    count(def, {{grouping_var}}, !!sym(var_name)) %>%  # 2-var frequency table
    count(!!sym(var_name)) %>% # frequency table of number of
    # values of the grouping variable, with deaths in each value of the variable
    arrange(desc(n)) %>%              # ranked downward
    pull(!!sym(var_name))      # vector of the groups
  new_var_description <- var_description

  # Get the new order based on ranked
  new_order <- match(new_var_description$ranked, var_description$levels)

  # Reorder levels
  new_var_description$levels <- var_description$ranked

  # Reorder levels_es if it exists
  if (!is.null(var_description$levels_es)) {
    new_var_description$levels_es <- var_description$levels_es[new_order]
  }

  # Reorder colors
  new_var_description$colors <- var_description$colors[new_order]

  # Reorder colors_es if it exists
  if (!is.null(var_description$colors_es)) {
    new_var_description$colors_es <- var_description$colors_es[new_order]
  }

  new_var_description
}


#' Sort variable description by frequency
#'
#' Reorders the levels and colors in a variable description list based on
#' the frequency of each level in the data. More frequent levels are ranked
#' higher.
#'
#' @param var_description A list containing variable metadata with elements:
#'   \itemize{
#'     \item \code{r_variable}: Name of the variable in the dataframe
#'     \item \code{levels}: Character vector of level names
#'     \item \code{colors}: Named vector of colors for each level
#'     \item \code{levels_es}: (Optional) Spanish translations of levels
#'     \item \code{colors_es}: (Optional) Spanish-named color vector
#'   }
#' @param def The dataframe containing the data to analyze
#' @param grouping_var Optional grouping variable (not used, included for
#'   consistency with \code{sorted_by_widest_appearance})
#'
#' @return A modified variable description list with levels and colors reordered
#'   by frequency. Includes a new \code{$ranked} element with the sorted levels.
#'
#' @export
#' @examples
#' \dontrun{
#' deaths <- assign_protest_domain_levels(deaths_aug24)
#' # Sort protest domains by total frequency
#' pd_sorted <- sorted_by_most_frequent(
#'   var_description = lev$protest_domain,
#'   def = deaths
#' )
#' }
sorted_by_most_frequent <- function(var_description, def, grouping_var = NULL) {
  # Note: grouping_var parameter is not used but included for API consistency

  var_name <- var_description$r_variable
  var_description$ranked <-
    count(def, !!sym(var_name)) %>% # frequency table for the variable in question
    arrange(desc(n)) %>%              # ranked downward
    pull(!!sym(var_name))      # vector of the groups
  new_var_description <- var_description

  # Get the new order based on ranked
  new_order <- match(new_var_description$ranked, var_description$levels)

  # Reorder levels
  new_var_description$levels <- var_description$ranked

  # Reorder levels_es if it exists
  if (!is.null(var_description$levels_es)) {
    new_var_description$levels_es <- var_description$levels_es[new_order]
  }

  # Reorder colors
  new_var_description$colors <- var_description$colors[new_order]

  # Reorder colors_es if it exists
  if (!is.null(var_description$colors_es)) {
    new_var_description$colors_es <- var_description$colors_es[new_order]
  }

  new_var_description
}
