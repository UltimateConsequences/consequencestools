#' Relabel presidential administration using presidency_name_table
#'
#' Replaces pres_admin values with corresponding values from a specified column
#' in presidency_name_table (e.g., presidency_year, presidency_initials, etc.)
#'
#' @param dataframe The input dataframe containing a pres_admin column
#' @param output_column The column from presidency_name_table to use for new labels
#'   (unquoted column name)
#' @param .input_column The column from presidency_name_table to match against
#'   pres_admin (default "presidency")
#' @param keep_original Logical indicating whether to keep the original pres_admin
#'   column as pres_admin_original (default TRUE)
#'
#' @return The dataframe with pres_admin relabeled according to output_column
#'
#' @export
#'
#' @examples
#' def <- assign_presidency_levels(deaths_aug24)
#' def_labeled <- relabel_pres_admin(def, presidency_year)
#' def_labeled <- relabel_pres_admin(def, presidency_initials)
#' def_labeled <- relabel_pres_admin(def, presidency_fullname_es, keep_original = TRUE)
relabel_pres_admin <- function(dataframe, output_column, .input_column="presidency", keep_original = TRUE) {
  output_col_name <- rlang::ensym(output_column)
  input_col_name <- rlang::ensym(.input_column)

  result <- dataframe %>%
    left_join(select(presidency_name_table, {{input_col_name}}, {{output_column}}),
              by = join_by(pres_admin == {{input_col_name}}))

  if (keep_original) {
    result <- result %>%
      rename(pres_admin_original = pres_admin)
  } else {
    result <- result %>%
      select(-pres_admin)
  }

  result <- result %>%
    rename(pres_admin = {{output_column}})

  if(length(unique(presidency_name_table[[rlang::as_name(output_col_name)]])) < nrow(presidency_name_table)) {
    warning("Some new values are ambiguous. pres_admin not returned as a factor.")
  } else {
    result <- result %>%
      # Make pres_admin a factor in the order specified in presidency_name_table
      mutate(pres_admin = factor(pres_admin,
                                 levels = presidency_name_table[[rlang::as_name(output_col_name)]]))
  }

  return(result)
}
