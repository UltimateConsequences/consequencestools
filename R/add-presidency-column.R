#' Add Presidency Column to Dataframe
#'
#' This function adds a column from the presidency lookup table to a dataframe
#' based on matching a specified key column.
#'
#' @param dataframe The dataframe to which the presidency column will be added
#' @param variable The column name in the dataframe to match against the lookup
#'   table
#' @param lookup_table The table containing presidency information (defaults to
#'   presidency_name_table)
#' @param dest_var The name of the new column to create
#' @param source_var The column name in the lookup table to use as values
#'   (defaults to "presidency")
#' @param .location The location to place the new column: "right" of all data
#'   (default), "replace", or "beside" the original
#'
#' @return The original dataframe with the presidency column added
#' @export
#'
#' @examples
#' pres_freq_table <- dplyr::count(assign_presidency_levels(deaths_aug24), pres_admin)
#' add_presidency_column(pres_freq_table, "pres_admin", dest_var= "presidency_surnames")
#' add_presidency_column(pres_freq_table, "pres_admin", dest_var= "presidency_surnames",
#'   .location="replace")
#' add_presidency_column(pres_freq_table, "pres_admin", dest_var= "id_presidency",
#'   .location="beside")
#' add_presidency_column(pres_freq_table, "pres_admin", dest_var= "presidency_year_es",
#'   .location="left")
#' add_presidency_column(pres_freq_table, "pres_admin", dest_var= "presidency_initials_num",
#'   .location="right")
add_presidency_column <- function(dataframe, variable,
                                  dest_var,
                                  lookup_table = presidency_name_table,
                                  source_var = "presidency",
                                  .location = "right") {
  assertthat::assert_that(variable %in% names(dataframe))
  assertthat::assert_that(source_var %in% names(lookup_table))
  assertthat::assert_that(dest_var %in% names(lookup_table))

  key_column <- lookup_table[, c(source_var)]
  dest_column <- lookup_table[, c(dest_var)]

  # Create a subset of the lookup table with just the columns we need
  match_table <- lookup_table[, c(source_var, dest_var)]

  # Join the tables
  dataframe <- dplyr::left_join(dataframe, match_table,
                                by = join_by({{variable}} == {{source_var}}))

  unique_values <- unique(lookup_table[[dest_var]])
  dataframe[[dest_var]] <- factor(dataframe[[dest_var]], levels = unique_values)

  # # assign levels
  # dataframe <- dataframe %>%
  #     mutate({{dest_var}} := factor({{dest_var}},
  #                                  levels = pull(distinct(dest_column))))

  if (.location %in% c("beside", "replace", "substitute")){
    dataframe <- dplyr::relocate(dataframe, {{dest_var}},
                                 .after = {{variable}})
    if (.location %in% c("replace", "substitute")){
      dataframe <- dataframe[ ,!names(dataframe) %in% c(variable)]
      if (.location == "substitute"){
        dataframe <- dplyr::rename(dataframe, {{variable}} := {{dest_var}})
      }
    }
  }
  if (.location == "left"){
    dataframe <- dplyr::relocate(dataframe, {{dest_var}})
  }
  return(dataframe)
}


#' Render Presidency Information
#'
#' This function retrieves specific information about a presidency from the
#' presidency lookup table based on a given presidency name.
#'
#' @param value The name of the presidency to look up.
#' @param dest_var The specific information to retrieve (e.g., "first_day",
#'   "id_presidency", "presidency_surnames", "presidency_year_es").
#' @param lookup_table The table containing presidency information (defaults to
#'   presidency_name_table).
#' @param source_var The column name in the lookup table to use as values
#'  (defaults to "presidency").
#'
#' @return The requested information about the presidency.
#' @export
#'
#' @examples
#' render_presidency("Gonzalo Sanchez de Lozada (2nd)", "first_day") # "2002-08-06"
#' render_presidency("Gonzalo Sanchez de Lozada (2nd)", "id_presidency") # "p107"
#' render_presidency("Gonzalo Sanchez de Lozada (2nd)", "presidency_surnames")
#' render_presidency("Gonzalo Sanchez de Lozada (2nd)", "presidency_year_es")
render_presidency <- function(value,
                                  dest_var,
                                  lookup_table = presidency_name_table,
                                  source_var = "presidency") {
  assertthat::assert_that(source_var %in% names(lookup_table))
  assertthat::assert_that(dest_var %in% names(lookup_table))

  key_column <- lookup_table[, c(source_var)] %>% as.vector()
  dest_column <- lookup_table[, c(dest_var)]

  index <- which(sapply(key_column, str_equivalent, y = value))
  if (length(index) == 0){
    warning("Value not found in lookup table")
    return(NA)
  }
  if (length(index) > 1){
    warning("Multiple matches found in lookup table; returning first match")
    index <- index[1]
  }

  dest_column[[index, 1]]
}
