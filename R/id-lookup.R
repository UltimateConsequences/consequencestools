utils::globalVariables(
  c(
    "event.status",
    "id_event", "id_pres",
    "presidency_name_table"
    ))

#' Get the ID of a presidency based on the name
#'
#' @param name Name of a president or presidency
#' @param pres_table A data frame containing the presidency name table
#' @param index_var The name of the column to search for the name
#'
#' @return The unique identifier for the presidency as listed in the `index_var`
#'   column of the `pres_table`. If no match is found, returns an empty string.
#' @export
#'
#' @examples
#' id_for_president("Evo Morales")
#' id_for_president("Hernán Siles zuazo")
#' id_for_president("Jeanine Áñez")
#' id_for_president("HB") # returns a warning for multiple options
id_for_president <- function(name, pres_table = presidency_name_table,
                             index_var = "id_presidency"){
  assertthat::assert_that(
    is.character(name),
    is.data.frame(pres_table),
    index_var %in% names(pres_table)
  )
  name <- trimws(name)

  # Check if the name is in any of the unique name columns
  unique_name_columns <- c("presidency", "presidency_year",
                           "presidency_year_es", "presidency_initials_num")
  matching_columns <- intersect(unique_name_columns, names(pres_table))
  pres_row <- pres_table %>%
    dplyr::filter(if_any(!!matching_columns, ~ str_equivalent(., name))) %>%
    dplyr::select(id_pres = !!sym(index_var))

  # If no matches, try a broader search
  if (nrow(pres_row) == 0) {
    pres_row <- pres_table %>%
      dplyr::filter(if_any(tidyselect::starts_with("pres"), ~ str_equivalent(., name))) %>%
      dplyr::select(id_pres = !!sym(index_var))
  }

  # If still no matches, return an empty string
  if (nrow(pres_row) == 0) {
    return("")
  }

  if (nrow(pres_row) > 1){
    if (length(unique(pres_row$id_pres))>1) {
      warning("Multiple matching presidencies. Returning the first one.")
    }
    return(pres_row$id_pres[1])
  }

  return(pres_row$id_pres)
}


#' Add an unique identifier for presidency (id_pres) column to a dataset
#'
#' @param dataset A dataframe with a column containing the name of the president,
#'   either named `presidency` or `pres_admin`
#' @param pres_table A data frame containing the presidency name table
#' @param overwrite A boolean indicating whether to overwrite an existing
#'   column.
#' @return A dataframe with a new column `id_pres` containing the unique,
#'   placed after the `presidency` or `pres_admin` column.
#'
#' @export
#' @examples
#' add_id_for_president(deaths_aug24) %>% dplyr::select(1, pres_admin, id_pres)
add_id_for_president <- function(dataset, pres_table = presidency_name_table,
                             overwrite = FALSE){
  if (!"presidency" %in% names(dataset) & !"pres_admin" %in% names(dataset)) {
    stop("The dataset does not contain a 'presidency' or 'pres_admin' column.")
  }
  if ("id_pres" %in% names(dataset) &
      overwrite == FALSE) {
    warning(paste("The dataset already contains the 'id_pres' column. ",
                  "Call add_id_for_president() with overwrite = TRUE to replace it."))
    return(dataset)
  }

  # Check if 'presidency' or 'pres_admin' exists and use it
  if ("presidency" %in% names(dataset)) {
    col_used <- "presidency"
    dataset$presidency <- trimws(dataset$presidency)
    dataset$id_pres <- vapply(dataset$presidency, function(x) id_for_president(x, pres_table),
                              FUN.VALUE = character(1))
  } else if ("pres_admin" %in% names(dataset))  {
    col_used <- "pres_admin"
    dataset$pres_admin <- trimws(dataset$pres_admin)
    dataset$id_pres <- vapply(dataset$pres_admin, function(x) id_for_president(x, pres_table),
                              FUN.VALUE = character(1))
  }

  dataset <- dataset %>%
    dplyr::relocate(id_pres, .after = {{col_used}})

  return(dataset)
}

#' Get ID of an event based on the name
#'
#' @param title Name of the event
#' @param event_table A data frame containing `event_title`
#'   and `id_event`
#'
#' @return ID of the event as listed in the `event_table`. If no match is found,
#'   returns an empty string.
#'
#' @export
#' @examples
#' id_for_event("Education strike 1987", event_status_aug24)
#' id_for_event("Huayllani ROADBLOCK", event_status_aug24)
id_for_event <- function(title, event_table = event.status){
  event_row <- event_table %>% dplyr::filter(str_equivalent(event_title, title))
  if (nrow(event_row) == 0) {
    return("")
  }

  if (nrow(event_row) > 1){
    if (length(unique(event_row$id_event))>1) {
      warning("Multiple events found with the same title. Returning the first one.")
    }
    return(event_row$id_event[1])
  }

  return(event_row$id_event)
}

#' Add an unique identifier for event (id_event) column to a dataset
#'
#' @param dataset A dataframe with a column containing the name of the event
#'   in `event_title`
#' @param event_table A data frame containing `event_title`
#'   and `id_event`
#' @param overwrite A boolean indicating whether to overwrite an existing
#'   `id_event` variable
#'
#' @returns A dataframe with a new column `id_event` containing the unique
#'   event ID, placed after the `event_title` column.
#' @export
#'
#' @examples
#' add_id_for_event(deaths_aug24, event_status_aug24) %>%
#'   dplyr::select(event_title, id_event, id_indiv)
add_id_for_event <- function(dataset, event_table = event.status,
                             overwrite = FALSE){
  if (!"event_title" %in% names(dataset)) {
    stop("The dataset does not contain the 'event_title' column.")
  }
  if ("id_event" %in% names(dataset) &
      overwrite == FALSE) {
    warning(paste("The dataset already contains the 'id_event' column. ",
                  "Call add_id_for_event() with overwrite = TRUE to replace it."))
    return(dataset)
  }

  dataset$id_event <- vapply(dataset$event_title, function(x) id_for_event(x, event_table),
                             FUN.VALUE = character(1))

  dataset <- dataset %>%
    dplyr::relocate(id_event, .after = event_title)

  return(dataset)
}

