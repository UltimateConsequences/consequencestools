#' Filter a Dataframe to Remove Unintentional and/or Unconfirmed Deaths
#'
#' These functions filter a dataframe to remove deaths that are considered unintentional
#' or unconfirmed, based on the values in the `intentionality` and `unconfirmed` columns.
#'
#' `standard_filter()` removes both unconfirmed deaths and deaths with "Collateral",
#' "Nonconflict Accident", or "Nonconflict" values in the `intentionality` column.
#' If either the `intentionality` or `unconfirmed` column is missing, a message is
#' displayed, but the function still filters based on the available column.
#'
#' `filter_out_unintentional()` removes only deaths with "Collateral", "Nonconflict Accident",
#' or "Nonconflict" values in the `intentionality` column. If the `intentionality` column
#' is missing, a message is displayed and the original dataframe is returned.
#'
#' `filter_out_unconfirmed()` removes only unconfirmed deaths based on the `unconfirmed`
#' column. If the `unconfirmed` column is missing, a message is displayed and the original
#' dataframe is returned.
#'
#' @param dataframe A data frame containing information about deaths
#'
#' @return The input dataframe with the specified types of deaths removed
#' @export
#'
#' @examples
#' filtered_data <- standard_filter(deaths_aug24)
#' intentional_data <- filter_out_unintentional(deaths_aug24)
#' confirmed_data <- filter_out_unconfirmed(deaths_aug24)
standard_filter <- function(dataframe){
  if (!any( ("intentionality" %in% colnames(dataframe)),
            ("unconfirmed" %in% colnames(dataframe))  )){
    message("standard_filter() applied, but intentionality and unconfirmed variables were missing.")
    return(dataframe)
  }

  if (("unconfirmed" %in% colnames(dataframe))) {
    dataframe <- dataframe %>%
      dplyr::filter(is.na(unconfirmed) | unconfirmed != TRUE)
  }

  if (("intentionality" %in% colnames(dataframe))) {
    dataframe <- dataframe %>%
      dplyr::filter(is.na(intentionality) | intentionality != "Collateral") %>%
      dplyr::filter(is.na(intentionality) | intentionality != "Nonconflict Accident") %>%
      dplyr::filter(is.na(intentionality) | intentionality != "Nonconflict")
  }

  dataframe
}

#' @rdname standard_filter
#' @export
filter_out_unintentional <- function(dataframe){
  if (!("intentionality" %in% colnames(dataframe))){
    message("Intentionality variable unavailable for filtering.")
    return(dataframe)
  }

  dataframe %>%
      dplyr::filter(is.na(intentionality) | intentionality != "Collateral") %>%
      dplyr::filter(is.na(intentionality) | intentionality != "Nonconflict Accident") %>%
      dplyr::filter(is.na(intentionality) | intentionality != "Nonconflict")
}

#' @rdname standard_filter
#' @export
filter_out_unconfirmed <- function(dataframe){
  if (!("unconfirmed" %in% colnames(dataframe))){
    message("Unconfirmed variable unavailable for filtering.")
    return(dataframe)
  }

  dataframe %>%
      dplyr::filter(is.na(unconfirmed) | unconfirmed != TRUE)
}
