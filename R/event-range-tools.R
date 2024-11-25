#' Count Ongoing Events on a Specific Date
#'
#' @param event_date_range_table A table of date ranges of events in the
#'   format produced by event_date_range_table()
#' @param input_date A Date object specifying the date to check for ongoing events
#'
#' @return An integer representing the number of events ongoing on the input date
#'
#' @examples
#' event_range_table <- event_date_range_table(deaths_aug24)
#' num_events <- event_range_table %>% count_ongoing_events(as.Date("1988-06-27"))
#' print(paste("Number of ongoing events:", num_events))
#'
#' @export
count_ongoing_events <- function(event_date_range_table, input_date) {
  ongoing_events <- event_date_range_table %>%
    dplyr::filter(date_first <= input_date & date_last >= input_date)

  return(nrow(ongoing_events))
}

#' Count Events Overlapping with a Specific Month
#'
#' @param event_date_range_table A table of date ranges of events in the
#'   format produced by event_date_range_table()
#' @param yearmonth A yearmon object specifying the month to check for overlapping events
#'
#' @return An integer representing the number of events overlapping with the specified month
#'
#' @examples
#' event_range_table <- event_date_range_table(deaths_aug24)
#' num_events <- count_events_in_month(event_range_table, zoo::as.yearmon("2003-01"))
#' print(paste("Number of overlapping events:", num_events))
#'
#' @export
count_events_in_month <- function(event_date_range_table, yearmonth) {
  # Convert yearmonth to start and end dates of the month
  start_date <- zoo::as.Date(yearmonth)
  end_date <- zoo::as.Date(yearmonth) + months(1) - lubridate::days(1)

  # Filter events overlapping with the month
  overlapping_events <- event_date_range_table %>%
    dplyr::filter(date_first <= end_date & date_last >= start_date)

  return(nrow(overlapping_events))
}
