#' Estimate date based on known data
#'
#' This project works with imperfect data representing the dates of lethal
#' events. But when we need to order all the events chronologically, we must
#' estimate the dates involved. These function changes known year, month, and/or
#' day variables into estimated dates, expressed either as a string by
#' estimated_date_string() or as a Date variable by estimated_date().
#'
#' When the day is unknovn, it is estimated as the 15th. When the year is
#' unknovn the estimated date is June 30. When the year is also unknovn,
#' returns NA.
#'
#' @param year Year as an integer
#' @param month Month as an integer
#' @param day Day as an integer
#'
#' @return A date in YYYY-MM-DD form, expressed as a string or Date.
#' @export
#'
#' @examples
#' estimated_date_string(2015, NA, NA)
#' estimated_date_string(2015, 8, NA)
#' (estimated_date(2015, 10, 11)
estimated_date <- function(year, month, day) {
  if (is.na(year)) return(NA)

  date_string <- estimated_date_string(year, month, day)

  as.Date(date_string)
}
#' Produce an Estimated Date String for (Sometimes Incomplete) Dates
#'
#' This function produces dates using partial information to enable
#' sequential sorting by date even when information is incomplete.
#' It treats unknown dates within a year as June 30 and unknown dates
#' within a month as the 15th day of the month.
#'
#' @param year Numerical year
#' @param month Numerical month
#' @param day Numerical day
#'
#' @return A numerical date string in "YYYY-MM-DD" format
#' @export
#' @importFrom incase in_case
#'
#' @examples
#' estimated_date_string(2015, NA, NA), "2015-06-30")
#' estimated_date_string(2015, 8, NA), "2015-08-15")
#' estimated_date_string(2015, 1, 11)
estimated_date_string <- function(year, month, day){
  month_padded <- incase::in_case(
    (is.na(month)) ~ "00",
    (!is.na(month)) ~ sprintf("%02d", month))

  day_padded <- incase::in_case(
    (is.na(day)) ~ "00",
    (!is.na(day)) ~ sprintf("%02d", day))

  date_string <- incase::in_case(
    (is.na(year)) ~ NA,
    (is.na(month)) ~ str_glue("{year}-06-30"),
    (is.na(day)) ~ str_glue("{year}-{month_padded}-15"),
    TRUE ~ paste(year, month_padded, day_padded, sep = "-")
  )

  return(date_string)
}

#' Produce an Displayed Date String for (Sometimes Incomplete) Dates
#'
#' This function produces a displayable string using partial information
#' about a date to convey to the viewer exactly how much we know about the
#' date. When even the year is unknown, it reads "Date Unknown".
#' When the day is missing, it tell us "May 2010". Otherwise
#' the date is expressed in this format: "21 March 2024".
#'
#' @param year Numerical year
#' @param month Numerical month
#' @param day Numerical day
#'
#' @return A string describing the date.
#' @export
#'
#' @examples
#' displayed_date_string(2015, NA, NA), "2015-06-30")
#' displayed_date_string(2015, 8, NA), "2015-08-15")
#' displayed_date_string(2015, 1, 11)
displayed_date_string <- function(year, month, day){
  # Unfortunate work-around to the simultaneous evaluation done by incase::in_case()
  # when using the vectors month.abb, month.name
  month_name <- ""
  month_abb <- ""
  if (!is.na(month)) {
    month_name <- month.name[month]
    month_abb <- month.abb[month]
  }

  date_string <- incase::in_case(
    (is.na(year)) ~ "Date Unknown",
    ((is.na(day) & is.na(month) & !is.na(year))) ~ stringr::str_glue("{year}"),
    (is.na(day) & !is.na(month) & !is.na(year)) ~ stringr::str_glue("{month_name} {year}"),
    TRUE ~ stringr::str_glue("{day} {month_abb} {year}")
  )

  return(date_string)
}
