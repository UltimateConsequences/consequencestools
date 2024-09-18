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


estimated_date_string <- function(year, month, day){
  month_padded <- in_case(
    (is.na(month)) ~ "00",
    (!is.na(month)) ~ sprintf("%02d", month))

  day_padded <- in_case(
    (is.na(day)) ~ "00",
    (!is.na(day)) ~ sprintf("%02d", day))

  date_string <- in_case(
    (is.na(year)) ~ NA,
    (is.na(month)) ~ str_glue("{year}-06-30"),
    (is.na(day)) ~ str_glue("{year}-{month_padded}-15"),
    TRUE ~ paste(year, month_padded, day_padded, sep = "-")
  )

  return(date_string)
}
