# create an event count by title that also counts the number of state perpetrator
#   and state victim deaths
#
library(lubridate)
library(stringr)
library(dplyr)
library(incase)

de_first_per_event <- function(de){
  de %>%
    distinct(event_title, .keep_all=TRUE) %>%
    mutate(date_text = estimated_date_string(year, month, day),
           date = as.Date(date_text))
}

de_earliest_per_event <- function(de){
  output <- de %>%
    mutate(date_text = estimated_date_string(year, month, day),
           date = as.Date(date_text))
  output <- output[order(output$date), ]
  output <- distinct(output, event_title, .keep_all=TRUE)
  output
}

de_latest_per_event <- function(de){
  output <- de %>%
    mutate(date_text = estimated_date_string(year, month, day),
           date = as.Date(date_text))

  output <- output[order(rev(output$date)), ]
  output <- distinct(output, event_title, .keep_all=TRUE)
  output <- output[order(output$date), ]
  output
}

diffdays <- function(date1, date2){
  difftime(as.POSIXct(date2), as.POSIXct(date1), units="days")
}

event_date_range_table <- function(dataframe){

  date_vars <- c("year", "month", "day", "date", "date_text")

  de.earliest_per_event <- de_earliest_per_event(dataframe) %>%
    select(event_title, date_vars) %>%
    rename_with(~ paste0(.x, "_first"), date_vars)

  de.latest_per_event <- de_latest_per_event(dataframe) %>%
    select(event_title, date_vars) %>%
    rename_with(~ paste0(.x, "_last"), date_vars)

  event.date_table <- left_join(de.earliest_per_event, de.latest_per_event)

  event.date_table %>%
    mutate(length_in_days = diffdays(date_first, date_last) + 1) %>%
    relocate(event_title, date_first, date_last, length_in_days)
}

add_days_between <- function(event.date_table){
  event.date_table <- event.date_table %>%
    mutate(days_since_last = diffdays(lag(date_last), date_first)) %>%
    mutate(prior_event = lag(event_title))
  if ("length_in_days" %in% names(event.date_table)){
    event.date_table <- relocate(event.date_table, event_title, date_first,
                                 date_last, length_in_days, days_since_last,
                                 prior_event)
  }

  event.date_table
}


event_responsibilty_summary_table <- function(dataframe) {
  de <- dataframe

  msg_fixfactor <- paste0("The function event_responsibility_summary_table requires factoring ",
                          "state_responsibliity.\n",
                          "Try applying assign_state_responsibility_levels(deaths_aug24, ",
                          "simplify = TRUE) first.")

  assertthat::assert_that("state_responsibility" %in% names(dataframe))
  assertthat::assert_that(class(dataframe$state_responsibility) == "factor",
                          msg =  msg_fixfactor)
  assertthat::assert_that("Perpetrator" %in% levels(dataframe$state_responsibility),
                          msg =  msg_fixfactor)

  event.responsibility.dateless <- de %>% group_by(event_title) %>%
    dplyr::summarize(
      n = n(),
      n_state_perp = sum(state_responsibility=="Perpetrator", na.rm = TRUE),
      n_state_victim = sum(state_responsibility=="Victim", na.rm = TRUE),
      n_state_separate = sum(str_detect(state_responsibility, "Separate"), na.rm = TRUE),
      .groups='drop'
    )

  de.first_per_event <- de %>%
    distinct(event_title, .keep_all=TRUE) %>%
    mutate(date_text = estimated_date_string(year, month, day),
           date = as.Date(date_text))

  # This test code will show the corrected dates.
  # de.first_per_event %>% filter(is.na(day)) %>% select(event_title, date, year, month, day)

  # merge the event table with the corresponding year(s)
  event.responsibility.dated <- event.responsibility.dateless %>%
    left_join(unique(select(de.first_per_event, event_title, date, year, protest_domain, pres_admin))) %>%
    select(event_title, date, year, everything()) %>%
    arrange(date)

  return(event.responsibility.dated) # use this result as the event responsibility table
}

er.numerical.columns <- c("n","n_state_perp","n_state_victim",
                          "n_state_separate")
