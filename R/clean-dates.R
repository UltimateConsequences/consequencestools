globalVariables(c("later_date_flag"))
# clean_dates(),
# A function that processes multiple options:
# 1.It treats blanks as "unknown", but only if there is in fact a date.
# 2.It treats the first date in the text field as numerical date
# 3.It saves the whole non-numerical field as a note in a relevant variable.

clean_dates <- function(de_chardates){
  de_chardates <- de_chardates %>%
    mutate(day_notes = case_when(is.na(day) ~ "Unknown day",
                                 !str_detect(day,"^([0-9]*)$") ~ day,
                                 TRUE ~ ""
    ))
  de_chardates <- de_chardates %>%
    mutate(month_notes = case_when(is.na(month) ~ "Unknown month",
                                   !str_detect(month,"^([0-9]*)$") ~ month,
                                   TRUE ~ ""
    ))
  de_chardates <- de_chardates %>%
    mutate(year_notes = case_when(is.na(year) ~ "Unknown year",
                                  !str_detect(year,"^([0-9]*)$") ~ year,
                                  TRUE ~ ""
    ))

  de_chardates <- de_chardates %>%
    mutate(later_date_flag = !(is.na(later_day) & is.na(later_month) & is.na(later_year))) # this flag is true if there is later date

  # Diagnostic variable lets us see if that worked.
  #
  #  de_test <- de_chardates %>%
  #    dplyr::select(later_year, later_month, later_day, later_date_flag)

  de_chardates <- de_chardates %>%
    mutate(later_day_notes = case_when(!later_date_flag  ~ "", # return nothing if all three columns are blank
                                       is.na(later_day) ~ "Unknown day",
                                       !str_detect(later_day,"^([0-9]*)$") ~ day,
                                       TRUE ~ ""
    ))
  de_chardates <- de_chardates %>%
    mutate(later_month_notes = case_when(!later_date_flag ~ "", # return nothing if all three columns are blank
                                         is.na(later_month) ~ "Unknown month",
                                         !str_detect(later_month,"^([0-9]*)$") ~ month,
                                         TRUE ~ ""
    ))
  de_chardates <- de_chardates %>%
    mutate(later_year_notes = case_when(!later_date_flag ~ "", # return nothing if all three columns are blank
                                        is.na(later_year) ~ "Unknown year",
                                        !str_detect(later_year,"^([0-9]*)$") ~ year,
                                        TRUE ~ ""
    ))
  de_chardates <- de_chardates %>% dplyr::select(-"later_date_flag") # Remove temporary flag

  date_num_variables <- c("year", "month", "day", "later_year", "later_month", "later_day")

  de_chardates <- de_chardates %>%
  #  mutate_at(date_num_variables, str_extract, "\\d+")
    mutate(dplyr::across(any_of(date_num_variables), ~ stringr::str_extract(.x, pattern = "\\d+")))

  de_chardates <- de_chardates %>%
    # mutate_at(date_num_variables, as.integer)
    mutate(dplyr::across(any_of(date_num_variables), ~ as.integer(.x, pattern = "\\d+")))
  #    mutate(across(.cols = year:later_day, .fns = as.integer) )

  return(de_chardates)
}
