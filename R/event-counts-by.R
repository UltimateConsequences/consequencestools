event_list_with_counts_by <- function(dataframe, ...){
  dataframe %>%
    group_by(... , event_title) %>%
    dplyr::summarize(count = n(), .groups = "rowwise") %>%
    arrange(desc(count)) %>% ungroup() 
  }

event_counts_by <- function(def, ..., newline_style="html"){
  comma_newline <- case_when(
    newline_style == "html" ~ ", <br> ",
    newline_style == "text" ~ ",\n",
    newline_style == "markdown" ~ ", <br> ",
    TRUE ~ ",\n"
  ) 
  event_list_with_counts_by(def, ...) %>%
    group_by(...) %>%
    dplyr::summarize(
      total = sum(count), 
      events = paste(event_title, " (", count,")", sep="", 
                     collapse = comma_newline),
      .groups = "rowwise"
      )
}

truncate_event_list <- function(dataframe, variable = events, num_events=4){
  dataframe <- dataframe %>% rename(events={{variable}})
  dataframe <- dataframe %>% mutate(nth_comma_position = as.numeric(unlist
                                                (str_locate_all(pattern = ",", events) %>%
                                                    purrr::map(~ .x[,1][num_events])
                                                ))) %>%
    mutate(events = case_when(
      is.na(nth_comma_position) ~ events,
      TRUE ~ paste(str_sub(events, start=1,
                           end=nth_comma_position-1), "…")
    )) %>%
    select(-nth_comma_position)
  
  dataframe %>% rename({{variable}} := events)
}
