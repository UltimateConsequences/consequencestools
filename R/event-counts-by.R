globalVariables(c("count", "total", "events", "value_string"))


#' Produce Table of Events by a Custom Set of Variables
#'
#' @param dataframe Dataframe to be processed.
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> List of variables in tidy-select format.
#'
#' @return A table whose left columns show all of the possible value
#'     combinations for the variables being analyzed. This is followed
#'     by a `count` column with the number of deaths registered for
#'     each corresponding event. Each event appears on its own line.
#' @export
#'
#' @examples
#' deaths_aug24 %>% event_list_with_counts_by(protest_domain, protest_campaign) %>%
#'   dplyr::filter(count > 5) %>% dplyr::arrange(dplyr::desc(count))
event_list_with_counts_by <- function(dataframe, ...){
  dataframe %>%
    group_by(... , event_title) %>%
    dplyr::summarize(count = dplyr::n(), .groups = "rowwise") %>%
    dplyr::arrange(dplyr::desc(count)) %>% ungroup()
  }

#' Produce Table Summarized by a Custom Set of Variables, Each with an Event List
#'
#' @param def Dataframe to be processed.
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> List of variables in tidy-select format.
#' @param newline_style Text format for separating the event lists
#' @param count_events A boolean flag. If true return `n_events` a count of the number
#'   of events that match.
#'
#' @return A dataframe listing all possible combinations of the
#'     variables being analyzed, followed by an events column, which
#'     has a text list of names of events each followed by the number
#'     of deaths involved in parentheses.
#' @export
#'
#' @examples
#' deaths_aug24 %>% event_counts_by(protest_domain, protest_campaign) %>%
#'   dplyr::filter(total>2) %>% dplyr::arrange(dplyr::desc(total)) %>% print(n=10)
#' deaths_aug24 %>% event_counts_by(protest_domain, department) %>%
#'   dplyr::filter(total > 1) %>% dplyr::arrange(department, dplyr::desc(total)) %>% print(n=100)
#' deaths_aug24 %>% event_counts_by(department) %>%
#'   dplyr::arrange(dplyr::desc(total)) %>% dplyr::select(department) %>%
#'   dplyr::pull()
#' deaths_aug24 %>% event_counts_by(state_perpetrator, munition) %>%
#'   dplyr::filter(state_perpetrator=="Yes") %>% dplyr::arrange(dplyr::desc(total)) %>% print(n=40)
event_counts_by <- function(def, ..., newline_style="html", count_events=FALSE){
  comma_newline <- case_when(
    newline_style == "html" ~ ", <br>",
    newline_style == "text" ~ ",\n",
    newline_style == "markdown" ~ ", <br>",
    TRUE ~ ",\n"
  )
  count_table <- event_list_with_counts_by(def, ...) %>%
    group_by(...) %>%
    dplyr::summarize(
      total = sum(count),
      n_events = dplyr::n(),
      events = paste(event_title, " (", count,")", sep="",
                     collapse = comma_newline),
      .groups = "rowwise"
      )
  if (!count_events){
    count_table <- count_table[, names(count_table) != "n_events"]
  }
  return(count_table)
}

#' Process Event Count Strings to First N Events
#'
#' @param dataframe Dataframe to be processed.
#' @param variable Variable containing the event string
#'     (`events` by default)
#' @param num_events Number of events to be kept in string
#' @param sep_char Character that separates the listed items
#'
#' @return A dataframe with the designated variable truncated.
#' @export
#'
#' @examples
#' deaths_aug24 %>% event_counts_by(department, newline_style="text") %>%
#'   truncate_event_list(num_events = 6) %>% dplyr::arrange(dplyr::desc(total))
truncate_event_list <- function(dataframe, variable = "events", num_events=4, sep_char=","){
  if  (!({{variable}} %in% colnames(dataframe))){
    warning(str_glue("Variable \"{variable}\" is not available for editing."))
    return(dataframe)
  }

  ellipsis_character <- "\u2026"
  space_and_ellipsis <- str_c(" ", ellipsis_character)

  dataframe <- dataframe %>% dplyr::rename(events={{variable}})
  dataframe <- dataframe %>% dplyr::mutate(nth_comma_position = as.numeric(unlist
                                                (str_locate_all(pattern = sep_char,
                                                                events) %>%
                                                    purrr::map(~ .x[,1][num_events])
                                                ))) %>%
    dplyr::mutate(events = case_when(
      is.na(nth_comma_position) ~ events,
      TRUE ~ paste(str_sub(events, start=1,
                           end=nth_comma_position), space_and_ellipsis)
    )) %>%
    dplyr::select(-"nth_comma_position")

  dataframe %>% dplyr::rename({{variable}} := events)
}



#' Get Top Values as a String
#'
#' @param freq_table A data frame containing frequency data
#' @param n_values Integer. Number of top values to return. Default is 3.
#' @param incl_counts Logical. Whether to include counts in the output string. Default is FALSE.
#' @param by Name of the frequency column
#'
#' @return A string containing the top values, separated by commas
#'
#' @import dplyr
#'
#' @examples
#' freq_table <- data.frame(protest_domain = c("A", "B", "C"), total = c(10, 5, 3))
#' top_values_string(freq_table)
#' top_values_string(freq_table, n_values = 2, incl_counts = TRUE)
#' evco <- event_counts_by(deaths_aug24, protest_domain, count_events=TRUE)
#' top_values_string(evco, 5, incl_counts = TRUE, by="n_events")
#'
#' @export
top_values_string <- function(freq_table, n_values=3, incl_counts = FALSE, by = "total") {
  value_column <- names(freq_table)[1]
  freq_table <- freq_table %>%
    arrange(desc(!!sym(by))) %>%
    rowwise() %>%
    mutate(
      value_string = if_else(
        incl_counts,
        paste0(!!sym(value_column), " (", !!sym(by), ")"),
        !!sym(value_column)
      )
    )

  result <- freq_table %>% head(n = n_values) %>%
    pull(value_string) %>%
    paste(collapse = ", ")

  return(result)
}
