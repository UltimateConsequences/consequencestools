globalVariables(c("count", "total", "events"))


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
#'   dplyr::filter(count > 5) %>% dplyr::arrange(desc(count))
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
#'
#' @return A dataframe listing all possible combinations of the
#'     variables being analyzed, followed by an events column, which
#'     has a text list of names of events each followed by the number
#'     of deaths involved in parentheses.
#' @export
#'
#' @examples
#' deaths_aug24 %>% event_counts_by(protest_domain, protest_campaign) %>%
#'   dplyr::filter(total>2) %>% dplyr::arrange(desc(total)) %>% print(n=10)
#' deaths_aug24 %>% event_counts_by(protest_domain, department) %>%
#'   dplyr::filter(total > 1) %>% dplyr::arrange(department, desc(total)) %>% print(n=100)
#' deaths_aug24 %>% event_counts_by(department) %>%
#'   dplyr::arrange(desc(total)) %>% dplyr::select(department) %>%
#'   dplyr::pull()
#' deaths_aug24 %>% event_counts_by(state_perpetrator, munition) %>%
#'   dplyr::filter(state_perpetrator=="Yes") %>% dplyr::arrange(desc(total)) %>% print(n=40)
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

#' Process Event Count Strings to First N Events
#'
#' @param dataframe Dataframe to be processed.
#' @param variable Variable containing the event string
#'     (`events` by default)
#' @param num_events Number of events to be kept in string
#' @param sep_char Character that separates the listed items
#'
#' @return A dataframe with the designated variable dataed.
#' @export
#'
#' @examples
#' deaths_aug24 %>% event_counts_by(department, newline_style="text") %>%
#'   truncate_event_list(num_events = 6) %>% dplyr::arrange(desc(total))
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
