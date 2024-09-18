globalVariables(c("muni_text"))
#' Produce a Summary Table that lists municipalities
#'
#' This summary table aggregates municipality and department into
#' a singular location variable `muni_text` and appends it to
#' the right side of a summary table by an arbitrary set of variables.
#'
#' @param dataframe Dataframe to be processed.
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> List of variables in tidy-select format.
#'
#' @return A table whose left columns show all of the possible value
#'     combinations for the variables being analyzed. This is followed
#'     by a `count` column with the number of deaths registered for
#'     each corresponding municipality Each event appears on its own line.
#' @export
#'
#' @examples
#' muni_list_with_counts_by(deaths_aug24, event_title)
muni_list_with_counts_by <- function(dataframe, ...){
  dataframe %>%
    dplyr::mutate(muni_text = paste(municipality, department, sep=", ")) %>%
    group_by(... , muni_text) %>%
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
#' muni_counts_by(deaths_aug24, event_title) %>% dplyr::arrange(desc(total))
#' muni_counts_by(deaths_aug24, event_title, newline_style="oneline")
#' muni_counts_by(deaths_aug24, event_title, newline_style="oneline") %>%
#'   dplyr::mutate(len=stringr::str_length(municipalities)) %>%
#'   dplyr::arrange(desc(total)) %>% print(n=25)
muni_counts_by <- function(def, ..., count_muni=FALSE, newline_style="html"){
  sep_newline <- case_when(
    newline_style == "html" ~ "; <br> ",
    newline_style == "text" ~ ";\n",
    newline_style == "markdown" ~ "; <br> ",
    newline_style == "oneline" ~ "; ",
    TRUE ~ ",\n"
  )
  count_table <- muni_list_with_counts_by(def, ...) %>%
    group_by(...) %>%
    dplyr::summarize(
      total = sum(count),
      n_muni = n(),
      municipalities = paste(muni_text, " (", count,")", sep="",
                     collapse = sep_newline),
      .groups = "rowwise"
    )
  if (!count_muni){
    count_table <- count_table[, names(count_table) != "n_muni"]
  }
  return(count_table)
}

#' Process Municipality Count Strings to First N Municipalities
#'
#' @param dataframe Dataframe to be processed.
#' @param variable Variable containing the event string
#'     (`municipalities` by default)
#' @param num_muni Number of events to be kept in string
#' @param sep_char Single-character symbol used to separate the list
#'     (`;` by default)
#'
#' @return A dataframe with the designated variable dataed.
#' @export
#'
#' @examples
#' deaths_aug24 %>% muni_counts_by(department, newline_style="text") %>%
#'   truncate_muni_list(num_muni = 3) %>% dplyr::arrange(desc(total))
truncate_muni_list <- function(dataframe, variable = "municipalities", num_muni=4, sep_char=";"){
  truncate_event_list(dataframe, variable, num_muni, sep_char)
}
