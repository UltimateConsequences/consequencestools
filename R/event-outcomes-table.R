globalVariables(c("pres_admin.x", "pres_admin.y", "outcome", "outcome_summary",
                  "unique_admins"))
#' Generate a Table of Event Counts
#'
#' This function generates a table of event counts based on the provided deaths data.
#'
#' @param deaths A data frame containing filtered deaths data.
#' @param deaths_unfiltered A data frame containing unfiltered deaths data.
#' @param ... Additional arguments passed to the `count_range_by` function.
#'
#' @return A table of event counts.
#' @export
#'
#' @examples
#' deaths_aug24_filtered <- standard_filter(deaths_aug24)
#' deaths_aug24_unfiltered <- deaths_aug24
#' event_counts_table(deaths_aug24_filtered, deaths_aug24_unfiltered)
event_counts_table <- function(deaths, deaths_unfiltered,
                               ...){
  count_range_by(deaths, deaths_unfiltered, event_title, ...)
}

#' Create a Table of Event Outcomes
#'
#' This function joins relevant event status information with campaign events data
#' to create a table of event outcomes.
#'
#' @param campaign_events A data frame containing campaign events data.
#' @param event.status A data frame containing event status information.
#'
#' @return A table of event outcomes.
#' @export
#'
#' @examples
#' deaths_aug24_filtered <- standard_filter(deaths_aug24)
#' deaths_aug24_unfiltered <- deaths_aug24
#' event_counts <- event_counts_table(deaths_aug24_filtered, deaths_aug24_unfiltered)
#' event_outcomes_table(event_counts, event_status_aug24)
event_outcomes_table <- function(campaign_events, event.status){
  # select information from the event status table, then join it into the campaign/events table
  event.status_relevant <- event.status %>% select(event_title, outcome, outcome_summary)

  event_outcomes <- campaign_events %>%
    left_join(event.status_relevant, by = c("event_title"))

  return(event_outcomes)
}

#' Generate a Table of Event Descriptions
#'
#' This function generates a table of event descriptions based on the provided deaths data.
#' It assigns presidential administrations and years to each event title.
#'
#' @param deaths A data frame containing deaths data.
#'
#' @return A table of event descriptions.
#' @export
event_description_table <- function(deaths) {
  def <- deaths
  event.domain.count <- def %>%
    summarize(.by=c(event_title, protest_domain, pres_admin), n=dplyr::n())

  event.year <- def %>%
    select(event_title, year) %>%
    distinct(event_title, .keep_all = TRUE) # year assigned to the first year in the event

  # pres_admin assigned to the presidency with most of the deaths
  #
  # Deal with the anomalous cases where there are multiple administrations for
  # a single event by using this trick
  #
  # multiple_admins:
  #    event_title               unique_admins pres_admin
  # 1 Post-resignation protests             2 Interim military government
  # 2 Post-resignation protests             2 Jeanine Áñez   #

  # designated_admin_table:
  #   event_title               protest_domain    pres_admin                      n
  # 1 Post-resignation protests Partisan Politics Interim military government     9
  #
  # Find events matched to multiple presidential administrations
  multiple_admins <- event.domain.count %>%
    group_by(event_title) %>%
    summarize(unique_admins = dplyr::n_distinct(pres_admin)) %>%
    dplyr::filter(unique_admins > 1) %>%
    left_join(select(event.domain.count, event_title, pres_admin), by = "event_title")

  # Where there are
  designated_admin_table <- event.domain.count %>%
    dplyr::filter(event_title %in% multiple_admins$event_title) %>%
    arrange(dplyr::desc(n)) %>%
    distinct(event_title, protest_domain, .keep_all = TRUE)

  # Merging to perform replacement
  event_description <- def %>%
    select(event_title, protest_domain, pres_admin) %>% # assign pres_admin
    left_join(designated_admin_table, by = c("event_title", "protest_domain")) %>%
    mutate(pres_admin = dplyr::if_else(!is.na(pres_admin.y), pres_admin.y, pres_admin.x)) %>%
    select(-pres_admin.x, -pres_admin.y, -n) %>%
    unique() %>%
    left_join(event.year, by = "event_title") %>%                          # assign year
    relocate(year, .after = "event_title")

  event_description
}

