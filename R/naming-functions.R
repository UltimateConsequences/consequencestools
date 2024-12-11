utils::globalVariables(c("cause_death_es", "date_text_es", "dec_affiliation_es",
    "dec_fullname", "detailed_label", "detailed_label_es", "event_title_es",
    "incident_line", "incident_line_es", "intentionality_es", "name_line",
    "name_line_es", "state_responsibility_es", "styled_protest_domain",
    "styled_protest_domain_es", "month.name.es"))

#' Add Name Line to Dataframe
#'
#' @param dataframe A dataframe containing deceased person information
#' @return A dataframe with added 'dec_fullname' and 'name_line' columns
#'
#' @export
add_nameline <- function(dataframe){
  dataframe <- dataframe %>% mutate(
    dec_fullname = in_case(
      is.na(dec_firstname) & is.na(dec_surnames) ~ "Unknown",
      is.na(dec_firstname) & !is.na(dec_surnames) ~ paste0("? ", dec_surnames),
      !is.na(dec_firstname) & is.na(dec_surnames) ~ paste0(dec_firstname, " ?"),
      TRUE ~ paste0(dec_firstname, " ", dec_surnames)),
    name_line = in_case(
      is.na(dec_age) ~ paste0("<b>Name:</b> ", dec_fullname, sep=""),
      TRUE ~ paste0("<b>Name:</b> ", dec_fullname, " (",
                    render_age(dec_age), ")", sep="")))
  return(dataframe)
}

#' Add Spanish Name Line to Dataframe
#'
#' @param dataframe A dataframe containing deceased person information
#' @return A dataframe with added 'dec_fullname' and 'name_line_es' columns
#1
#' @export
add_nameline_es <- function(dataframe){
  dataframe <- dataframe %>% mutate(
    dec_fullname = in_case(
      is.na(dec_firstname) & is.na(dec_surnames) ~ "Desconocido",
      is.na(dec_firstname) & !is.na(dec_surnames) ~ paste0("? ", dec_surnames),
      !is.na(dec_firstname) & is.na(dec_surnames) ~ paste0(dec_firstname, " ?"),
      TRUE ~ paste0(dec_firstname, " ", dec_surnames)),
    name_line_es = in_case(
      is.na(dec_age) ~ paste0("<b>Nombre:</b> ", dec_fullname),
      TRUE ~ paste0("<b>Nombre:</b> ", dec_fullname, " (",
                    render_age_es(dec_age), ")")))
  dateframe <- dataframe %>% select(-dec_fullname)
  return(dataframe)
}

#' Add Date Line to Dataframe
#'
#' @param dataframe A dataframe containing incident date information
#' @return A dataframe with added 'month_name', 'date_text', and 'incident_line' columns
#'
#' @export
add_dateline <- function(dataframe){
  dataframe <- dataframe %>% mutate(month_name = month.name[month],
                                    date_text = case_when(
                                      (is.na(day) &
                                         is.na(month) & !is.na(year)) ~ str_glue("in {year}"),
                                      (is.na(day) &
                                         !is.na(month) &
                                         !is.na(year)) ~ str_glue("in {month.name[month]} {year}"),
                                      (is.na(year)) ~  "on an unknown date",
                                      # Normal date are just pasted
                                      TRUE ~ (str_glue("on {day} {month_name} {year}"))
                                    ),
                                    incident_line = str_c("Fatal incident ", date_text)
  )
  return(dataframe)
}

#' Add Spanish Date Line to Dataframe
#'
#' @param dataframe A dataframe containing incident date information
#'
#' @return A dataframe with added 'month_name_es', 'date_text_es', and 'incident_line_es' columns
#'
#' @export
add_dateline_es <- function(dataframe){
  dataframe <- dataframe %>% mutate(month_name_es = month.name.es[month],
                                    date_text_es = case_when(
                                      (is.na(day) &
                                         is.na(month) & !is.na(year)) ~ str_glue("en {year}"),
                                      (is.na(day) &
                                         !is.na(month) &
                                         !is.na(year)) ~ str_glue("en {month_name_es} de {year}"),
                                      (is.na(year)) ~  "en una fecha desconocida",
                                      # Normal date are just pasted
                                      TRUE ~ (str_glue("el {day} de {month_name_es} de {year}"))
                                    ),
                                    incident_line_es = str_c("Incidente fatal:  ", date_text_es)
  )
  return(dataframe)
}

#' Repair Name Line in Dataframe
#'
#' @param dataframe A dataframe containing deceased person information
#' @return A dataframe with a repaired 'name_line' column
#'
#' @export
repair_name_line<- function(dataframe){
  de_variables <- colnames(dataframe)

  if (("dec_firstname" %in% de_variables) & ("dec_surnames" %in% de_variables)) {
    dataframe <- add_nameline(dataframe)
  }else{
    dataframe <- dataframe %>% mutate(name_line = "Name data unavailable")
    warning("Name information missing from data table.")
  }
  return(dataframe)
}

#' Repair Spanish Name Line in Dataframe
#'
#' @param dataframe A dataframe containing deceased person information
#' @return A dataframe with a repaired 'name_line' column in Spanish
#'
#' @export
repair_name_line_es <- function(dataframe){
  de_variables <- colnames(dataframe)

  if (("dec_firstname" %in% de_variables) & ("dec_surnames" %in% de_variables)) {
    dataframe <- add_nameline_es(dataframe)
  }else{
    dataframe <- dataframe %>% mutate(name_line = "Faltan datos de nombres")
    warning("Name information missing from data table.")
  }
  return(dataframe)
}

#' Repair Incident Line in Dataframe
#'
#' @param dataframe A dataframe containing incident information
#' @return A dataframe with a repaired 'incident_line' column
#'
#' @export
repair_incident_line <- function(dataframe){
  de_variables <- colnames(dataframe)

  if ("date_text" %in% de_variables) {
    dataframe <- dataframe %>% mutate(incident_line = str_c("Fatal incident ", date_text))
  }else{
    if (("year" %in% de_variables) &
        ("month" %in% de_variables) &
        ("day" %in% de_variables)) {
      dataframe <- add_dateline(dataframe)
    }else{
      dataframe <- dataframe %>% mutate(incident_line = "")
      warning("Date information missing from data table.")
    }
  }
  return(dataframe)
}

#' Repair Spanish Incident Line in Dataframe
#'
#' @param dataframe A dataframe containing incident information
#' @return A dataframe with a repaired 'incident_line_es' column in Spanish
#'
#' @export
repair_incident_line_es <- function(dataframe){
  de_variables <- colnames(dataframe)

  if ("date_text_es" %in% de_variables) {
    dataframe <- dataframe %>% mutate(incident_line_es = str_c("Incidente fatal:  ", date_text_es))
  }else{
    if (("year" %in% de_variables) &
        ("month" %in% de_variables) &
        ("day" %in% de_variables)) {
      dataframe <- add_dateline_es(dataframe)
      dataframe <- dataframe %>% mutate(incident_line_es = str_c("Incidente fatal:  ", date_text_es))
    }else{
      dataframe <- dataframe %>% mutate(incident_line = "")
      warning("Date information missing from data table.")
    }
  }
  return(dataframe)
}


#' Add Detailed Label to Dataframe
#'
#' @param dataframe A dataframe containing event and deceased person information
#' @return A dataframe with an added 'detailed_label' column
#'
#' @export
#'
#' @details This function creates a detailed HTML-formatted label for each row in the dataframe.
#' It includes information such as event title, protest domain, deceased person's name,
#' affiliation, cause of death, incident date, and location (if available).
#'
#' The function first checks and repairs 'name_line' and 'incident_line' if they're missing.
#' It then applies color coding to the protest domain and combines all information into
#' a single detailed label.
#'
#' @note This function assumes the existence of a 'protest_domain.colors' vector for color coding.
add_detailed_label <- function(dataframe){
  de_variables <- colnames(dataframe)

  if (!("name_line" %in% de_variables)){
    # repair name_line
    dataframe <- repair_name_line_es(dataframe)
  }

  if (!("incident_line" %in% de_variables)) {
    dataframe <-  repair_incident_line(dataframe)
  }

  dataframe <- dataframe %>% rowwise() %>% mutate(
    protest_domain_color = consequencestools::protest_domain$colors[protest_domain],
    styled_protest_domain =
      str_glue("<span style=\"color:{protest_domain_color};\"><b>",
               "{protest_domain}</b></span>")
  )

  dataframe <- dataframe %>%  mutate(detailed_label=paste("<b>", event_title, "</b><br>",
                                       styled_protest_domain, "<br><br>",
#                                      protest_domain, "<br>",
                                       name_line, "<br>",
                                       "<b>Affiliation:</b> ", dec_affiliation, "<br>",
                                       sr_text, " | ", intentionality, "<br>",
                                       "<b>Cause of death:</b> ", cause_death, "<br>",
                                       incident_line, "<br>",
                                       # "<b>Location:</b> ", location,
                                       sep="")
                                     )
  if ("location" %in% de_variables){
    # include location line
    dataframe <- dataframe %>%  mutate(detailed_label=paste(detailed_label,
                                                        "<b>Location:</b> ", location,
                                                        sep=""))
  }

  return(dataframe)
}

#' Add Detailed Label in Spanish to Dataframe
#'
#' @param dataframe A dataframe containing event and deceased person information
#' @return A dataframe with an added 'detailed_label' column
#'
#' @export
#'
#' @details This function creates a detailed HTML-formatted label for each row in the dataframe.
#' It includes information such as event title, protest domain, deceased person's name,
#' affiliation, cause of death, incident date, and location (if available).
#'
#' The function first checks and repairs 'name_line' and 'incident_line' if they're missing.
#' It then applies color coding to the protest domain and combines all information into
#' a single detailed label.
#'
#' @note This function assumes the existence of a 'protest_domain.colors' vector for color coding.
add_detailed_label_es <- function(dataframe){
  de_variables <- colnames(dataframe)

  if (!("name_line_es" %in% de_variables)){
    # repair name_line
    dataframe <- repair_name_line_es(dataframe)
  }

  if (!("incident_line_es" %in% de_variables)) {
    dataframe <-  repair_incident_line_es(dataframe)
  }

  dataframe <- dataframe %>% rowwise() %>% mutate(
    protest_domain_color = consequencestools::protest_domain$colors[protest_domain],
    styled_protest_domain_es =
      str_glue("<span style=\"color:{protest_domain_color};\"><b>",
               "{protest_domain_es}</b></span>")
  )

  variables_to_print <- c("event_title_es", "name_line_es", "dec_affiliation_es",
                         "state_responsibility_es", "intentionality_es", "cause_death_es",
                         "incident_line_es")
  if (all(variables_to_print %in% de_variables)){
    dataframe <- dataframe %>%  mutate(detailed_label_es=paste("<b>", event_title_es, "</b><br>",
                                                        styled_protest_domain_es, "<br><br>",
                 #                                      protest_domain_es, "<br>",
                                                        name_line_es, "<br>",
                                                        "<b>Affiliaci\u00F3n:</b> ", dec_affiliation_es, "<br>",
                                                        "<b>Papel del Estado:</b> ", state_responsibility_es, " | ", intentionality_es, "<br>",
                                                        "<b>Causa de muerte:</b> ", cause_death_es, "<br>",
                                                        incident_line_es, "<br>",
                                                        # "<b>Location:</b> ", location,
                                                        sep=""))
  } else {
    warning("Needed variables missing.")
  }

  if ("location" %in% de_variables){
    # include location line
    dataframe <- dataframe %>%  mutate(detailed_label_es=paste(detailed_label_es,
                                                        "<b>Lugar:</b> ", location,
                                                        sep=""))
  }

  return(dataframe)
}

#' Add Simple Label to Dataframe
#'
#' @param dataframe A dataframe containing event and deceased person information
#' @return A dataframe with an added 'tooltip_text' column
#'
#' @export
#'
#' @details This function creates a simple HTML-formatted label for each row in the dataframe.
#' It includes information such as event title, deceased person's name, affiliation,
#' state responsibility, intentionality, cause of death, and incident date.
#'
#' The function first checks and repairs 'name_line' and 'incident_line' if they're missing.
add_simple_label <- function(dataframe){
  de_variables <- colnames(dataframe)

  if (!("name_line" %in% de_variables)){
    # repair name_line
    dataframe <- repair_name_line(dataframe)
  }

  if (!("incident_line" %in% de_variables)) {
    dataframe <- repair_incident_line(dataframe)
  }

  dataframe <- dataframe %>% mutate(tooltip_text=paste("<b>", event_title, "</b>\n",
                                          name_line,
                                          "<b>Affiliation:</b> ", dec_affiliation, "\n",
                                          state_responsibility, " | ", intentionality, "\n",
                                          "<b>Cause of death:</b> ", cause_death, "\n",
                                          incident_line, sep=""))
  return(dataframe)
}

#' Add Simple Label in Spanish to Dataframe
#'
#' @param dataframe A dataframe containing event and deceased person information
#' @return A dataframe with an added 'tooltip_text_es' column in Spanish
#'
#' @export
#'
#' @details This function creates a simple HTML-formatted label in Spanish for each row in the dataframe.
#' It includes information such as event title, deceased person's name, affiliation,
#' state responsibility, intentionality, cause of death, and incident date, all in Spanish.
#'
#' The function first checks and repairs 'name_line' and 'incident_line' if they're missing.
add_simple_label_es <- function(dataframe){
  de_variables <- colnames(dataframe)

  if (!("name_line" %in% de_variables)){
    # repair name_line
    dataframe <- repair_name_line(dataframe)
  }

  if (!("incident_line" %in% de_variables)) {
    dataframe <- repair_incident_line(dataframe)
  }

  dataframe <- dataframe %>% mutate(tooltip_text_es=paste("<b>", event_title_es, "</b>\n",
                                                       name_line_es,
                                                       "<b>Affiliaci\u00F3n:</b> ", dec_affiliation_es, "\n",
                                                       state_responsibility_es, " | ", intentionality_es, "\n",
                                                       "<b>Causa de muerte:</b> ", cause_death_es, "\n",
                                                       incident_line_es, sep=""))
  return(dataframe)
}

#' Add Simple Label to Dataframe
#'
#' @param dataframe A dataframe containing event and deceased person information
#' @return A dataframe with an added 'tooltip_text' column
#'
#' @export
#'
#' @details This function creates a simple HTML-formatted label for each row in the dataframe.
#' It includes information such as event title, deceased person's name, affiliation,
#' state responsibility, intentionality, cause of death, and incident date.
#'
#' The function first checks and repairs 'name_line' and 'incident_line' if they're missing.
fully_annotate <- function(dataframe){
  dataframe %>%
    add_nameline() %>%
    dplyr::mutate(age_text = render_age(dec_age)) %>%
    add_dateline() %>%
    add_simple_label()
}

#' Add Simple Label in Spanish to Dataframe
#'
#' @param dataframe A dataframe containing event and deceased person information
#' @return A dataframe with an added 'tooltip_text_es' column in Spanish
#'
#' @export
#'
#' @details This function creates a simple HTML-formatted label in Spanish for each row in the dataframe.
#' It includes information such as event title, deceased person's name, affiliation,
#' state responsibility, intentionality, cause of death, and incident date, all in Spanish.
#'
#' The function first checks and repairs 'name_line' and 'incident_line' if they're missing.
fully_annotate_es <- function(dataframe){
  # insert a check here that the active translation table includes English and Spanish
  # this requires some upgrades to transcats
  #
  dataframe %>% transcats::translated_join_vars(
    c("event_title", "dec_affiliation",
      "state_responsibility", "intentionality",
      "cause_death",
      "protest_domain")) %>%
    add_nameline_es() %>%
    add_dateline_es() %>%
    add_simple_label_es() %>%
    add_detailed_label() %>%
    add_detailed_label_es()
}
