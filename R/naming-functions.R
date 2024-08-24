source(here::here("src","render-age.R"))
library(stringr)
library(dplyr)
library(incase)

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

month.name.es <- c("enero", "febrero", "marzo",
                   "abril", "mayo", "junio",
                   "julio", "agosto", "septiembre",
                   "octubre", "noviembre", "diciembre")

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
   
library(htmltools) 

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
    protest_domain_color = protest_domain.colors[protest_domain],
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
    protest_domain_color = protest_domain.colors[protest_domain],
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
                                                        "<b>Affiliación:</b> ", dec_affiliation_es, "<br>",
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
                                                       "<b>Affiliación:</b> ", dec_affiliation_es, "\n", 
                                                       state_responsibility_es, " | ", intentionality_es, "\n", 
                                                       "<b>Causa de muerte:</b> ", cause_death_es, "\n",
                                                       incident_line_es, sep="")) 
  return(dataframe)
}
# 
# de.test <- de.located %>% tail(20)
# de.test <- add_detailed_label(de.test)



