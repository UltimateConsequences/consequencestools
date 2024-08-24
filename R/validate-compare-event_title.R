# Compare event_titles between de (deaths entries)  and event.status
source(here::here("src","00-import-settings-local.R"), local = knitr::knit_global())
source(here::here("src","01-import.R"), local = knitr::knit_global())

de.events <- de %>% distinct(event_title, year)

es.events <- unique(event.status$event_title)

anti_join(data_frame(de.events), data_frame(es.events), by=join_by(de.events==es.events))

de.events.matched <- de.events %>% mutate(matched = (event_title %in% es.events))
filter(de.events.matched, matched==FALSE) 
