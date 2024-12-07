# This file generated within the project by the following code (choosing the filename based on
# when event-status was last saved.
#
# event.status %>% select(1:3, n_name_problems:outcome_summary, has_narrative:expand_narrative, newspaper:sourcing) -> event.status.release
# saveRDS(event.status.release, "../consequencestools/data-raw/event-status-release-2024-09-15.rds")

es.filename <- "data-raw/event-status-release-2024-09-15.rds"

event_status_aug24 <- readr::read_rds(here_filename(es.filename))
usethis::use_data(event_status_aug24, overwrite = TRUE)
