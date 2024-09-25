# source(here::here("src", "update-versioned-archive.R"))
# source(here::here("src", "here_filename.R"))
#
# library(googledrive)
# library(googlesheets4)
# library(dplyr)
# library(stringr)
#
# de.raw <- import_deaths_database(incl_inactive=FALSE, incl_narrative=FALSE,
#                                  char_date_cleaning=FALSE,
#                                  parse_parenthical=FALSE)
#
# de.raw.filename <- paste0("de-raw-", Sys.Date(), ".rds")
# saveRDS(de.raw, here::here("data",de.raw.filename))

der.filename <- "data-raw/de-raw-2024-09-24.rds"

deaths_raw_sep24 <- readr::read_rds(here_filename(der.filename))
usethis::use_data(deaths_raw_sep24)
