de.filename <- "data-raw/deaths-entries-2024-08-12.rds"

deaths_aug24 <- readr::read_rds(here_filename(de.filename))
usethis::use_data(deaths_aug24)
