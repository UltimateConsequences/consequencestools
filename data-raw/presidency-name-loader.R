pnt.filename <- "data-raw/presidency-name-table.csv"

presidency_name_table <- vroom::vroom(pnt.filename,
                                         col_types = "cccccccccccDD")
usethis::use_data(presidency_name_table, overwrite = TRUE)
