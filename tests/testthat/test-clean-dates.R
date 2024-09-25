# Note that these tests are currently non-interactive because the function
# has been run in the construction of deaths_aug24

test_that("clean_dates(): Notes are functional", {
  date_notes <- deaths_aug24 %>% dplyr::mutate(n =
                                   stringr::str_length(stringr::str_c(day_notes, month_notes, year_notes))) %>%
    dplyr::select(day_notes, month_notes, year_notes, n) %>%
    dplyr::arrange(dplyr::desc(n)) %>%
    dplyr::filter(n > 0)
  expect_equal(sum(is.na(deaths_aug24$day)), sum(date_notes$day_notes == "Unknown day"))
  expect_equal(sum(is.na(deaths_aug24$month)), sum(date_notes$month_notes == "Unknown month"))
  expect_equal(sum(is.na(deaths_aug24$year)), sum(date_notes$year_notes == "Unknown year"))
  expect_snapshot(date_notes)
  rm(date_notes)
})

# test_that("clean_dates(): Notes are functional", {
#   date_notes <- deaths_aug24 %>% dplyr::mutate(n =
#                                                  stringr::str_length(stringr::str_c(later_day_notes,
#                                                                            later_month_notes,
#                                                                            later_year_notes))) %>%
#     dplyr::select(later_day_notes, later_month_notes, later_year_notes, n) %>%
#     arrange(dplyr::desc(n)) %>%
#     dplyr::filter(n > 0)
#   later_deaths_aug24 <- deaths_aug24 %>% dplyr::filter(stringr::str_length(stringr::str_c(later_day,
#                                                                                 later_month,
#                                                                                 later_year)) > 0)
#   # These tests would make sense if we could access the imported columns of day, month, year
#   # expect_equal(sum(is.na(later_deaths_aug24$day)), sum(date_notes$later_day_notes == "Unknown day"))
#   # expect_equal(sum(is.na(later_deaths_aug24$month)), sum(date_notes$later_month_notes == "Unknown month"))
#   # expect_equal(sum(is.na(later_deaths_aug24$year)), sum(date_notes$later_year_notes == "Unknown year"))
#   # expect_snapshot(date_notes)
#   rm(date_notes)
# })

test_that("clean_dates(): Correct by snapshot test", {
  de.raw_datenotes <- structure(list(event_title = c("TIPNIS landowner conflict", "Sucre assassination of MIR leader",
                                                   "UMOPAR 1992", "Laymi-Qaqachaka 1995", "Laymi-Qaqachaka 1995",
                                                   "Laymi-Qaqachaka 1995", "Chayanta mining strike", "Cocalero Death Denouncement",
                                                   "Laymi-Qaqachaka 1997", "Laymi-Qaqachaka 1997", "Laymi-Qaqachaka 1997",
                                                   "May 1997 coca clashes", "Santa Cruz land dispute", "Santa Cruz land dispute",
                                                   "Cocalero kidnapping 2000", "Cocalero kidnapping 2000", "Cocalero kidnapping 2000",
                                                   "Cocalero kidnapping 2000", "CSUTCB mobilization Sep 2000", "Loma Alta coca militarization",
                                                   "Sacaba-Chapare coca market conflict", "Gas War 2003", "Gas War 2003",
                                                   "Gas War 2003", "Huanuni clashes", "Huanuni clashes", "El Alto MPS politician assassinated",
                                                   "Inquisivi Councilwoman Assassinated", "Medical Strike", "La Paz pro-Evo election March",
                                                   "La Paz post-resignation violence"),
                                   id_indiv = c("i10012", "i11002", "i12002", "i15002", "i15003", "i15004", "i16021", "i16022", "i17001",
                                                "i17002", "i17003", "i17014", "i18039", "i18040", "i20056", "i20057",
                                                "i20058", "i20059", "i20055", "i21012", "i22009", "i23080", "i23103",
                                                "i23126", "i26018", "i26019", "i30001", "i01001", "i37004", "i39005",
                                                "i39015"),
                                   year = c("1990", "1991", "1992", "1995", "1995", "1995",
                                            "1996", "1996", "1997", "1997", "1997", "1997", "1998", "1998",
                                            "2000", "2000", "2000", "2000", "2000", "2001", "2002", "2003",
                                            "2003", "2003", "2006", "2006", "2010", NA, "2017", "2019", "2019"),
                                   month = c("5", "4", "6", "6", "6", "6", "12", NA, "3", "3",
                                             "3", "5", NA, NA, "9", "9", "9", "9", "9", "9", "1", "10", "10",
                                             "10", "10", "10", "2", NA, "12", "10", "11"),
                                   day = c(NA, "30 or unknown",
                                           NA, NA, NA, NA, NA, NA, NA, NA, NA, "7", NA, NA, "25", "25",
                                           "29", "29", "19 or 20 or 21", "11", NA, "11", "12", "13", "5 or 6",
                                           "5 or 6", "15", NA, "27", "29", "11"),
                                   later_year = c(NA, "1992",
                                                  NA, NA, NA, NA, "1996", NA, NA, NA, NA, "1997", NA, NA, "2000",
                                                  "2000", "2000", "2000", "2000", "2001", "2002", "2003", "2006",
                                                  NA, NA, NA, "2011", NA, "2017", "2019", "2019"),
                                   later_month = c(NA,
                                                   "11", NA, NA, NA, NA, "12", NA, NA, NA, NA, NA, NA, NA, "10",
                                                   "10", "10", "10", "9", "9", "2", "10", NA, "later", NA, NA, "12",
                                                   NA, "1", "11", "11"),
                                   later_day = c(NA, "9", NA, NA, NA, NA,
                                                 "24", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "24", "12/13",
                                                 "5", NA, NA, NA, NA, NA, "11 (velado; possibly died a day or two earlier)",
                                                 NA, "3 or 4", "28", "12 or 14")),
                              row.names = c(NA, -31L), class = c("tbl_df", "tbl", "data.frame"))
  expect_no_error(clean_dates(de.raw_datenotes))
  expect_snapshot(clean_dates(de.raw_datenotes))
})
