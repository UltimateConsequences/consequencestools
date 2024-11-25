utils::globalVariables(c("unconfirmed"))

test_that("standard_filter() works", {
  deaths_sample <- structure(list(event_title = c("Rockfall death during Coca March",
                                                  "Potosí campesino blockade", "Gas War 2003", "CIDOB March car crash",
                                                  "TIPNIS CIDOB March 2011", "Puerto Villarroel disability March accident",
                                                  "TIPNIS March 2012", "Disabled vigil incidental deaths", "Possible Achacachi Assassination",
                                                  "Oruro border killings", "Death of mining union leader", "Mapiri mining cooperative attack",
                                                  "UPEA balcony collapse", "Coripata Adepcoca clash", "San Ignacio de Velasco beating victim",
                                                  "Santa Cruz Indigenous marcher death", "Potosí civic/campesino clashes"),
                                  id_indiv = c("i14003", "i18038", "i23070", "i26021", "i31003",
                                               "i32001", "i32006", "i36012", "i38001", "i40001", "i40002", "i40003",
                                               "i41005", "i41001", "i41002", "i41003", "i41004"),
                                  dec_surnames = c("Sixto Mamani Ramírez",
                                                   "Llanos Grimaldez", "Condori Álvarez", "Flores Castro", "Moye Noza",
                                                   "Pacha", "Cunay", "Peñaloza", "Mamani", "Colque Gómez", "Gutiérrez Luna",
                                                   "Cáceres (Caseres)", "Cadena Choque", "Quispe Nina", "Peña Vaca",
                                                   "Rojas Abiyuna", "Titi Topolo"),
                                  year = c(1994L, 1998L, 2003L,
                                           2006L, 2011L, 2012L, 2012L, 2016L, 2018L, 2020L, 2020L, 2020L,
                                           2021L, 2021L, 2021L, 2021L, 2021L),
                                  unconfirmed = c(NA, NA, NA,
                                                  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, TRUE), # Note that the final TRUE is nonfactual
                                  intentionality = c("Nonconflict Accident",
                                                     "Nonconflict Accident", "Nonconflict Accident", "Nonconflict Accident",
                                                     "Nonconflict Accident", "Nonconflict Accident", "Nonconflict Accident",
                                                     "Nonconflict Accident", "Nonconflict Accident", "Direct", "Nonconflict",
                                                     "Direct / Conflict Accident", "Conflict Accident", "Direct",
                                                     "Direct (disputed)", "Nonconflict", "Indirect (disputed)")),
                             row.names = c(NA, -17L),
                             class = c("tbl_df", "tbl", "data.frame"))
  standard <- standard_filter(deaths_sample)
  expect_equal(ncol(standard), ncol(deaths_sample))
  expect_equal(nrow(standard), 5)
  expect_equal(nrow(dplyr::filter(standard, is.na(unconfirmed))), nrow(standard))
  expect_equal(sum(standard$intentionality=="Nonconflict") +
                 sum(standard$intentionality=="Nonconflict Accident") +
                 sum(standard$intentionality=="Collateral"), 0)
})

test_that("filter_out_unintentional() works", {
  deaths_sample <- structure(list(event_title = c("Rockfall death during Coca March",
                                                  "Potosí campesino blockade", "Gas War 2003", "CIDOB March car crash",
                                                  "TIPNIS CIDOB March 2011", "Puerto Villarroel disability March accident",
                                                  "TIPNIS March 2012", "Disabled vigil incidental deaths", "Possible Achacachi Assassination",
                                                  "Oruro border killings", "Death of mining union leader", "Mapiri mining cooperative attack",
                                                  "UPEA balcony collapse", "Coripata Adepcoca clash", "San Ignacio de Velasco beating victim",
                                                  "Santa Cruz Indigenous marcher death", "Potosí civic/campesino clashes"),
                                  id_indiv = c("i14003", "i18038", "i23070", "i26021", "i31003",
                                               "i32001", "i32006", "i36012", "i38001", "i40001", "i40002", "i40003",
                                               "i41005", "i41001", "i41002", "i41003", "i41004"),
                                  dec_surnames = c("Sixto Mamani Ramírez",
                                                   "Llanos Grimaldez", "Condori Álvarez", "Flores Castro", "Moye Noza",
                                                   "Pacha", "Cunay", "Peñaloza", "Mamani", "Colque Gómez", "Gutiérrez Luna",
                                                   "Cáceres (Caseres)", "Cadena Choque", "Quispe Nina", "Peña Vaca",
                                                   "Rojas Abiyuna", "Titi Topolo"),
                                  year = c(1994L, 1998L, 2003L,
                                           2006L, 2011L, 2012L, 2012L, 2016L, 2018L, 2020L, 2020L, 2020L,
                                           2021L, 2021L, 2021L, 2021L, 2021L),
                                  unconfirmed = c(NA, NA, NA,
                                                  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, TRUE), # Note that the final TRUE is nonfactual
                                  intentionality = c("Nonconflict Accident",
                                                     "Nonconflict Accident", "Nonconflict Accident", "Nonconflict Accident",
                                                     "Nonconflict Accident", "Nonconflict Accident", "Nonconflict Accident",
                                                     "Nonconflict Accident", "Nonconflict Accident", "Direct", "Nonconflict",
                                                     "Direct / Conflict Accident", "Conflict Accident", "Direct",
                                                     "Direct (disputed)", "Nonconflict", "Indirect (disputed)")),
                             row.names = c(NA, -17L),
                             class = c("tbl_df", "tbl", "data.frame"))
  filtered <- filter_out_unintentional(deaths_sample)
  expect_equal(ncol(filtered), ncol(deaths_sample))
  expect_equal(nrow(filtered), 6)
  expect_equal(sum(filtered$intentionality=="Nonconflict") +
                 sum(filtered$intentionality=="Nonconflict Accident") +
                 sum(filtered$intentionality=="Collateral"), 0)
})

test_that("standard_filter() works", {
  deaths_sample <- structure(list(event_title = c("Rockfall death during Coca March",
                                                  "Potosí campesino blockade", "Gas War 2003", "CIDOB March car crash",
                                                  "TIPNIS CIDOB March 2011", "Puerto Villarroel disability March accident",
                                                  "TIPNIS March 2012", "Disabled vigil incidental deaths", "Possible Achacachi Assassination",
                                                  "Oruro border killings", "Death of mining union leader", "Mapiri mining cooperative attack",
                                                  "UPEA balcony collapse", "Coripata Adepcoca clash", "San Ignacio de Velasco beating victim",
                                                  "Santa Cruz Indigenous marcher death", "Potosí civic/campesino clashes"),
                                  id_indiv = c("i14003", "i18038", "i23070", "i26021", "i31003",
                                               "i32001", "i32006", "i36012", "i38001", "i40001", "i40002", "i40003",
                                               "i41005", "i41001", "i41002", "i41003", "i41004"),
                                  dec_surnames = c("Sixto Mamani Ramírez",
                                                   "Llanos Grimaldez", "Condori Álvarez", "Flores Castro", "Moye Noza",
                                                   "Pacha", "Cunay", "Peñaloza", "Mamani", "Colque Gómez", "Gutiérrez Luna",
                                                   "Cáceres (Caseres)", "Cadena Choque", "Quispe Nina", "Peña Vaca",
                                                   "Rojas Abiyuna", "Titi Topolo"),
                                  year = c(1994L, 1998L, 2003L,
                                           2006L, 2011L, 2012L, 2012L, 2016L, 2018L, 2020L, 2020L, 2020L,
                                           2021L, 2021L, 2021L, 2021L, 2021L),
                                  unconfirmed = c(NA, NA, NA,
                                                  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, TRUE), # Note that the final TRUE is nonfactual
                                  intentionality = c("Nonconflict Accident",
                                                     "Nonconflict Accident", "Nonconflict Accident", "Nonconflict Accident",
                                                     "Nonconflict Accident", "Nonconflict Accident", "Nonconflict Accident",
                                                     "Nonconflict Accident", "Nonconflict Accident", "Direct", "Nonconflict",
                                                     "Direct / Conflict Accident", "Conflict Accident", "Direct",
                                                     "Direct (disputed)", "Nonconflict", "Indirect (disputed)")),
                             row.names = c(NA, -17L),
                             class = c("tbl_df", "tbl", "data.frame"))
  filtered <- filter_out_unconfirmed(deaths_sample)
  expect_equal(ncol(filtered), ncol(deaths_sample))
  expect_equal(nrow(filtered), 16)
  expect_equal(nrow(dplyr::filter(filtered, !is.na(unconfirmed))), 0)
})

