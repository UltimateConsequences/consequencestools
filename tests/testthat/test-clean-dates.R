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

