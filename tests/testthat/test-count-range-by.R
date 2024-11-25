test_that("Appropriately sized table for count_range_by", {
  deaths_aug24_filtered <- standard_filter(deaths_aug24)
  deaths_aug24_unfiltered <- deaths_aug24

  n_calculated_columns <- 11
  n_dropped_separate_columns <- 2
  n_dropped_extra_columns <- 4

  range_by_domain <- count_range_by(deaths_aug24_filtered, deaths_aug24_unfiltered, protest_domain)
  range_by_department <- count_range_by(deaths_aug24_filtered, deaths_aug24_unfiltered, department)
  range_by_department_sep <- count_range_by(deaths_aug24_filtered, deaths_aug24_unfiltered, department,
                                        drop_separate = TRUE)
  range_by_department_sep_ext <- count_range_by(deaths_aug24_filtered, deaths_aug24_unfiltered, department,
                                            drop_separate = TRUE, drop_extra = TRUE)

  expect_equal(nrow(range_by_domain),
               nrow(unique(na.omit(deaths_aug24[,"protest_domain"]))))
  expect_equal(nrow(range_by_department),
               nrow(unique(na.omit(deaths_aug24[,"department"]))))
  expect_equal(ncol(range_by_department_sep),
               n_calculated_columns - n_dropped_separate_columns + 1)
  expect_equal(ncol(range_by_department_sep_ext),
               n_calculated_columns - n_dropped_separate_columns - n_dropped_extra_columns + 1)
})


test_that("Results table is consistent with past runs", {
  deaths_aug24_filtered <- standard_filter(deaths_aug24)
  deaths_aug24_unfiltered <- deaths_aug24

  expect_no_error(count_range_by(deaths_aug24_filtered, deaths_aug24_unfiltered, protest_domain))
  expect_snapshot(count_range_by(deaths_aug24_filtered, deaths_aug24_unfiltered, protest_domain))
  expect_snapshot(count_range_by(deaths_aug24_filtered, deaths_aug24_unfiltered, department))
  expect_snapshot(count_range_by(deaths_aug24_filtered, deaths_aug24_unfiltered, department,
                                 drop_separate = TRUE))
  expect_snapshot(count_range_by(deaths_aug24_filtered, deaths_aug24_unfiltered, department,
                                 drop_separate = TRUE, drop_extra = TRUE))
})
