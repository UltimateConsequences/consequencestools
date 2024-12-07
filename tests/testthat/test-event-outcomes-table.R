test_that("Appropriately sized table for event_counts_table", {
  deaths_aug24_filtered <- standard_filter(deaths_aug24)
  deaths_aug24_unfiltered <- deaths_aug24

  n_calculated_columns <- 11
  n_dropped_separate_columns <- 2
  n_dropped_extra_columns <- 4

  range_by_event <- event_counts_table(deaths_aug24_filtered, deaths_aug24_unfiltered)
  range_by_event_sep <- event_counts_table(deaths_aug24_filtered, deaths_aug24_unfiltered,
                                            drop_separate = TRUE)
  range_by_event_sep_ext <- event_counts_table(deaths_aug24_filtered, deaths_aug24_unfiltered,
                                                drop_separate = TRUE, drop_extra = TRUE)

  expect_equal(nrow(range_by_event),
               nrow(unique(na.omit(deaths_aug24[,"event_title"]))))
  expect_equal(ncol(range_by_event_sep),
               n_calculated_columns - n_dropped_separate_columns + 1)
  expect_equal(ncol(range_by_event_sep_ext),
               n_calculated_columns - n_dropped_separate_columns - n_dropped_extra_columns + 1)
})


test_that("Results table is consistent with past runs", {
  deaths_aug24_filtered <- standard_filter(deaths_aug24)
  deaths_aug24_unfiltered <- deaths_aug24
  expect_no_error(event_counts_table(deaths_aug24_filtered, deaths_aug24_unfiltered))
  expect_snapshot(event_counts_table(deaths_aug24_filtered, deaths_aug24_unfiltered))
  expect_snapshot(event_counts_table(deaths_aug24_filtered, deaths_aug24_unfiltered,
                                 drop_separate = TRUE))
  expect_snapshot(event_counts_table(deaths_aug24_filtered, deaths_aug24_unfiltered,
                                 drop_separate = TRUE, drop_extra = TRUE))
})

test_that("Appropriately sized table for event_counts_table", {
  deaths_aug24_filtered <- standard_filter(deaths_aug24)
  deaths_aug24_unfiltered <- deaths_aug24

  n_calculated_columns <- 11
  n_dropped_separate_columns <- 2
  n_dropped_extra_columns <- 4

  range_by_event <- event_counts_table(deaths_aug24_filtered, deaths_aug24_unfiltered)
  range_by_event_sep <- event_counts_table(deaths_aug24_filtered, deaths_aug24_unfiltered,
                                            drop_separate = TRUE)
  range_by_event_sep_ext <- event_counts_table(deaths_aug24_filtered, deaths_aug24_unfiltered,
                                                drop_separate = TRUE, drop_extra = TRUE)

  expect_equal(nrow(range_by_event),
               nrow(unique(na.omit(deaths_aug24[,"event_title"]))))
  expect_equal(ncol(range_by_event_sep),
               n_calculated_columns - n_dropped_separate_columns + 1)
  expect_equal(ncol(range_by_event_sep_ext),
               n_calculated_columns - n_dropped_separate_columns - n_dropped_extra_columns + 1)
})


test_that("Appropriately sized table for event_outcomes_table", {
  deaths_aug24_filtered <- standard_filter(deaths_aug24)
  deaths_aug24_unfiltered <- deaths_aug24
  event_counts <- event_counts_table(deaths_aug24_filtered, deaths_aug24_unfiltered)

  outcomes_table <- event_outcomes_table(event_counts, event_status_aug24)

  n_calculated_columns <- 11

  range_by_event_sep <- event_counts_table(deaths_aug24_filtered, deaths_aug24_unfiltered,
                                           drop_separate = TRUE)
  range_by_event_sep_ext <- event_counts_table(deaths_aug24_filtered, deaths_aug24_unfiltered,
                                               drop_separate = TRUE, drop_extra = TRUE)

  expect_equal(nrow(outcomes_table),
               nrow(unique(na.omit(deaths_aug24[,"event_title"]))))
  expect_equal(ncol(outcomes_table),
               n_calculated_columns + 3)
})

test_that("Appropriately sized table for event_description_table", {

  description_table <- event_description_table(deaths_aug24)

  expect_equal(nrow(description_table),
               nrow(unique(na.omit(deaths_aug24[,"event_title"]))))
  expect_equal(ncol(description_table), 4)
})


test_that("Event description table is consistent with past runs", {
  expect_no_error(event_description_table(deaths_aug24))
  expect_snapshot(event_description_table(deaths_aug24))
})
