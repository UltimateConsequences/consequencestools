test_that("wide_event_outcomes_reactable working as expected", {
  deaths_aug24_filtered <- standard_filter(deaths_aug24)
  deaths_aug24_unfiltered <- deaths_aug24

  wetable <- wide_event_outcomes_table(deaths_aug24_filtered, deaths_aug24_unfiltered,
    event_status_aug24, drop_extra=TRUE, drop_separate=TRUE)

  expect_no_error(wide_event_outcomes_reactable(wetable))
  expect_snapshot(wide_event_outcomes_reactable(wetable))
})
