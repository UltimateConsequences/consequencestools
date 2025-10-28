test_that("wide_event_outcomes_reactable works as expected", {
  deaths_aug24_filtered <- standard_filter(deaths_aug24)
  deaths_aug24_unfiltered <- deaths_aug24
  event_outcomes_aug24 <- wide_event_outcomes_table(deaths_aug24_filtered, deaths_aug24_unfiltered,
                                                    event_status_aug24)
  expect_snapshot(wide_event_outcomes_reactable(event_outcomes_aug24))
})


