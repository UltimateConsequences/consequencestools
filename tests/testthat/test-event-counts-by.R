test_that("event_list_with_counts_by(): Appropriately sized table", {
  num_events <- length(unique(deaths_aug24$event_title))
  expect_equal(ncol(event_list_with_counts_by(deaths_aug24, department)), 3)
  expect_gte(nrow(event_list_with_counts_by(deaths_aug24, department)), num_events)
  expect_equal(ncol(event_list_with_counts_by(deaths_aug24, department, month)), 4)
  expect_gte(nrow(event_list_with_counts_by(deaths_aug24, department, month)), num_events)
})

test_that("event_list_with_counts_by(): Results table is consistent with past runs", {
  expect_no_error(event_list_with_counts_by(deaths_aug24, department))
  expect_no_error(event_list_with_counts_by(deaths_aug24, protest_domain, protest_campaign))
  expect_snapshot(event_list_with_counts_by(deaths_aug24, department))
  expect_snapshot(event_list_with_counts_by(deaths_aug24, protest_domain, protest_campaign))
})

test_that("event_counts_by(): Appropriately sized table", {
  expect_equal(ncol(event_counts_by(deaths_aug24, department)), 3)
  expect_equal(nrow(event_counts_by(deaths_aug24, department)), 9)
  expect_equal(ncol(event_counts_by(deaths_aug24, department, month)), 4)
  expect_equal(ncol(event_counts_by(deaths_aug24, department, month, count_events=TRUE)), 5)
})

test_that("event_counts_by(): Results table is consistent with past runs", {
  expect_no_error(event_counts_by(deaths_aug24, department))
  expect_no_error(event_counts_by(deaths_aug24, protest_domain, protest_campaign))
  expect_snapshot(event_counts_by(deaths_aug24, department))
  expect_snapshot(event_counts_by(deaths_aug24, protest_domain, protest_campaign))
  expect_snapshot(event_counts_by(deaths_aug24, protest_domain, protest_campaign, count_events = TRUE))
})

test_that("Missing column produces warning",{
  evc <- event_counts_by(deaths_aug24, protest_domain, protest_campaign)
  evc_without_events <- dplyr::select(evc, -"events")
  expect_warning(truncate_event_list(evc_without_events))
})

test_that("event_counts_by(): Appropriately sized table", {
  expect_equal(nrow(event_counts_by(deaths_aug24, department)),
               nrow(truncate_event_list(event_counts_by(deaths_aug24, department))))
  expect_equal(ncol(truncate_event_list(event_counts_by(deaths_aug24, department, month))), 4)
})

test_that("truncate_event_list(): Results table is consistent with past runs",{
  expect_snapshot(truncate_event_list(event_counts_by(deaths_aug24, protest_domain, protest_campaign)))
  expect_snapshot(truncate_event_list(event_counts_by(deaths_aug24, pres_admin, protest_domain)))
})
