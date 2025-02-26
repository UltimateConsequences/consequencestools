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

test_that("top_values_string works correctly", {
  # Sample data
  test_data <- data.frame(
    protest_domain = c("A", "B", "C", "D"),
    total = c(10, 8, 5, 3)
  )

  # Test default behavior
  expect_equal(top_values_string(test_data), "A, B, C")

  # Test with custom n_values
  expect_equal(top_values_string(test_data, n_values = 2), "A, B")

  # Test with included counts
  expect_equal(top_values_string(test_data, incl_counts = TRUE), "A (10), B (8), C (5)")

  # Test with custom 'by' column
  test_data$frequency <- c(15, 12, 7, 4)
  expect_equal(top_values_string(test_data, by = "frequency"), "A, B, C")

  # Test with all parameters combined
  expect_equal(top_values_string(test_data, n_values = 2, incl_counts = TRUE, by = "frequency"), "A (15), B (12)")

  # Test error for non-existent 'by' column
  expect_error(top_values_string(test_data, by = "nonexistent"))

  expect_snapshot(top_values_string(event_counts_by(deaths_aug24, department), 8))

  evco <- event_counts_by(deaths_aug24, protest_domain, count_events=TRUE)
  expect_snapshot(top_values_string(evco, 5, incl_counts = TRUE, by="n_events"))
})

