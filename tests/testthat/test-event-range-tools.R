# Sample data
event_date_range_table <- tibble(
  event_title = c("Cochabamba dept coca clashes Jan 2003", "Bus crash during miners pension protest", "Pensioner strike", "Caranavi blockade clash", "Villa Union militarization", "Tarifazo tax protest", "Senda VI coca eradication", "Pampa San Miguel urban land conflict 2003", "Chapare soldiers killed Jun 2003", "Caracollo mining protest", "Santa Rosa del Sara protests", "Gas War 2003", "Bustillos cocalero assasination", "Chapare soldiers killed late 2003", "San Ignacio de Moxos Mayor assassinated"),
  date_first = as.Date(c("2003-01-13", "2003-01-15", "2003-01-18", "2003-01-30", "2003-02-05", "2003-02-12", "2003-02-22", "2003-02-27", "2003-06-14", "2003-06-23", "2003-07-22", "2003-09-17", "2003-10-22", "2003-10-23", "2003-12-19")),
  date_last = as.Date(c("2003-01-16", "2003-01-15", "2003-01-25", "2003-01-30", "2003-02-05", "2003-02-13", "2003-02-22", "2003-02-27", "2003-06-14", "2003-06-23", "2003-07-22", "2003-10-16", "2003-10-22", "2003-12-04", "2003-12-19")),
  length_in_days = as.difftime(c(4, 1, 8, 1, 1, 2, 1, 1, 1, 1, 1, 30, 1, 43, 1), units = "days")
)

# Tests for count_ongoing_events()
test_that("count_ongoing_events works correctly", {
  expect_equal(count_ongoing_events(event_date_range_table, as.Date("2003-01-14")), 1)
  expect_equal(count_ongoing_events(event_date_range_table, as.Date("2003-01-15")), 2)
  expect_equal(count_ongoing_events(event_date_range_table, as.Date("2003-02-01")), 0)
  expect_equal(count_ongoing_events(event_date_range_table, as.Date("2003-10-01")), 1)
  expect_equal(count_ongoing_events(event_date_range_table, as.Date("2003-12-31")), 0)
})

# Tests for ount_events_in_month()
test_that("count_events_in_month works correctly", {
  expect_equal(count_events_in_month(event_date_range_table, zoo::as.yearmon("Jan 2003")), 4)
  expect_equal(count_events_in_month(event_date_range_table, zoo::as.yearmon("Feb 2003")), 4)
  expect_equal(count_events_in_month(event_date_range_table, zoo::as.yearmon("Jun 2003")), 2)
  expect_equal(count_events_in_month(event_date_range_table, zoo::as.yearmon("Oct 2003")), 3)
  expect_equal(count_events_in_month(event_date_range_table, zoo::as.yearmon("Dec 2003")), 2)
})
