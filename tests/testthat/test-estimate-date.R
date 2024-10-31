test_that("estimated_date() provides correct results", {
  expect_equal(estimated_date(NA, NA, NA), NA)
  expect_equal(estimated_date(2015, NA, NA), as.Date("2015-06-30"))
  expect_equal(estimated_date(2015, 8, NA), as.Date("2015-08-15"))
  expect_equal(estimated_date(2015, 10, 11), as.Date("2015-10-11"))
})

test_that("estimated_date() provides correct results", {
  expect_equal(estimated_date_string(NA, NA, NA), NA_character_)
  expect_equal(estimated_date_string(2015, NA, NA), "2015-06-30")
  expect_equal(estimated_date_string(2015, 8, NA), "2015-08-15")
  expect_equal(estimated_date_string(2015, 1, 11), "2015-01-11")
})

test_that("combine_dates works correctly", {
  # Setup test dataframe
  test_df <- data.frame(
    year = c(2021, 2022, 2023, 2020),
    month = c(1, 6, 2, NA),
    day = c(17, 28, NA, NA),
    event_title = c("Event 1", "Event 2", "Event 3", "Event 4")
  )

  # Test basic functionality
  result <- combine_dates(test_df)
  expect_equal(nrow(result), 4)
  expect_equal(result$date_text, c("17 Jan 2021", "28 Jun 2022", "February 2023", "2020"))
  expect_equal(result$date, as.Date(c("2021-01-17", "2022-06-28", "2023-02-15", "2020-06-30")))
  expect_true("date" %in% names(result))
  expect_true("date_text" %in% names(result))

  # Test incl_laterdate
  test_df <- test_df[1:2,]

  test_df$later_year <- c(2021, 2022)
  test_df$later_month <- c(2, 7)
  test_df$later_day <- c(1, 1)
  result_later <- combine_dates(test_df, incl_laterdate = TRUE)
  expect_true("laterdate" %in% names(result_later))

  # Test date_at_front
  result_front <- combine_dates(test_df, date_at_front = TRUE)
  expect_equal(names(result_front)[1:2], c("event_title", "date"))

  # Add more tests as needed
})
