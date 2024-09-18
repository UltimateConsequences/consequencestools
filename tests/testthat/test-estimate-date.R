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

