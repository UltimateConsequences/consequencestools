test_that("n_filter works correctly", {
  test_data <- data.frame(
    species = c("Human", "Droid", "Wookiee", "Droid"),
    height = c(180, 110, 230, 130)
  )

  expect_equal(n_filter(test_data, species == "Droid"), 2)
  expect_equal(n_filter(test_data, species == "Droid" & height < 120), 1)
  expect_equal(n_filter(test_data, species == "Ewok"), 0)
})

test_that("n_municipality works correctly", {
  test_data <- data.frame(
    municipality = c("New York", "Los Angeles", "Chicago", "New York"),
    population = c(8000000, 4000000, 2700000, 8000000)
  )

  expect_equal(n_municipality(test_data, "New York"), 2)
  expect_equal(n_municipality(test_data, "Los Angeles"), 1)
  expect_equal(n_municipality(test_data, "Houston"), 0)
})
