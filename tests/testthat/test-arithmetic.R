test_that("share_of_largest_n works correctly", {
  # Test with default rank
  expect_equal(share_of_largest_n(c(10, 5, 3, 2)), 0.5)

  # Test with specified rank
  expect_equal(share_of_largest_n(c(10, 5, 3, 2), rank = 2), 0.75)

  # Test with all equal values
  expect_equal(share_of_largest_n(c(1, 1, 1, 1), rank = 2), 0.5)

  # Test with negative values
  expect_equal(share_of_largest_n(c(-10, -5, -3, -2), rank = 1), 0.1)

  # Test with NA values
  expect_equal(share_of_largest_n(c(10, 5, NA, 5), rank = 2), 0.75)

  # Test error when rank is too large
  expect_error(share_of_largest_n(c(1, 2, 3), rank = 4))

  # Test with single value
  expect_equal(share_of_largest_n(c(10)), 1)
})

test_that("n_observations_to_reach_share works correctly", {
  # Test with default share
  expect_equal(n_observations_to_reach_share(c(10, 5, 3, 2)), 1)

  # Test with higher share
  expect_equal(n_observations_to_reach_share(c(10, 5, 3, 2), share = 0.8), 3)

  # Test with all equal values
  expect_equal(n_observations_to_reach_share(c(1, 1, 1, 1), share = 0.5), 2)

  # Test with NA values
  expect_equal(n_observations_to_reach_share(c(10, 5, NA, 2), share = 0.7), 2)

  # Test error when share is out of bounds
  expect_error(n_observations_to_reach_share(c(1, 2, 3), share = 1.1))
  expect_error(n_observations_to_reach_share(c(1, 2, 3), share = 0))

  # Test with single value
  expect_equal(n_observations_to_reach_share(c(10)), 1)
})
