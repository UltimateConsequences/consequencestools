test_that("combine_names works correctly", {
  # Test input
  df <- data.frame(
    dec_firstname = c("John", NA, "Jane"),
    dec_surnames = c("Doe", "Smith", NA)
  )

  # Expected output
  expected_df <- df %>%
    mutate(name = c("John Doe", "? Smith", "Jane ?"))

  # Run function
  result <- combine_names(df)

  # Check results
  expect_equal(result$name, expected_df$name)
  expect_equal(ncol(result), ncol(df) + 1)
  expect_equal(nrow(result), nrow(df))

  # Test with custom unknown_string
  result_custom <- combine_names(df, unknown_string = "Unknown")
  expect_equal(result_custom$name, c("John Doe", "Unknown Smith", "Jane Unknown"))
})
