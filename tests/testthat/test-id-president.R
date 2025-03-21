test_that("id_for_president works correctly", {
  # Sample data
  pres_table <- data.frame(
    presidency = c("René Barrientos", "Luis Adolfo Siles Salinas", "Alfredo Ovando"),
    id_presidency = c("p86", "p87", "p88"),
    presidency_commonname = c("René Barrientos", "Luis Adolfo Siles", "Alfredo Ovando"),
    stringsAsFactors = FALSE
  )

  # Test with default parameters
  expect_equal(id_for_president("René Barrientos", pres_table), "p86")

  # Test with a different index_var
  expect_equal(id_for_president("Luis Adolfo Siles", pres_table, index_var = "presidency_commonname"),
               "Luis Adolfo Siles")

  # Test with a name that doesn't exist
  expect_equal(id_for_president("John Doe", pres_table), "")

  # Test with partial name match
  expect_equal(id_for_president("Ovando", pres_table), "")

  # Test case sensitivity
  expect_equal(id_for_president("rené barrientos", pres_table), "p86")

  # Test with empty string input
  expect_equal(id_for_president("", pres_table), "")

  # Test with NULL input
  expect_error(id_for_president(NULL, pres_table))
})

test_that("add_id_for_president works correctly", {
  # Sample presidency name table
  pres_table <- data.frame(
    presidency = c("René Barrientos", "Luis Adolfo Siles", "Alfredo Ovando"),
    id_presidency = c("p86", "p87", "p88"),
    presidency_commonname = c("René Barrientos", "Luis Adolfo Siles Salinas", "Alfredo Ovando"),
    stringsAsFactors = FALSE
  )

  # Sample dataset to be modified
  sample_dataset <- data.frame(
    event_title = c("Event 1", "Event 2", "Event 3"),
    pres_admin = c("René Barrientos", "Luis Adolfo Siles", "Unknown President"),
    stringsAsFactors = FALSE
  )

  # Test with default parameters
  result <- add_id_for_president(sample_dataset, pres_table)
  expect_true("id_pres" %in% names(result))
  expect_equal(result$id_pres, c("p86", "p87", ""))
  expect_equal(which(names(result) == "id_pres"), which(names(result) == "pres_admin") + 1)

  # Test with 'presidency' column instead of 'pres_admin'
  sample_dataset_2 <- sample_dataset %>% rename(presidency = pres_admin)
  result_2 <- add_id_for_president(sample_dataset_2, pres_table)
  expect_true("id_pres" %in% names(result_2))
  expect_equal(result_2$id_pres, c("p86", "p87", ""))
  expect_equal(which(names(result_2) == "id_pres"), which(names(result_2) == "presidency") + 1)

  # Test overwrite parameter
  sample_dataset_3 <- sample_dataset %>% mutate(id_pres = "old_id")
  result_3 <- add_id_for_president(sample_dataset_3, pres_table, overwrite = TRUE)
  expect_equal(result_3$id_pres, c("p86", "p87", ""))

  # Test overwrite parameter (false)
  expect_warning(add_id_for_president(sample_dataset_3, pres_table, overwrite = FALSE))

  # Test with missing required columns
  sample_dataset_4 <- sample_dataset %>% select(-pres_admin)
  expect_error(add_id_for_president(sample_dataset_4, pres_table))
})
