test_that("generate_examples_with_results works", {
  call <- "n_filter(dplyr::starwars, (species==\"Droid\") & height<120)"
  expected_examples <- "#' @examples\n#' n_filter(dplyr::starwars, (species==\"Droid\") & height<120) # \"3\""
  expected_test <- "test_that(\"Testing n_filter\", {\n  expect_equal(n_filter(dplyr::starwars, (species==\"Droid\") & height<120), \"3\")\n})"

  expect_snapshot(generate_examples_with_results(call, format = "examples"))
  expect_snapshot(generate_examples_with_results(call, format = "tests"))
})
