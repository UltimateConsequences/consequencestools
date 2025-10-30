# tests/testthat/test-sort-var-description.R

test_that("sorted_by_widest_appearance reorders levels correctly", {
  # Create sample data
  test_data <- tibble::tibble(
    year = c(2010, 2010, 2011, 2011, 2012, 2013),
    category = c("A", "B", "A", "C", "A", "B")
  )

  # Create sample var_description
  test_desc <- list(
    r_variable = "category",
    levels = c("A", "B", "C"),
    colors = c("A" = "#FF0000", "B" = "#00FF00", "C" = "#0000FF")
  )

  # Apply function
  result <- sorted_by_widest_appearance(test_desc, test_data, year)

  # A appears in 3 years, B in 2 years, C in 1 year
  expect_equal(as.character(result$ranked), c("A", "B", "C"))
  expect_equal(result$levels, c("A", "B", "C"))
  expect_equal(names(result$colors), c("A", "B", "C"))
  expect_equal(as.character(result$colors), c("#FF0000", "#00FF00", "#0000FF"))
})

test_that("sorted_by_widest_appearance handles tied ranks", {
  test_data <- tibble::tibble(
    year = c(2010, 2010, 2011, 2011),
    category = c("A", "B", "A", "B")
  )

  test_desc <- list(
    r_variable = "category",
    levels = c("A", "B"),
    colors = c("A" = "#FF0000", "B" = "#00FF00")
  )

  result <- sorted_by_widest_appearance(test_desc, test_data, year)

  # Both A and B appear in 2 years
  expect_equal(length(result$ranked), 2)
  expect_true(all(c("A", "B") %in% as.character(result$ranked)))
})

test_that("sorted_by_widest_appearance preserves Spanish levels", {
  test_data <- tibble::tibble(
    year = c(2010, 2010, 2011, 2011, 2012),
    category = c("A", "B", "A", "C", "A")
  )

  test_desc <- list(
    r_variable = "category",
    levels = c("A", "B", "C"),
    levels_es = c("Categoría A", "Categoría B", "Categoría C"),
    colors = c("A" = "#FF0000", "B" = "#00FF00", "C" = "#0000FF")
  )

  result <- sorted_by_widest_appearance(test_desc, test_data, year)

  # Check that Spanish levels are reordered to match English
  expect_equal(result$levels, c("A", "B", "C"))
  expect_equal(result$levels_es, c("Categoría A", "Categoría B", "Categoría C"))
})

test_that("sorted_by_widest_appearance preserves Spanish colors", {
  test_data <- tibble::tibble(
    year = c(2010, 2010, 2011, 2012),
    category = c("B", "A", "B", "B")
  )

  test_desc <- list(
    r_variable = "category",
    levels = c("A", "B"),
    levels_es = c("Categoría A", "Categoría B"),
    colors = c("A" = "#FF0000", "B" = "#00FF00"),
    colors_es = c("Categoría A" = "#FF0000", "Categoría B" = "#00FF00")
  )

  result <- sorted_by_widest_appearance(test_desc, test_data, year)

  # B appears in 3 years, A in 1 year
  expect_equal(result$levels, c("B", "A"))
  expect_equal(result$levels_es, c("Categoría B", "Categoría A"))
  expect_equal(names(result$colors_es), c("Categoría B", "Categoría A"))
  expect_equal(as.character(result$colors_es), c("#00FF00", "#FF0000"))
})

test_that("sorted_by_most_frequent reorders by frequency", {
  test_data <- tibble::tibble(
    category = c("A", "A", "A", "B", "B", "C")
  )

  test_desc <- list(
    r_variable = "category",
    levels = c("C", "B", "A"),
    colors = c("C" = "#0000FF", "B" = "#00FF00", "A" = "#FF0000")
  )

  result <- sorted_by_most_frequent(test_desc, test_data)

  # A has 3, B has 2, C has 1
  expect_equal(as.character(result$ranked), c("A", "B", "C"))
  expect_equal(result$levels, c("A", "B", "C"))
  expect_equal(names(result$colors), c("A", "B", "C"))
  expect_equal(as.character(result$colors), c("#FF0000", "#00FF00", "#0000FF"))
})

test_that("sorted_by_most_frequent ignores grouping_var parameter", {
  test_data <- tibble::tibble(
    year = c(2010, 2010, 2011),
    category = c("A", "B", "A")
  )

  test_desc <- list(
    r_variable = "category",
    levels = c("A", "B"),
    colors = c("A" = "#FF0000", "B" = "#00FF00")
  )

  result1 <- sorted_by_most_frequent(test_desc, test_data)
  result2 <- sorted_by_most_frequent(test_desc, test_data, grouping_var = year)

  # Both should give same result (A appears 2x, B appears 1x)
  expect_equal(result1$ranked, result2$ranked)
  expect_equal(result1$levels, result2$levels)
})

test_that("sorted_by_most_frequent preserves Spanish translations", {
  test_data <- tibble::tibble(
    category = c("C", "C", "C", "A", "A", "B")
  )

  test_desc <- list(
    r_variable = "category",
    levels = c("A", "B", "C"),
    levels_es = c("Categoría A", "Categoría B", "Categoría C"),
    colors = c("A" = "#FF0000", "B" = "#00FF00", "C" = "#0000FF"),
    colors_es = c("Categoría A" = "#FF0000",
                  "Categoría B" = "#00FF00",
                  "Categoría C" = "#0000FF")
  )

  result <- sorted_by_most_frequent(test_desc, test_data)

  # C has 3, A has 2, B has 1
  expect_equal(result$levels, c("C", "A", "B"))
  expect_equal(result$levels_es, c("Categoría C", "Categoría A", "Categoría B"))
  expect_equal(names(result$colors_es),
               c("Categoría C", "Categoría A", "Categoría B"))
})

test_that("functions work with real package data", {
  skip_if_not(exists("deaths_aug24"), "deaths_aug24 not available")
  skip_if_not(exists("lev"), "lev not available")
  skip_if_not(exists("assign_levels"), "assign_levels not available")

  deaths_filtered <- assign_levels(deaths_aug24, "standard", .simplify = TRUE)

  # Test sorted_by_widest_appearance with real data
  if ("protest_domain" %in% names(lev)) {
    result_wide <- sorted_by_widest_appearance(
      lev$protest_domain,
      deaths_filtered,
      pres_admin
    )

    expect_true("ranked" %in% names(result_wide))
    expect_equal(length(result_wide$levels), length(result_wide$colors))
    if (!is.null(result_wide$levels_es)) {
      expect_equal(length(result_wide$levels), length(result_wide$levels_es))
    }
  }

  # Test sorted_by_most_frequent with real data
  if ("protest_domain" %in% names(lev)) {
    result_freq <- sorted_by_most_frequent(
      lev$protest_domain,
      deaths_filtered
    )

    expect_true("ranked" %in% names(result_freq))
    expect_equal(length(result_freq$levels), length(result_freq$colors))
  }
})

test_that("functions handle single-level variables", {
  test_data <- tibble::tibble(
    year = c(2010, 2011, 2012),
    category = c("A", "A", "A")
  )

  test_desc <- list(
    r_variable = "category",
    levels = c("A"),
    colors = c("A" = "#FF0000")
  )

  result_wide <- sorted_by_widest_appearance(test_desc, test_data, year)
  result_freq <- sorted_by_most_frequent(test_desc, test_data)

  expect_equal(length(result_wide$levels), 1)
  expect_equal(length(result_freq$levels), 1)
  expect_equal(result_wide$levels, "A")
  expect_equal(result_freq$levels, "A")
})

test_that("functions preserve all color values", {
  test_data <- tibble::tibble(
    category = c("B", "C", "A")
  )

  test_desc <- list(
    r_variable = "category",
    levels = c("A", "B", "C"),
    colors = c("A" = "#FF0000", "B" = "#00FF00", "C" = "#0000FF")
  )

  result <- sorted_by_most_frequent(test_desc, test_data)

  # Check all colors are present (even if reordered)
  expect_setequal(as.character(result$colors),
                  c("#FF0000", "#00FF00", "#0000FF"))
})
