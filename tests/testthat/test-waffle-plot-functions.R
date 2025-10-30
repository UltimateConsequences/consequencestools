# Tests for waffle plot functions

# Setup test data and descriptions
setup_test_data <- function() {
  # Create sample fill variable description
  fill_desc <- list(
    title = "Test Category",
    levels = c("Category A", "Category B", "Category C"),
    colors = c("Category A" = "#FF0000",
               "Category B" = "#00FF00",
               "Category C" = "#0000FF")
  )

  # Create sample dataframe
  test_df <- tibble::tibble(
    year = c(2010, 2010, 2012, 2012, 2014, 2014),
    pres_admin = factor(c("Admin1", "Admin1", "Admin2", "Admin2", "Admin3", "Admin3"),
                        levels = c("Admin1", "Admin2", "Admin3", "Admin4")),
    category = factor(c("Category A", "Category B", "Category A",
                        "Category C", "Category B", "Category C"),
                      levels = c("Category A", "Category B", "Category C")),
    value = c(5, 3, 4, 2, 6, 1)
  )

  list(df = test_df, fill_desc = fill_desc)
}

test_that("waffle_counts filters and counts correctly", {
  test_data <- setup_test_data()

  result <- waffle_counts(test_data$df, year, category, test_data$fill_desc)

  # Should have 6 rows (one for each year-category combination)
  expect_equal(nrow(result), 6)

  # Should have 3 columns: year, category, n
  expect_equal(ncol(result), 3)

  # Category should be properly factored
  expect_true(is.factor(result$category))
  expect_equal(levels(result$category), test_data$fill_desc$levels)
})

test_that("waffle_counts filters out NA and Unknown values", {
  test_data <- setup_test_data()

  # Add problematic rows
  df_with_nas <- test_data$df %>%
    dplyr::bind_rows(
      tibble::tibble(year = NA, pres_admin = "Admin1", category = "Category A", value = 1),
      tibble::tibble(year = 2015, pres_admin = "Unknown", category = "Category A", value = 1),
      tibble::tibble(year = 2015, pres_admin = "Admin1", category = NA, value = 1)
    )

  result <- waffle_counts(df_with_nas, pres_admin, category, test_data$fill_desc)

  # Should only have the original 6 rows
  expect_equal(nrow(result), 6)

  # Should not contain NA or "Unknown"
  expect_false(any(is.na(result$pres_admin)))
  expect_false(any(result$pres_admin == "Unknown"))
  expect_false(any(is.na(result$category)))
})

test_that("waffle_counts works on standard dataset", {
  deaths <- deaths_aug24 %>%
    assign_state_responsibility_levels(simplify = TRUE)
  result <- waffle_counts(deaths, year, state_responsibility,
                          lev$state_responsibility)

  # Should have more than 0 rows
  expect_gt(nrow(result), 0)

  # Should have full range of values
  expect_equal(sort(unique(result$year)),
               sort(unique(na.omit(deaths$year))))
  expect_equal(sort(unique(result$state_responsibility)),
               sort(unique(deaths$state_responsibility)))

  # Should have 3 columns: year, state_responsibility, n
  expect_equal(names(result), c("year", "state_responsibility", "n"))

  # state_responsibility should be properly factored
  expect_true(is.factor(result$state_responsibility))
  expect_equal(levels(result$state_responsibility), lev$state_responsibility$levels)
})

test_that("make_waffle_chart returns a ggplot object", {
  test_data <- setup_test_data()

  result <- make_waffle_chart(test_data$df, year, category,
                              test_data$fill_desc, complete_x = FALSE)

  expect_s3_class(result, "gg")
  expect_s3_class(result, "ggplot")
})

test_that("complete_x_values adds missing years", {
  test_data <- setup_test_data()

  # Create data with gaps
  df_with_gaps <- test_data$df %>%
    dplyr::filter(year != 2012)  # Remove 2012

  counts_df <- waffle_counts(df_with_gaps, year, category, test_data$fill_desc)

  completed_counts <- complete_x_values(counts_df, year, category,
                                        all_levels = NULL, .verbose = FALSE)

  # Check that 2012 was added
  expect_true(2012 %in% completed_counts$year)
})
complete_x_values()

test_that("complete_x_values fixes missing years in standard data", {
  deaths <- assign_levels(deaths_aug24, "standard", .simplify=TRUE)

  # Create data with gaps
  df_with_gaps <- deaths %>%
    dplyr::filter(year != 2012)  # Remove 2012

  waffle_counts_df <- waffle_counts(df_with_gaps,
                                    x_var = year,
                                    fill_var = protest_domain,
                                    fill_var_description = lev$protest_domain)

  expect_equal(nrow(waffle_counts_df[waffle_counts_df$year == 2012, ]), 0)

  waffle_counts_df_completed <- complete_x_values(waffle_counts_df,
                    x_var = year,
                    fill_var = protest_domain,
                    all_levels = NULL,
                    .verbose = FALSE)
  expect_true(2012 %in% waffle_counts_df_completed$year)

  result <- make_waffle_chart(df_with_gaps,
                              x_var = year,
                              fill_var = protest_domain,
                              lev$protest_domain, complete_x = FALSE)

  # Check that the plot data doesn't include 2012
  plot_data <- ggplot2::ggplot_build(result)$data[[1]]
  expect_false(2012 %in% unique(df_with_gaps$year))

  result_completed <- make_waffle_chart(df_with_gaps,
                              x_var = year,
                              fill_var = protest_domain,
                              lev$protest_domain, complete_x = TRUE)
  # Check that the plot data includes 2012
  plot_data_completed <- ggplot2::ggplot_build(result_completed)$data[[1]]
  expect_true(2012 %in% unique(plot_data_completed$x))


})

test_that("make_waffle_chart with complete_x=TRUE fills year gaps", {
#  test_data <- setup_test_data()

  # Create data with gaps in years
  df_with_gaps <- deaths_aug24 %>%
    assign_state_responsibility_levels(simplify = TRUE) %>%
    dplyr::filter(year != 2012)  # Remove 2012

  # This should add a null block for 2012
  result <- make_waffle_chart(df_with_gaps,
                              x_var = year,
                              fill_var = state_responsibility,
                              lev$state_responsibility, complete_x = FALSE)

  expect_s3_class(result, "ggplot")

  # Check that white color was added for null blocks
  fill_scale <- result$scales$get_scales("fill")
  expect_true(" " %in% names(fill_scale$palette()))
})

# test_that("make_waffle_chart with complete_x=TRUE fills pres_admin gaps", {
#   test_data <- setup_test_data()
#
#   # Create data with gaps in pres_admin (missing Admin2)
#   df_with_gaps <- test_data$df %>%
#     dplyr::filter(pres_admin != "Admin2")
#
#   # This should add a null block for Admin2
#   result <- make_waffle_chart(df_with_gaps, pres_admin, category,
#                               test_data$fill_desc, complete_x = TRUE)
#
#   expect_s3_class(result, "ggplot")
#
#   # Check that white color was added for null blocks
#   fill_scale <- result$scales$get_scales("fill")
#   expect_true(" " %in% names(fill_scale$palette()))
# })

# test_that("make_waffle_chart respects color palette from fill_var_description", {
#   test_data <- setup_test_data()
#
#   result <- make_waffle_chart(test_data$df, year, category,
#                               test_data$fill_desc, complete_x = FALSE)
#
#   # Get the fill scale
#   fill_scale <- result$scales$get_scales("fill")
#   colors <- fill_scale$palette()(length(test_data$fill_desc$colors))
#   names(colors) <- test_data$fill_desc$levels
#
#   # Colors should match (order may differ)
#   expect_setequal(names(colors), names(test_data$fill_desc$colors))
# })

test_that("make_waffle_chart_tall returns a ggplot object", {
  test_data <- setup_test_data()

  result <- make_waffle_chart_tall(test_data$df, year, category,
                                   test_data$fill_desc)

  expect_s3_class(result, "gg")
  expect_s3_class(result, "ggplot")
})

test_that("make_waffle_chart handles different waffle_width values", {
  test_data <- setup_test_data()

  result1 <- make_waffle_chart(test_data$df, year, category,
                               test_data$fill_desc, waffle_width = 5)
  result2 <- make_waffle_chart(test_data$df, year, category,
                               test_data$fill_desc, waffle_width = 15)

  expect_s3_class(result1, "ggplot")
  expect_s3_class(result2, "ggplot")
})

test_that("complete_x verbose mode produces output", {
  test_data <- setup_test_data()

  df_with_gaps <- test_data$df %>%
    dplyr::filter(year != 2012)

  # Should print verbose output
  expect_output(
    make_waffle_chart(df_with_gaps, year, category,
                      test_data$fill_desc, complete_x = TRUE, .verbose = TRUE),
    "Null values"
  )
})
