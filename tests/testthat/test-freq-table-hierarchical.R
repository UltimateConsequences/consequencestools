test_that("two_layer_frequency_table results are consistent with past runs.", {
  expect_snapshot(two_layer_frequency_table(deaths_aug24,
                                            perp_affiliation, dec_affiliation,
                                            sort=TRUE))
  mtcars2 <- mtcars %>% mutate(cyl = as.factor(cyl), gear = as.factor(gear))
  # expect_snapshot(two_layer_frequency_table(mtcars2, cyl, gear,
  #                                           sort = TRUE, threshold = 1))
})

test_that("insert_rows_by_group_vars works", {
  df_main <- data.frame(group = c("A", "A", "B"),
                        sub = c("x", "y", "z"),
                        n = 1:3, stringsAsFactors = FALSE)
  df_add  <- data.frame(group = c("A", "C"),
                        sub = c("other", "other"),
                        n = c(10, 5), stringsAsFactors = FALSE)
  df_inserted <- insert_rows_by_group_vars(df_main, df_add, group, sub)
  expect_equal(which(df_inserted$sub=="other"), c(3,5)) # other inserted 3rd and 5th positions
  expect_equal(which(df_inserted$group=="C"), 5) # C is inserted at the end
  expect_equal(df_inserted[df_inserted$group=="C",2], "other")
  expect_equal(df_inserted[df_inserted$group=="C",3], 5)
})

# test_that("two_layer_frequency_kable results are consistent with past runs.", {
#   expect_snapshot(two_layer_frequency_kable(deaths_aug24, perp_affiliation,
#                                             dec_affiliation, sort=TRUE))
#   mtcars2 <- mtcars %>% mutate(cyl = as.factor(cyl), gear = as.factor(gear))
#   expect_snapshot(two_layer_frequency_kable(mtcars2, cyl, gear, sort = TRUE,
#                                             threshold = 1, unit_is_deaths = FALSE))
# })
