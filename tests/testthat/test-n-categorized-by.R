test_that("Appropriately sized table", {
  n_calculated_columns <- 15
  expect_equal(nrow(n_categorized_by(deaths_aug24, protest_domain, complete=TRUE)),
               nrow(unique(na.omit(deaths_aug24[,"protest_domain"]))))
  expect_equal(ncol(n_categorized_by(deaths_aug24, protest_domain, complete=TRUE)),
               n_calculated_columns+1)
  expect_equal(ncol(n_categorized_by(deaths_aug24, department,
                                     complete=TRUE)),
               n_calculated_columns+1)
})

test_that("Appropriately sized table", {
  n_calculated_columns <- 4
  expect_equal(nrow(n_responsibility_by(deaths_aug24, protest_domain, complete=TRUE)),
               nrow(unique(na.omit(deaths_aug24[,"protest_domain"]))))
  expect_equal(nrow(n_responsibility_by(deaths_aug24, department, complete=TRUE)),
               nrow(unique(na.omit(deaths_aug24[,"department"]))))
  expect_equal(ncol(n_responsibility_by(deaths_aug24, protest_domain, complete=TRUE)),
               n_calculated_columns+1)
  expect_equal(ncol(n_responsibility_by(deaths_aug24, department,
                                     complete=TRUE)),
               n_calculated_columns+1)
})

test_that("Results table is consistent with past runs", {
  expect_no_error(n_categorized_by(deaths_aug24, pres_admin, complete=TRUE))
  expect_snapshot(n_categorized_by(deaths_aug24, pres_admin, complete=FALSE))
  expect_snapshot(n_categorized_by(deaths_aug24, pres_admin, complete=TRUE))
  expect_snapshot(n_categorized_by(deaths_aug24, pres_admin, complete=TRUE, sp_binary = TRUE))
  expect_snapshot(n_responsibility_by(deaths_aug24, cause_death, complete=TRUE))
})
