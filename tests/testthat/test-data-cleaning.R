test_that("assign_presidency_levels works correctly", {
  # Test input
  # de.raw$pres_admin[25*(1:27)] -> dput()
  df.subsample <- data.frame(pres_admin = c("Víctor Paz Estenssoro", "Víctor Paz Estenssoro", "Gonzalo Sanchez de Lozada (1st)",
                                  "Gonzalo Sanchez de Lozada (1st)", "Gonzalo Sanchez de Lozada (1st)",
                                  "Hugo Banzer (2nd)", "Hugo Banzer (2nd)", "Hugo Banzer (2nd)",
                                  "Hugo Banzer (2nd)", "Hugo Banzer (2nd)", "Jorge Quiroga", "Gonzalo Sanchez de Lozada (2nd)",
                                  "Gonzalo Sanchez de Lozada (2nd)", "Gonzalo Sanchez de Lozada (2nd)",
                                  "Gonzalo Sanchez de Lozada (2nd)", "Gonzalo Sanchez de Lozada (2nd)",
                                  "Gonzalo Sanchez de Lozada (2nd)", "Evo Morales", "Evo Morales",
                                  "Evo Morales", "Evo Morales", "Evo Morales", "Evo Morales", "Interim military government",
                                  "Jeanine Áñez", "Luis Arce", "Luis Arce"))
  df.alphabetical <- data.frame(pres_admin = c("Evo Morales", "Evo Morales", "Evo Morales", "Evo Morales",
                                                "Evo Morales", "Evo Morales", "Gonzalo Sanchez de Lozada (1st)",
                                                "Gonzalo Sanchez de Lozada (1st)", "Gonzalo Sanchez de Lozada (1st)",
                                                "Gonzalo Sanchez de Lozada (2nd)", "Gonzalo Sanchez de Lozada (2nd)",
                                                "Gonzalo Sanchez de Lozada (2nd)", "Gonzalo Sanchez de Lozada (2nd)",
                                                "Gonzalo Sanchez de Lozada (2nd)", "Gonzalo Sanchez de Lozada (2nd)",
                                                "Hugo Banzer (2nd)", "Hugo Banzer (2nd)", "Hugo Banzer (2nd)",
                                                "Hugo Banzer (2nd)", "Hugo Banzer (2nd)", "Interim military government",
                                                "Jeanine Áñez", "Jorge Quiroga", "Luis Arce", "Luis Arce",
                                                "Víctor Paz Estenssoro", "Víctor Paz Estenssoro"))

  # Run function
  result.sub <- assign_presidency_levels(df.subsample)
  result <- result.sub
  result.alpha <- assign_presidency_levels(df.alphabetical)

  # Check results
  expect_s3_class(result.sub$pres_admin, "factor")
  expect_equal(levels(result.sub$pres_admin), president$levels)

  # CHeck if they sort by level
  result.alpha_sorted <- arrange(result.alpha, pres_admin)
  expect_identical(result.sub$pres_admin, result.alpha_sorted$pres_admin)

  # Test with no pres_admin column
  df_no_pres <- data.frame(other_col = 1:3)
  result_no_pres <- assign_presidency_levels(df_no_pres)
  expect_equal(df_no_pres, result_no_pres)
})

test_that("assign_state_perpetrator_levels works correctly", {
  # Test input
  df <- data.frame(state_perpetrator = c("yes", "no", NA, "LIKELY YES", 'indirect', "disputed", "Suspected", "in mutiny"))
  df_no_na <- filter(df, !is.na(state_perpetrator))

  # Run function
  result <- assign_state_perpetrator_levels(df)
  result_simple <- assign_state_perpetrator_levels(df, simplify = TRUE)
  result_no_na <- assign_state_perpetrator_levels(df_no_na)

  # Check results
  expect_s3_class(result$state_perpetrator, "factor")
  expect_equal(sort(levels(result$state_perpetrator)), sort(c("Unknown", "Yes", "No", "Likely Yes", "Indirect",
                                                              "Disputed", "Suspected", "In Mutiny")))

  expect_s3_class(result_simple$state_perpetrator, "factor")
  expect_equal(levels(result_simple$state_perpetrator), c("Unknown", "No", "Indirect", "Mutiny", "Yes"))

  # Check title case
  expect_true(all(str_to_title(df_no_na$state_perpetrator) == as.character(result_no_na$state_perpetrator)))

  # Check NA handling
  expect_equal(as.character(result$state_perpetrator[is.na(df$state_perpetrator)]), "Unknown")
})
