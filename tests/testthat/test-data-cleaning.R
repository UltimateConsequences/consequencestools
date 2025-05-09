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
  df_no_na <- dplyr::filter(df, !is.na(state_perpetrator))

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

test_that("assign_state_responsibility_levels works correctly", {
  # Test input
  df <- data.frame(
    state_responsibility = c("State perpetrator", "Separate from state", NA, "State likely perpetrator",
                             "State indirect perpetrator", "Disputed", "Possibly state involved",
                             "State perpetrator, State victim in mutiny"),
    intentionality = c("Direct", "Direct", "Direct", "Direct",
                       "Direct", "Direct", "Direct", "Direct")
  )
  df_no_na <- dplyr::filter(df, !is.na(state_responsibility))

  # Run function
  result <- assign_state_responsibility_levels(df)
  result_simple <- assign_state_responsibility_levels(df, simplify = TRUE)
  result_no_na <- assign_state_responsibility_levels(df_no_na)

  # Check results
  expect_s3_class(result$state_responsibility, "factor")
  expect_equal(result$sr_text, df$state_responsibility)
  expect_equal(sort(levels(result$state_responsibility)),
               sort(c("Unknown", "State perpetrator", "Separate from state", "State likely perpetrator",
                      "State indirect perpetrator", "Disputed", "Possibly state involved",
                      "State perpetrator, State victim in mutiny")))

  expect_s3_class(result_simple$state_responsibility, "factor")
  expect_equal(sort(levels(result_simple$state_responsibility)),
               sort(c("Unknown", "Separate", "Perpetrator", "Involved")))

  # Check original text preservation
  expect_equal(as.character(df_no_na$state_responsibility), result_no_na$sr_text)

  # Check NA handling
  expect_equal(as.character(result$state_responsibility[is.na(df$state_responsibility)]), "Unknown")

  # Check intentionality override
  df_intent <- df
  df_intent$intentionality[1] <- "Incidental"
  df_intent$intentionality[2] <- "Conflict Accident"
  result_intent <- assign_state_responsibility_levels(df_intent)
  expect_equal(as.character(result_intent$state_responsibility[1]), "Incidental")
  expect_equal(as.character(result_intent$state_responsibility[2]), "Accidental")
})

test_that("assign_levels works correctly", {
  # Test with all variables
  result <- assign_levels(deaths_aug24, "pres_admin", "state_responsibility", "state_perpetrator", "protest_domain", "location_precision")
  expect_snapshot(str(result))

  # Test with missing variable in dataframe
  df_no_protest <- select(deaths_aug24, -protest_domain)
  expect_warning(
    assign_levels(df_no_protest, "pres_admin", "protest_domain"),
    "Variable not found in dataframe: protest_domain"
  )

  # Test with non-existent function
  expect_warning(
    assign_levels(deaths_aug24, "non_existent_var"),
    "No corresponding function for variable: non_existent_var"
  )

  # Test with subset of variables
  result_subset <- assign_levels(deaths_aug24, "pres_admin", "state_responsibility")
  expect_snapshot(str(result_subset))

  # Test that unspecified variables remain unchanged
  original_protest_domain <- deaths_aug24$protest_domain
  result_partial <- assign_levels(deaths_aug24, "pres_admin", "state_responsibility")
  expect_equal(result_partial$protest_domain, original_protest_domain)
})
