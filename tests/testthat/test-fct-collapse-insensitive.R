
# Test code
test_that("fct_collapse and fct_collapse_insensitive work correctly", {

  # Sample data frames
  df1 <- data.frame(
    id_numbers = 1:10,
    state_perpetrator = c("Yes", "Likely Yes", "No", "Indirect", "Unknown",
                          "Presumed Yes", "In mutiny", "Disputed", "Likely No", "Suspected")
  )

  df2 <- data.frame(
    id_numbers = 1:10,
    state_perpetrator = c("yes", "LIKELY YES", "no", "indirect", "UNKNOWN",
                          "Presumed yes", "in mutiny", "disputed", "likely no", "suspected")
  )

  # Mapping
  mapping <- list(
    Yes = c("Yes", "Likely Yes", "Presumed Yes"),
    Indirect = c("Indirect"),
    No = c("No", "Likely No", "Presumed No"),
    Mutiny = c("In mutiny"),
    Unknown = c("Unknown", "Disputed", "Suspected")
  )


  # Test fct_collapse with df1
  result1 <- df1 %>%
    mutate(
      factored = fct_collapse(state_perpetrator, !!!mapping),
      original = state_perpetrator
    ) %>% suppressWarnings()

  expect_setequal(levels(result1$factored), c("Yes", "Indirect", "No", "Mutiny", "Unknown"))
  expect_equal(as.character(result1$factored),
               c("Yes", "Yes", "No", "Indirect", "Unknown", "Yes", "Mutiny", "Unknown", "No", "Unknown"))

  # Test fct_collapse_insensitive with df2
  result2 <- df2 %>%
    mutate(
      factored = fct_collapse_insensitive(state_perpetrator, !!!mapping),
      original = state_perpetrator
    )

  expect_setequal(levels(result2$factored), c("Yes", "Indirect", "No", "Mutiny", "Unknown"))
  expect_equal(as.character(result2$factored),
               c("Yes", "Yes", "No", "Indirect", "Unknown", "Yes", "Mutiny", "Unknown", "No", "Unknown"))
})
