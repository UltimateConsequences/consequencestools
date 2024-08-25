test_that("Quasilogicals collapse to yes and no", {
  va <- tibble::new_tibble(list(victim_armed =
                                  c("No", "Yes", "Likely Yes", "Likely No", "Likely no",
                                    "Likely disarmed", "Unclear", "Disputed", "Presumed No")))

  expect_equal(quasilogical_as_binary(stringr::str_to_title(va$victim_armed)) %>%
                 as.character(),
               c("No", "Yes", "Yes", "No", "No", "No", "Unclear", "Disputed",
                 "No"))
  rm(va)
})
