test_that("Non-numeric fails", {
  expect_error(render_age("text"))
})

halves <-  tibble::new_tibble(list(age=1:4)) %>%
      dplyr::mutate(half_age = age - 0.5) %>%
      dplyr::mutate(age_text = render_age(half_age)) %>%
      dplyr::mutate(age_text_es = render_age_es(half_age))

sixes <-  tibble::new_tibble(list(age=-2:2)) %>%
  dplyr::mutate(powers_of_six = 6 ^ age) %>%
  dplyr::mutate(age_text = render_age(powers_of_six)) %>%
  dplyr::mutate(age_text_es = render_age_es(powers_of_six))

test_that("English renders are correct", {
  expect_equal(halves$age_text, c("6 months", "1 year, 6 months", "2 years", "3 years"))
  expect_equal(sixes$age_text, c("less than 30 days", "2 months", "1 year", "6 years", "36 years"))
})

test_that("Spanish renders are correct", {
  expect_equal(halves$age_text_es, c("6 meses", "1 año y 6 meses", "2 años", "3 años"))
  expect_equal(sixes$age_text_es, c("menos que 30 días", "2 meses", "1 año", "6 años", "36 años"))
})

rm(halves)
rm(sixes)
