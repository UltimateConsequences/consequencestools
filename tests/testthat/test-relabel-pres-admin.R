test_that("relabel_pres_admin works", {
  def <- deaths_aug24
  def_labeled <- relabel_pres_admin(def, presidency_year, keep_original = TRUE)
  expect_equal(ncol(def_labeled), ncol(def) + 1)
  expect_true("pres_admin_original" %in% colnames(def_labeled))
  expect_equal(def_labeled$pres_admin_original, def$pres_admin)
  expect_snapshot(def_labeled)

  def_labeled2 <- relabel_pres_admin(def, presidency_initials) %>% suppressWarnings()
  expect_equal(ncol(def_labeled2), ncol(def) + 1)
  expect_true(all(unique(def_labeled2$pres_admin) %in% presidency_name_table$presidency_initials))

  expect_snapshot(def_labeled2)
})
