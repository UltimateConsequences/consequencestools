test_that("Testing add_presidency_column", {
  pres_freq_table <- count(assign_presidency_levels(deaths_aug24), pres_admin)
  expect_snapshot(add_presidency_column(pres_freq_table, "pres_admin",
                                        dest_var= "presidency_surnames"))
  expect_snapshot(add_presidency_column(pres_freq_table, "pres_admin",
                                        dest_var= "presidency_surnames",
                                        .location="replace"))
  expect_snapshot(add_presidency_column(pres_freq_table, "pres_admin",
                                        dest_var= "id_presidency",
                                        .location="beside"))
  expect_snapshot(add_presidency_column(pres_freq_table, "pres_admin",
                                        dest_var= "presidency_year_es",
                                        .location="left"))
  expect_snapshot(add_presidency_column(pres_freq_table, "pres_admin",
                                        dest_var= "presidency_initials_num",
                                        .location="right"))
})

