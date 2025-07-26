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

test_that("Testing render_presidency", {
  names <- names(presidency_name_table)
  for (i in names) {
    expect_snapshot(render_presidency("p104", i, source_var = "id_presidency"))
  }
})

test_that("Testing render_presidency", {
  expect_equal(as.character(render_presidency("Gonzalo Sanchez de Lozada (2nd)", "first_day")), "2002-08-06")
  expect_equal(render_presidency("Gonzalo Sanchez de Lozada (2nd)", "id_presidency"), "p107")
  expect_equal(render_presidency("Gonzalo Sanchez de Lozada (2nd)", "presidency_surnames"), "Sánchez de Lozada")
  expect_equal(render_presidency("Gonzalo Sanchez de Lozada (2nd)", "presidency_year_es"), "Gonzalo Sánchez de Lozada (2002-2003)")

  expect_equal(render_presidency("p111", "presidency_commonname_es", source_var="id_presidency"), "Gobierno interino militar (2019)")
  expect_equal(render_presidency("p111", "presidency_initials_num", source_var="id_presidency"), "Mil")
  expect_equal(render_presidency("p111", "presidency_commonname", source_var="id_presidency"), "Military government (2019)")
})
