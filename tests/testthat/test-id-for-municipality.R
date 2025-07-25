test_that("Testing id_for_municipality", {
  expect_equal(id_for_municipality("La Paz"), "020101")
  expect_equal(id_for_municipality("La Paz", "La Paz"), "020101")
  expect_warning(id_for_municipality("La Paz", "Cochabamba"), "Municipality La Paz not found in department Cochabamba . Returning empty string.")
  expect_equal(suppressWarnings(id_for_municipality("La Paz", "Cochabamba")), "")
  expect_equal(id_for_municipality("Potosí", "Potosí"), "050101")
  expect_equal(id_for_municipality("Potosi"), "050101")
  expect_equal(suppressWarnings(id_for_municipality("San Ramón")), "071103")
  expect_warning(id_for_municipality("San Ramon"), "Multiple municipalities found for San Ramon without department . Returning first ID found.")
  expect_equal(id_for_municipality("San Ramón", "Santa Cruz"), "071103")
  expect_equal(id_for_municipality("San Ramón", "Beni"), "080702")
})

test_that("Testing id_for_municipality_2", {
  expect_equal(id_for_municipality_2("zudanez"), "010301")
  expect_equal(id_for_municipality_2("villa montes"), "060303")
  expect_equal(id_for_municipality_2("villamontes"), "060303")
  expect_warning(id_for_municipality_2("San Ignacio"), "Multiple municipalities found for San Ignacio without department . Returning first ID found.")
  expect_equal(suppressWarnings(id_for_municipality_2("San Ignacio")), "070301")
  expect_equal(id_for_municipality_2("San Ignacio", "Beni"), "080501")
})

test_that("Testing municipality_vector_from_id", {
  expect_equal(municipality_vector_from_id("020101"), list(municipality = "La Paz", province = "Murillo", department = "La Paz"))
  expect_equal(municipality_vector_from_id("050101") %>% unlist %>% unname, c("Potosí", "Tomás Frías", "Potosí"))
  expect_equal(municipality_vector_from_id("071103") %>% unlist %>% unname, c("San Ramón", "Ñuflo de Chaves", "Santa Cruz"))
  expect_equal(municipality_vector_from_id("080702") %>% unlist %>% unname, c("San Ramón", "Mamoré", "Beni"))
  expect_equal(municipality_vector_from_id("010301") %>% unlist %>% unname, c("Villa Zudáñez", "Zudáñez", "Chuquisaca"))
  expect_equal(municipality_vector_from_id("060303") %>% unlist %>% unname, c("Villa Montes", "Gran Chaco", "Tarija"))
  expect_equal(municipality_vector_from_id("070301") %>% unlist %>% unname, c("San Ignacio de Velasco", "Velasco", "Santa Cruz"))
  expect_equal(municipality_vector_from_id("080501") %>% unlist %>% unname, c("San Ignacio de Moxos", "Moxos", "Beni"))
})

test_that("Testing municipality_name_from_id", {
  expect_equal(municipality_name_from_id("020101"), "La Paz")
  expect_equal(municipality_name_from_id("050101"), "Potosí")
  expect_equal(municipality_name_from_id("071103"), "San Ramón")
  expect_equal(municipality_name_from_id("080702"), "San Ramón")
  expect_equal(municipality_name_from_id("010301"), "Villa Zudáñez")
  expect_equal(municipality_name_from_id("060303"), "Villa Montes")
  expect_equal(municipality_name_from_id("070301"), "San Ignacio de Velasco")
  expect_equal(municipality_name_from_id("080501"), "San Ignacio de Moxos")
})

test_that("Testing province_name_from_id", {
  expect_equal(province_name_from_id("020101"), "Murillo")
  expect_equal(province_name_from_id("050101"), "Tomás Frías")
  expect_equal(province_name_from_id("071103"), "Ñuflo de Chaves")
  expect_equal(province_name_from_id("080702"), "Mamoré")
  expect_equal(province_name_from_id("010301"), "Zudáñez")
  expect_equal(province_name_from_id("060303"), "Gran Chaco")
  expect_equal(province_name_from_id("070301"), "Velasco")
  expect_equal(province_name_from_id("080501"), "Moxos")
})

test_that("Testing department_name_from_id", {
  expect_equal(department_name_from_id("020101"), "La Paz")
  expect_equal(department_name_from_id("050101"), "Potosí")
  expect_equal(department_name_from_id("071103"), "Santa Cruz")
  expect_equal(department_name_from_id("080702"), "Beni")
  expect_equal(department_name_from_id("010301"), "Chuquisaca")
  expect_equal(department_name_from_id("060303"), "Tarija")
  expect_equal(department_name_from_id("070301"), "Santa Cruz")
  expect_equal(department_name_from_id("080501"), "Beni")
})


