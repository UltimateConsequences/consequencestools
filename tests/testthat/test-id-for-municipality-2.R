# Test file for municipality ID lookup functions
library(testthat)
library(dplyr)

# Create test data based on the provided structure
test_anexo_municipios <- data.frame(
  codigo = c("010101", "010102", "010103", "010201", "010202", "010301", "010302", "010303"),
  municipio = c("Sucre", "Yotala", "Poroma", "Azurduy", "Tarvita", "Villa Zudáñez", "Presto", "Villa Mojocoya"),
  provincia = c("Oropeza", "Oropeza", "Oropeza", "Azurduy", "Azurduy", "Zudáñez", "Zudáñez", "Zudáñez"),
  departamento = c("Chuquisaca", "Chuquisaca", "Chuquisaca", "Chuquisaca", "Chuquisaca", "Chuquisaca", "Chuquisaca", "Chuquisaca"),
  stringsAsFactors = FALSE
)

test_muni_id_lookup_table <- data.frame(
  id_muni = c("010101", "010102", "010103", "010201", "010202", "010301", "010302", "010303"),
  muni_gb2014 = c("Sucre", "Yotala", "Poroma", "Azurduy", "Tarvita", "Zudañez", "Presto", "Mojocoya"),
  muni_anexo = c("Sucre", "Yotala", "Poroma", "Azurduy", "Tarvita", "Villa Zudáñez", "Presto", "Villa Mojocoya"),
  department = c("Chuquisaca", "Chuquisaca", "Chuquisaca", "Chuquisaca", "Chuquisaca", "Chuquisaca", "Chuquisaca", "Chuquisaca"),
  muni_list = I(list(
    "Sucre",
    "Yotala", 
    "Poroma",
    "Azurduy",
    "Tarvita",
    c("Villa Zudáñez", "Zudañez"),
    "Presto",
    c("Villa Mojocoya", "Mojocoya")
  )),
  stringsAsFactors = FALSE
)

test_that("id_for_municipality returns correct IDs for exact matches", {
  expect_equal(id_for_municipality("Sucre", "", test_anexo_municipios), "010101")
  expect_equal(id_for_municipality("Yotala", "", test_anexo_municipios), "010102")
  expect_equal(id_for_municipality("Villa Zudáñez", "", test_anexo_municipios), "010301")
})

test_that("id_for_municipality handles department filtering correctly", {
  expect_equal(id_for_municipality("Sucre", "Chuquisaca", test_anexo_municipios), "010101")
  expect_warning(
    result <- id_for_municipality("Sucre", "La Paz", test_anexo_municipios),
    "Municipality Sucre not found in department La Paz"
  )
  expect_equal(result, "")
})

test_that("id_for_municipality handles non-existent municipalities", {
  expect_warning(
    result <- id_for_municipality("NonExistent", "", test_anexo_municipios),
    "Municipality NonExistent not found"
  )
  expect_equal(result, "")
})

test_that("id_for_municipality handles NA input", {
  result <- id_for_municipality(NA, "", test_anexo_municipios)
  expect_true(is.na(result))
})

test_that("id_for_municipality_2 returns correct IDs for exact matches", {
  expect_equal(id_for_municipality_2("Sucre", "", test_muni_id_lookup_table), "010101")
  expect_equal(id_for_municipality_2("Yotala", "", test_muni_id_lookup_table), "010102")
  expect_equal(id_for_municipality_2("Villa Zudáñez", "", test_muni_id_lookup_table), "010301")
})

test_that("id_for_municipality_2 handles alternative names correctly", {
  expect_equal(id_for_municipality_2("Zudañez", "", test_muni_id_lookup_table), "010301")
  expect_equal(id_for_municipality_2("Villa Zudáñez", "", test_muni_id_lookup_table), "010301")
  expect_equal(id_for_municipality_2("Mojocoya", "", test_muni_id_lookup_table), "010303")
  expect_equal(id_for_municipality_2("Villa Mojocoya", "", test_muni_id_lookup_table), "010303")
})

test_that("id_for_municipality_2 handles department filtering correctly", {
  expect_equal(id_for_municipality_2("Sucre", "Chuquisaca", test_muni_id_lookup_table), "010101")
  expect_warning(
    result <- id_for_municipality_2("Sucre", "La Paz", test_muni_id_lookup_table),
    "Municipality Sucre not found in department La Paz"
  )
  expect_equal(result, "")
})

test_that("id_for_municipality_2 handles non-existent municipalities", {
  expect_warning(
    result <- id_for_municipality_2("NonExistent", "", test_muni_id_lookup_table),
    "Municipality NonExistent not found"
  )
  expect_equal(result, "")
})

test_that("id_for_municipality_2 handles NA input", {
  result <- id_for_municipality_2(NA, "", test_muni_id_lookup_table)
  expect_true(is.na(result))
})

test_that("id_for_municipality_2 validates input parameters", {
  expect_error(
    id_for_municipality_2(123, "", test_muni_id_lookup_table),
    "municipality_i is not a character vector"
  )
  expect_error(
    id_for_municipality_2("Sucre", 123, test_muni_id_lookup_table),
    "department_i is not a character vector"
  )
  expect_error(
    id_for_municipality_2("Sucre", "", "not_a_dataframe"),
    "muni_list_table is not a data frame"
  )
  
  # Test missing muni_list column
  bad_table <- test_muni_id_lookup_table
  bad_table$muni_list <- NULL
  expect_error(
    id_for_municipality_2("Sucre", "", bad_table),
    "muni_list not in names\\(muni_list_table\\)"
  )
})

test_that("Functions handle numeric codes correctly", {
  # Test with numeric codes in anexo table
  numeric_anexo <- test_anexo_municipios
  numeric_anexo$codigo <- as.numeric(c(10101, 10102, 10103, 10201, 10202, 10301, 10302, 10303))
  
  expect_equal(id_for_municipality("Sucre", "", numeric_anexo), "010101")
  expect_equal(id_for_municipality("Yotala", "", numeric_anexo), "010102")
})

test_that("Functions handle case-insensitive matching through str_equivalent", {
  # Assuming str_equivalent handles case-insensitive matching
  # These tests may need adjustment based on actual str_equivalent behavior
  skip_if_not(exists("str_equivalent"), "str_equivalent function not available")
  
  expect_equal(id_for_municipality("sucre", "", test_anexo_municipios), "010101")
  expect_equal(id_for_municipality_2("sucre", "", test_muni_id_lookup_table), "010101")
})

test_that("Multiple municipality warning is generated when appropriate", {
  # Create test data with duplicate municipality names in different departments
  multi_muni_table <- rbind(
    test_anexo_municipios,
    data.frame(
      codigo = "020101",
      municipio = "Sucre", # Duplicate name
      provincia = "Murillo",
      departamento = "La Paz",
      stringsAsFactors = FALSE
    )
  )
  
  expect_warning(
    result <- id_for_municipality("Sucre", "", multi_muni_table),
    "Multiple municipalities found for Sucre.*Returning first ID found"
  )
  expect_equal(result, "010101") # Should return first match
})