test_that("string_to_listcase works correctly", {
  expect_equal(string_to_listcase("hello, world"), "Hello, World")
  expect_equal(string_to_listcase("multiple, items, in, list"), "Multiple, Items, In, List")
  expect_equal(string_to_listcase("ALREADY UPPERCASE"), "Already uppercase")
  expect_equal(string_to_listcase("no comma"), "No comma")
})

