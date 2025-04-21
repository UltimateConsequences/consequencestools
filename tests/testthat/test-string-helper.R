test_that("string_to_listcase works correctly", {
  expect_equal(string_to_listcase("hello, world"), "Hello, World")
  expect_equal(string_to_listcase("multiple, items, in, list"), "Multiple, Items, In, List")
  expect_equal(string_to_listcase("ALREADY UPPERCASE"), "Already uppercase")
  expect_equal(string_to_listcase("no comma"), "No comma")
})


test_that("capitalize_sentences works correctly", {
  # Test single sentence
  expect_equal(capitalize_sentences("hello world."), "Hello world.")

  # Test multiple sentences
  expect_equal(capitalize_sentences("hello world. how are you? i'm fine!"), "Hello world. How are you? I'm fine!")

  # Test vector input
  input_vector <- c("hello world.", "how are you? i'm fine!", "this is a test.")
  expected_output <- c("Hello world.", "How are you? I'm fine!", "This is a test.")
  expect_equal(capitalize_sentences(input_vector), expected_output)

  # Test empty string
  expect_equal(capitalize_sentences(""), "")

  # Test vector with empty string
  expect_equal(capitalize_sentences(c("hello.", "", "world.")), c("Hello.", "", "World."))

  # Test sentence starting with number
  expect_equal(capitalize_sentences("123 test."), "123 test.")

  # Test preservation of existing capitalization
  expect_equal(capitalize_sentences("hello WORLD."), "Hello WORLD.")

  # Test multiple spaces between sentences
  expect_equal(capitalize_sentences("hello.  world."), "Hello.  World.")
})
