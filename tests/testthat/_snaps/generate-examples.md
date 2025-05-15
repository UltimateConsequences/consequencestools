# generate_examples_with_results works

    Code
      generate_examples_with_results(call, format = "examples")
    Output
      #' @examples
      #' n_filter(dplyr::starwars, (species=="Droid") & height<120) # "3"

---

    Code
      generate_examples_with_results(call, format = "tests")
    Output
      test_that("Testing n_filter", {
        expect_equal(n_filter(dplyr::starwars, (species=="Droid") & height<120), "3")
      })

