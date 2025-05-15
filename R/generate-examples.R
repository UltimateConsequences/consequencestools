# Function to generate Roxygen @examples or testthat code with results
#'
#' This function takes a string of function calls and generates Roxygen
#' `@examples` or `testthat` code with the results of those calls.
#' It evaluates each function call and appends the result as a comment.
#'
#' IMPORTANT: You should only use a set of calls where you have manually
#' verified the results as correct for the function.
#'
#' @param calls A string containing function calls, each on a new line.
#' @param format A string indicating the format of the output.
#'   "examples" for Roxygen `@examples` or "tests" for `testthat`.
#'
#' @export
#'
#' @examples
#' generate_examples_with_results("sqrt(4)", "examples") #  "# examples #' sqrt(4) # \"2\""
#' calls <- 'n_filter(dplyr::starwars, (species=="Droid") & height<120)'
#' generate_examples_with_results(calls, format = "tests")
#' generate_examples_with_results(calls, format = "examples")
generate_examples_with_results <- function(calls, format = "examples") {
  # Split the input string into individual lines
  call_lines <- strsplit(calls, "\n")[[1]]

  # Trim whitespace from each line
  call_lines <- trimws(call_lines)

  # Extract the function name from the first call
  function_name <- sub("\\(.*$", "", call_lines[1])

  # Initialize an empty vector to store the formatted output
  output <- c()

  if (format == "examples") {
    # Add the Roxygen @examples header
    output <- c(output, "#' @examples")

    # Iterate over each line
    for (call in call_lines) {
      # Evaluate the function call to get the result
      result <- tryCatch(
        eval(parse(text = call)), # Evaluate the function call
        error = function(e) paste("Error:", e$message) # Handle errors gracefully
      )

      # Convert the result to a string for inclusion as a comment
      result_str <- if (is.atomic(result) && length(result) == 1) {
        paste0("\"", as.character(result), "\"")
      } else {
        ""
      }

      # Add the formatted example line
      output <- c(output, paste0("#' ", call, " # ", result_str))
    }
  } else if (format == "tests") {
    # Add the testthat header
    output <- c(output, paste0('test_that("Testing ', function_name, '", {'))

    # Iterate over each line
    for (call in call_lines) {
      # Evaluate the function call to get the result
      result <- tryCatch(
        eval(parse(text = call)), # Evaluate the function call
        warning = function(w) {
          warning_message <- w$message
          attr(w, "result") <- tryCatch(eval(parse(text = call), envir = parent.frame()), error = function(e) NULL)
          w
        },
        error = function(e) paste("Error:", e$message) # Handle errors gracefully
      )

      # Check if the result is a warning
      if (inherits(result, "warning")) {
        result_value <- attr(result, "result")
        if (is.null(result_value)) result_value <- ""
        result_str <- if (is.atomic(result_value) && length(result_value) == 1) {
          paste0("\"", as.character(result_value), "\"")
        } else {
          "\"\""
        }
        # Add a test line with expect_warning
        output <- c(
          output,
          paste0("  expect_warning(", call, ", \"", result$message, "\")"),
          paste0("  expect_equal(", call, ", ", result_str, ")")
        )
      } else {
        # Convert the result to a testable format
        result_str <- if (is.atomic(result) && length(result) == 1) {
          paste0("\"", as.character(result), "\"")
        } else {
          "\"\""
        }
        # Add the formatted test line
        output <- c(output, paste0("  expect_equal(", call, ", ", result_str, ")"))
      }
    }

    # Close the test_that block
    output <- c(output, "})")
  } else {
    stop("Invalid format. Use 'examples' or 'tests'.")
  }

  # Collapse the output into a single string
  output <- paste(output, collapse = "\n")
  cat(output)
}

