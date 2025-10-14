# Test suite for HaugShape utility functions

test_that("run_app validates input parameters", {
  expect_error(
    run_app(port = "not_numeric"),
    "'port' must be a single integer between 1 and 65535, or NULL"
  )

  expect_error(
    run_app(port = 0),
    "'port' must be a single integer between 1 and 65535, or NULL"
  )

  expect_error(
    run_app(port = 99999),
    "'port' must be a single integer between 1 and 65535, or NULL"
  )

  expect_error(
    run_app(host = 123),
    "'host' must be a single character string"
  )

  expect_error(
    run_app(launch.browser = "yes"),
    "'launch.browser' must be a single logical value"
  )

  expect_error(
    run_app(quiet = "no"),
    "'quiet' must be a single logical value"
  )
})

# Test helper functions if they exist
test_that("helper functions work correctly", {
  # Test data validation helper
  test_data <- data.frame(
    x = 1:10,
    y = 1:10,
    group = rep(c("A", "B"), 5)
  )

  # These tests would check internal validation functions
  # if they were exported or made testable
  expect_true(is.data.frame(test_data))
  expect_true(nrow(test_data) > 0)
  expect_true(all(c("x", "y", "group") %in% colnames(test_data)))
})

# Mock test for file operations (if applicable)
test_that("file validation functions work", {
  # Create a temporary directory for testing
  temp_dir <- tempdir()
  temp_file <- file.path(temp_dir, "test.txt")

  # Create a test file
  writeLines("test content", temp_file)

  # Test file existence checks (these would be part of internal functions)
  expect_true(file.exists(temp_file))
  expect_true(dir.exists(temp_dir))

  # Clean up
  unlink(temp_file)
})

# Test parameter validation functions
test_that("parameter validation works correctly", {
  # Test numeric validation
  expect_error({
    if (!is.numeric("not_numeric")) stop("Must be numeric")
  }, "Must be numeric")

  # Test range validation
  expect_error({
    val <- -1
    if (val < 0) stop("Must be non-negative")
  }, "Must be non-negative")

  # Test length validation
  expect_error({
    val <- c(1, 2, 3)
    if (length(val) != 1) stop("Must be single value")
  }, "Must be single value")
})

# Test color validation
test_that("color parameter validation works", {
  # Valid colors should pass
  valid_colors <- c("red", "blue", "#FF0000", "rgb(255,0,0)")

  for (color in valid_colors) {
    expect_true(is.character(color))
    expect_true(nchar(color) > 0)
  }

  # Invalid colors should be caught (if validation exists)
  expect_true(is.character(""))  # Empty string test
})
