# Test data setup
setup_test_data <- function() {
  data.frame(
    PC1 = rnorm(50),
    PC2 = rnorm(50),
    group = sample(c("A", "B", "C"), 50, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

test_that("shape_plot creates valid ggplot object", {
  test_data <- setup_test_data()

  p <- shape_plot(test_data, x_col = "PC1", y_col = "PC2", group_col = "group")

  expect_s3_class(p, "ggplot")
  expect_true("PC1" %in% names(p$data))
  expect_true("PC2" %in% names(p$data))
})

test_that("shape_plot validates required arguments", {
  test_data <- setup_test_data()

  expect_error(
    shape_plot(x_col = "PC1", y_col = "PC2"),
    "Argument 'data' is required"
  )

  expect_error(
    shape_plot(test_data, y_col = "PC2"),
    "Argument 'x_col' is required"
  )

  expect_error(
    shape_plot(test_data, x_col = "PC1"),
    "Argument 'y_col' is required"
  )
})

test_that("shape_plot validates data frame input", {
  expect_error(
    shape_plot("not_a_dataframe", x_col = "PC1", y_col = "PC2"),
    "'data' must be a data frame"
  )

  expect_error(
    shape_plot(data.frame(), x_col = "PC1", y_col = "PC2"),
    "'data' cannot be empty"
  )
})

test_that("shape_plot validates column existence", {
  test_data <- setup_test_data()

  expect_error(
    shape_plot(test_data, x_col = "missing_col", y_col = "PC2"),
    "Column 'missing_col' not found in data"
  )

  expect_error(
    shape_plot(test_data, x_col = "PC1", y_col = "missing_col"),
    "Column 'missing_col' not found in data"
  )

  expect_error(
    shape_plot(test_data, x_col = "PC1", y_col = "PC2", group_col = "missing_col"),
    "Column 'missing_col' not found in data"
  )
})

test_that("shape_plot validates numeric columns", {
  test_data <- data.frame(
    x = letters[1:10],
    y = 1:10,
    group = rep(c("A", "B"), 5)
  )

  expect_error(
    shape_plot(test_data, x_col = "x", y_col = "y"),
    "Column 'x' must be numeric"
  )

  test_data$x <- 1:10
  test_data$y <- letters[1:10]

  expect_error(
    shape_plot(test_data, x_col = "x", y_col = "y"),
    "Column 'y' must be numeric"
  )
})

test_that("shape_plot validates plot_style parameter", {
  test_data <- setup_test_data()

  expect_error(
    shape_plot(test_data, x_col = "PC1", y_col = "PC2", plot_style = "invalid_style"),
    "plot_style must be one of: Haug, inverted_Haug, publication"
  )
})

test_that("shape_plot works with different plot_style options", {
  test_data <- setup_test_data()

  p1 <- shape_plot(test_data, x_col = "PC1", y_col = "PC2", plot_style = "Haug")
  p2 <- shape_plot(test_data, x_col = "PC1", y_col = "PC2", plot_style = "inverted_Haug")
  p3 <- shape_plot(test_data, x_col = "PC1", y_col = "PC2", plot_style = "publication")

  expect_s3_class(p1, "ggplot")
  expect_s3_class(p2, "ggplot")
  expect_s3_class(p3, "ggplot")
})

test_that("shape_plot handles hulls parameter correctly", {
  test_data <- setup_test_data()

  p_no_hulls <- shape_plot(test_data, x_col = "PC1", y_col = "PC2",
                          group_col = "group", show_hulls = FALSE)
  p_with_hulls <- shape_plot(test_data, x_col = "PC1", y_col = "PC2",
                            group_col = "group", show_hulls = TRUE)

  expect_s3_class(p_no_hulls, "ggplot")
  expect_s3_class(p_with_hulls, "ggplot")
})

test_that("shape_plot handles missing values appropriately", {
  test_data <- setup_test_data()
  test_data$PC1[1:5] <- NA
  test_data$PC2[6:10] <- NA

  expect_message(
    p <- shape_plot(test_data, x_col = "PC1", y_col = "PC2", group_col = "group"),
    "rows removed due to missing or non-finite values"
  )

  expect_s3_class(p, "ggplot")
})

test_that("shape_plot works without grouping variable", {
  test_data <- setup_test_data()

  p <- shape_plot(test_data, x_col = "PC1", y_col = "PC2")

  expect_s3_class(p, "ggplot")
})

test_that("shape_plot parameter validation for numeric values", {
  test_data <- setup_test_data()

  # Test that function accepts valid numeric parameters
  p <- shape_plot(test_data,
                 x_col = "PC1",
                 y_col = "PC2",
                 point_size = 3,
                 title_size = 20,
                 label_size = 16,
                 tick_size = 12)

  expect_s3_class(p, "ggplot")
})

test_that("shape_plot handles custom colors correctly", {
  test_data <- setup_test_data()

  p <- shape_plot(test_data,
                 x_col = "PC1",
                 y_col = "PC2",
                 group_col = "group",
                 point_color = c("red", "blue", "green"))

  expect_s3_class(p, "ggplot")
})
