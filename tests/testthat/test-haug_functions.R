# Test suite for HaugShape panel and overview functions

test_that("Haug_panel creates valid plot output", {
  test_data <- data.frame(
    PC1 = rnorm(50),
    PC2 = rnorm(50),
    group = sample(c("A", "B", "C"), 50, replace = TRUE)
  )

  # Test hulls panel
  p1 <- Haug_panel(test_data, x_col = "PC1", y_col = "PC2",
                   group_col = "group", panel_type = "hulls")
  expect_true(!is.null(p1))

  # Test contours panel
  p2 <- Haug_panel(test_data, x_col = "PC1", y_col = "PC2",
                   group_col = "group", panel_type = "contours")
  expect_true(!is.null(p2))
})

test_that("Haug_panel validates required arguments", {
  test_data <- data.frame(PC1 = rnorm(20), PC2 = rnorm(20),
                         group = rep(c("A", "B"), 10))

  expect_error(
    Haug_panel(x_col = "PC1", y_col = "PC2"),
    "Argument 'data' is required"
  )

  expect_error(
    Haug_panel(test_data, y_col = "PC2"),
    "Argument 'x_col' is required"
  )

  expect_error(
    Haug_panel(test_data, x_col = "PC1"),
    "Argument 'y_col' is required"
  )
})

test_that("Haug_panel validates column existence", {
  test_data <- data.frame(PC1 = rnorm(20), PC2 = rnorm(20))

  expect_error(
    Haug_panel(test_data, x_col = "missing_col", y_col = "PC2"),
    "Column 'missing_col' not found in data"
  )

  expect_error(
    Haug_panel(test_data, x_col = "PC1", y_col = "PC2",
               group_col = "missing_group"),
    "Column 'missing_group' not found in data"
  )
})

test_that("Haug_panel validates numeric columns", {
  test_data <- data.frame(
    x = letters[1:10],
    y = 1:10,
    group = rep(c("A", "B"), 5)
  )

  expect_error(
    Haug_panel(test_data, x_col = "x", y_col = "y", group_col = "group"),
    "Column 'x' must be numeric"
  )
})

test_that("Haug_panel validates group_vals parameter", {
  test_data <- data.frame(
    PC1 = rnorm(20),
    PC2 = rnorm(20),
    group = rep(c("A", "B"), 10)
  )

  expect_error(
    Haug_panel(test_data, x_col = "PC1", y_col = "PC2",
               group_col = "group", group_vals = c("C", "D")),
    "Group values not found in data: C, D"
  )
})

test_that("Haug_overview validation works correctly", {
  skip_if_not_installed("patchwork")

  test_data <- data.frame(
    col1 = rnorm(50),
    col2 = rnorm(50),
    col3 = rnorm(50),
    col4 = rnorm(50),
    group = sample(c("A", "B", "C"), 50, replace = TRUE)
  )

  # Test basic functionality
  result <- Haug_overview(test_data,
                          cols = c("col1", "col2", "col3", "col4"),
                          group_col = "group",
                          export_pdf = FALSE)

  expect_true(is.list(result))
  expect_true("hull_and_contours" %in% names(result))
  expect_true("boxplot" %in% names(result))
})
