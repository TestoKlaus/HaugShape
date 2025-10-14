# Test suite for HaugShape cluster analysis functions

test_that("cluster_plot creates valid ggplot object", {
  test_data <- data.frame(
    PC1 = rnorm(30),
    PC2 = rnorm(30)
  )

  p <- cluster_plot(test_data, x_col = "PC1", y_col = "PC2",
                   method = "kmeans", k = 3, show_shapes = FALSE)

  expect_s3_class(p, "ggplot")
})

test_that("cluster_plot validates required arguments", {
  test_data <- data.frame(PC1 = rnorm(20), PC2 = rnorm(20))

  expect_error(
    cluster_plot(x_col = "PC1", y_col = "PC2"),
    "Argument 'data' is required"
  )

  expect_error(
    cluster_plot(test_data, y_col = "PC2"),
    "Argument 'x_col' is required"
  )

  expect_error(
    cluster_plot(test_data, x_col = "PC1"),
    "Argument 'y_col' is required"
  )
})

test_that("cluster_plot validates clustering method", {
  test_data <- data.frame(PC1 = rnorm(20), PC2 = rnorm(20))

  expect_error(
    cluster_plot(test_data, x_col = "PC1", y_col = "PC2", method = "invalid"),
    "'method' must be one of: kmeans, hierarchical, distance_threshold"
  )
})

test_that("cluster_plot validates k parameter", {
  test_data <- data.frame(PC1 = rnorm(20), PC2 = rnorm(20))

  expect_error(
    cluster_plot(test_data, x_col = "PC1", y_col = "PC2", k = 0),
    "'k' must be a positive integer"
  )

  expect_error(
    cluster_plot(test_data, x_col = "PC1", y_col = "PC2", k = 25),
    "'k' cannot be greater than the number of data points"
  )
})

test_that("cluster_plot works with different clustering methods", {
  test_data <- data.frame(PC1 = rnorm(30), PC2 = rnorm(30))

  p1 <- cluster_plot(test_data, x_col = "PC1", y_col = "PC2",
                     method = "kmeans", k = 3, show_shapes = FALSE)
  p2 <- cluster_plot(test_data, x_col = "PC1", y_col = "PC2",
                     method = "hierarchical", k = 3, show_shapes = FALSE)
  p3 <- cluster_plot(test_data, x_col = "PC1", y_col = "PC2",
                     method = "distance_threshold",
                     distance_threshold = 1.5, show_shapes = FALSE)

  expect_s3_class(p1, "ggplot")
  expect_s3_class(p2, "ggplot")
  expect_s3_class(p3, "ggplot")
})

test_that("cluster_plot validates distance_threshold parameter", {
  test_data <- data.frame(PC1 = rnorm(20), PC2 = rnorm(20))

  expect_error(
    cluster_plot(test_data, x_col = "PC1", y_col = "PC2",
                 method = "distance_threshold", distance_threshold = NULL),
    "'distance_threshold' is required when method = 'distance_threshold'"
  )

  expect_error(
    cluster_plot(test_data, x_col = "PC1", y_col = "PC2",
                 method = "distance_threshold", distance_threshold = -1),
    "'distance_threshold' must be a positive number"
  )
})
