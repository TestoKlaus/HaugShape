test_that("shape_plot creates a ggplot object", {
  df <- data.frame(x = rnorm(100), y = rnorm(100))
  p <- shape_plot(df, "x", "y")
  expect_true(inherits(p, "ggplot"))
})
