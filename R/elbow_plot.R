#' Elbow Plot for K-means Clustering
#'
#' This function visualizes the elbow point for k-means clustering to help determine
#' the optimal number of clusters. It calculates the total within-cluster sum of squares (WCSS)
#' for a range of cluster numbers and plots the WCSS against the number of clusters.
#'
#' @param data A data frame containing the points to cluster.
#' @param x_col A character string specifying the column name for the x-axis.
#' @param y_col A character string specifying the column name for the y-axis.
#' @param max_k An integer specifying the maximum number of clusters to consider. Default is 10.
#' @param scale Logical. Whether to scale the `(x, y)` points before clustering. Default is TRUE.
#' @param title A character string specifying the title of the plot. Default is "Elbow Plot".
#' @param x_label A character string specifying the label for the x-axis. Default is "Number of Clusters (k)".
#' @param y_label A character string specifying the label for the y-axis. Default is "Total Within-Cluster Sum of Squares".
#' @param export Logical. Whether to export the plot as a TIFF file. Default is FALSE.
#' @param file_name A character string specifying the file name if `export = TRUE`. Default is "elbow_plot".
#' @param file_path An optional path for saving the exported file. If NULL, the file is saved in the working directory.
#'
#' @return A ggplot2 object visualizing the elbow plot.
#'
#' @import ggplot2
#' @importFrom stats kmeans
#' @export
elbow_plot <- function(data, x_col, y_col, max_k = 10, scale = TRUE,
                       title = "Elbow Plot",
                       x_label = "Number of Clusters (k)",
                       y_label = "Total Within-Cluster Sum of Squares",
                       export = FALSE, file_name = "elbow_plot", file_path = NULL) {
  if (!x_col %in% colnames(data) || !y_col %in% colnames(data)) {
    stop("Both `x_col` and `y_col` must exist in the data frame.")
  }

  # Extract and clean the data
  coordinates <- data[, c(x_col, y_col)]
  coordinates <- coordinates[complete.cases(coordinates) & is.finite(rowSums(coordinates)), ]

  if (nrow(coordinates) < 2) {
    stop("Not enough valid data points for clustering after cleaning.")
  }

  # Optionally scale the coordinates
  if (scale) {
    coordinates <- scale(coordinates)
  }

  # Compute WCSS for k = 1 to max_k
  wcss <- sapply(1:max_k, function(k) {
    kmeans(coordinates, centers = k, nstart = 10)$tot.withinss
  })

  # Create a data frame for plotting
  elbow_data <- data.frame(
    k = 1:max_k,
    wcss = wcss
  )

  # Generate the elbow plot
  plot <- ggplot(elbow_data, aes(x = k, y = wcss)) +
    geom_point(size = 3, color = "blue") +
    geom_line(size = 1, color = "blue") +
    labs(title = title, x = x_label, y = y_label) +
    theme_minimal(base_size = 15) +
    theme(plot.title = element_text(size = 20, face = "bold"))

  # Export the plot if requested
  if (export) {
    if (is.null(file_path)) {
      file_path <- paste0(file_name, ".tiff")
    }
    ggsave(file_path, plot, device = "tiff", dpi = 300)
  }

  return(plot)
}
