#' Cluster Plot with Automatic Clustering
#'
#' This function performs clustering on `(x, y)` points and visualizes the resulting clusters
#' with various clustering methods, including k-means, hierarchical clustering, and automatic
#' methods like DBSCAN or distance-threshold-based clustering. Shapes can be placed at the
#' cluster centers or at convex hull points.
#'
#' @param data A data frame containing the points to cluster.
#' @param x_col A character string specifying the column name for the x-axis.
#' @param y_col A character string specifying the column name for the y-axis.
#' @param shape_col (Optional) A column containing shape data as `Momocs::Out` objects for visualization at cluster centers or hull points.
#' @param method A character string specifying the clustering method: "kmeans", "hierarchical", "dbscan", or "distance_threshold".
#' @param k An integer specifying the number of clusters for k-means or hierarchical clustering. Ignored for "dbscan" and "distance_threshold".
#' @param scale Logical. Whether to scale the `(x, y)` points before clustering. Default is TRUE.
#' @param distance_threshold Numeric. The maximum distance between points in hierarchical clustering to form a cluster. Only used if `method = "distance_threshold"`. Default is NULL.
#' @param circle_size A numeric value specifying the size of the circles or convex hulls around clusters. Default is 1.
#' @param show_shapes Logical. Whether to show shapes for each cluster at the cluster centers or hull points. Default is TRUE.
#' @param shape_size A numeric value specifying the size of the shapes. Default is 0.01.
#' @param point_size A numeric value specifying the size of points in the scatter plot. Default is 2.
#' @param point_shape An integer specifying the shape of points in the scatter plot. Default is 21 (circle).
#' @param hull_alpha A numeric value between 0 and 1 specifying the transparency of the convex hulls. Default is 0.2.
#' @param hull_color A character string or vector specifying the color(s) of the convex hull borders. Default is "black".
#' @param hull_fill A character string or vector specifying the fill color(s) of the convex hulls. Default is automatically generated.
#' @param title A character string specifying the title of the plot. Default is NULL.
#' @param x_label A character string specifying the label for the x-axis. Defaults to the name of `x_col`.
#' @param y_label A character string specifying the label for the y-axis. Defaults to the name of `y_col`.
#' @param label_size A numeric value specifying the font size for axis labels. Default is 20.
#' @param title_size A numeric value specifying the font size for the plot title. Default is 24.
#' @param tick_size A numeric value specifying the font size for axis tick labels. Default is 15.
#' @param export Logical. Whether to export the plot as a TIFF file. Default is FALSE.
#' @param file_name A character string specifying the file name if `export = TRUE`. Default is "cluster_plot_output".
#' @param file_path An optional path for saving the exported file. If NULL, the file is saved in the working directory.
#' @param ... Additional parameters passed to `shape_plot` for further customization.
#'
#' @return A ggplot2 object visualizing the clusters and optional shapes.
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom stats kmeans hclust dist cutree
#' @importFrom grDevices chull
#'
#' @export
cluster_plot <- function(data, x_col, y_col, shape_col = "shape",
                         method = "kmeans", k = 3, scale = TRUE,
                         distance_threshold = NULL,
                         show_circles = FALSE, circle_size = 1,
                         show_ellipses = FALSE, ellipse_level = 0.95,
                         show_shapes = TRUE, shape_size = 0.01,
                         point_size = 2, point_shape = 21,
                         hull_alpha = 0.2, hull_color = "black", hull_fill = NULL,
                         title = NULL, x_label = NULL, y_label = NULL,
                         label_size = 20, title_size = 24, tick_size = 15,
                         export = FALSE, file_name = "cluster_plot_output", file_path = NULL,
                         shape_x_adjust = 0, shape_y_adjust = 0, shape_rotation = 0, # Shape-related parameters
                         ...) {
  if (!x_col %in% colnames(data) || !y_col %in% colnames(data)) {
    stop("Both `x_col` and `y_col` must exist in the data frame.")
  }

  # Extract coordinates for clustering
  coordinates <- data[, c(x_col, y_col)]

  # Optionally scale the coordinates
  if (scale) {
    coordinates <- scale(coordinates)
  }

  # Perform clustering
  clusters <- NULL
  if (method == "kmeans") {
    clustering <- kmeans(coordinates, centers = k)
    clusters <- clustering$cluster
  } else if (method == "hierarchical") {
    distance_matrix <- dist(coordinates)
    hclust_obj <- hclust(distance_matrix)
    clusters <- cutree(hclust_obj, k = k)
  } else if (method == "distance_threshold") {
    if (is.null(distance_threshold)) {
      stop("Please provide a `distance_threshold` for the 'distance_threshold' method.")
    }
    distance_matrix <- dist(coordinates)
    hclust_obj <- hclust(distance_matrix)
    clusters <- cutree(hclust_obj, h = distance_threshold)
  } else {
    stop("Invalid `method`. Choose 'kmeans', 'hierarchical', or 'distance_threshold'.")
  }

  # Add clusters to the data frame
  data$cluster <- factor(clusters)

  # Calculate cluster centers
  cluster_centers <- data %>%
    filter(cluster != 0) %>% # Exclude noise points if any
    group_by(cluster) %>%
    summarize(
      center_x = mean(.data[[x_col]], na.rm = TRUE),
      center_y = mean(.data[[y_col]], na.rm = TRUE),
      .groups = "drop"
    )

  # Pass augmented data to shape_plot
  plot <- shape_plot(
    data = data,
    x_col = x_col,
    y_col = y_col,
    group_col = "cluster",
    hull_fill = if (is.null(hull_fill)) scales::hue_pal()(length(unique(clusters[clusters > 0]))) else hull_fill,
    hull_alpha = hull_alpha,
    hull_color = hull_color,
    title = title,
    x_label = ifelse(is.null(x_label), x_col, x_label),
    y_label = ifelse(is.null(y_label), y_col, y_label),
    point_size = point_size,
    point_shape = point_shape,
    title_size = title_size,
    label_size = label_size,
    tick_size = tick_size,
    shape_x_adjust = shape_x_adjust,
    shape_y_adjust = shape_y_adjust,
    ...
  )

  # Add circles if draw_circles is TRUE
  if (show_circles) {
    plot <- plot +
      ggplot2::geom_point(
        data = cluster_centers,
        aes(x = center_x, y = center_y),
        size = circle_size * 10,  # Scale the size to match plot aesthetics
        shape = 1,               # Circle shape (open circle)
        color = "black",
        alpha = 0.5
      )
  }

  # Add ellipses if show_ellipses is TRUE
  if (show_ellipses) {
    requireNamespace("ggforce", quietly = TRUE)

    # Dynamically generate colors if hull_fill is NULL
    if (is.null(hull_fill)) {
      hull_fill <- scales::hue_pal()(length(unique(data$cluster)))
    }

    # Compute covariance and add ellipses for each cluster
    ellipses <- lapply(unique(data$cluster), function(cluster) {
      cluster_data <- data %>% filter(cluster == !!cluster)
      if (nrow(cluster_data) > 2) {
        cov_matrix <- cov(cluster_data[, c(x_col, y_col)], use = "complete.obs")
        center <- colMeans(cluster_data[, c(x_col, y_col)], na.rm = TRUE)

        # Eigen decomposition of the covariance matrix
        eigen_decomp <- eigen(cov_matrix)
        eigen_values <- sqrt(eigen_decomp$values)  # Standard deviations along principal axes
        eigen_vectors <- eigen_decomp$vectors

        # Generate ellipse points
        theta <- seq(0, 2 * pi, length.out = 100)
        ellipse_points <- cbind(
          eigen_values[1] * cos(theta),
          eigen_values[2] * sin(theta)
        ) %*% t(eigen_vectors) + center

        ellipse_df <- as.data.frame(ellipse_points)
        colnames(ellipse_df) <- c("x", "y")
        ellipse_df$cluster <- cluster
        return(ellipse_df)
      }
      return(NULL)
    })

    ellipses <- do.call(rbind, ellipses)

    plot <- plot +
      ggplot2::geom_polygon(
        data = ellipses,
        aes(x = x, y = y, group = cluster, fill = as.factor(cluster)),
        alpha = 0.2,
        color = "black"
      ) +
      ggplot2::scale_fill_manual(values = hull_fill)

  }

  # Add shapes to cluster centers if show_shapes is TRUE
  if (show_shapes && !is.null(shape_col) && shape_col %in% colnames(data)) {
    shapes <- lapply(seq_along(unique(clusters[clusters > 0])), function(cluster) {
      cluster_shape <- cluster_centers[cluster, ]
      if (!is.null(cluster_shape)) {
        shape_data <- data[data$cluster == cluster, shape_col, drop = TRUE]
        if (!is.null(shape_data[[1]]) && inherits(shape_data[[1]], "Out")) {
          # Extract coordinates directly from the Momocs::Out object
          shape_coordinates <- shape_data[[1]]$coo[[1]]
          shape_scaled <- shape_coordinates * shape_size
          shape_translated <- data.frame(
            x = shape_scaled[, 1] + cluster_shape$center_x + shape_x_adjust,
            y = shape_scaled[, 2] + cluster_shape$center_y + shape_y_adjust
          )
          return(shape_translated)
        } else {
          warning(paste("Shape data for cluster", cluster, "is not a valid Momocs::Out object or is missing."))
          return(NULL)
        }
      }
      return(NULL)
    })

    for (shape in shapes) {
      if (!is.null(shape)) {
        plot <- plot + ggplot2::geom_polygon(data = shape,
                                             ggplot2::aes(x = x, y = y),
                                             fill = "black", color = "black", alpha = 1)
      }
    }
  }

  # Export the plot if requested
  if (export) {
    if (is.null(file_path)) {
      file_path <- paste0(file_name, ".tiff")
    }
    ggplot2::ggsave(file_path, plot, device = "tiff", dpi = 300)
  }

  return(plot)
}
