#' Plot PCA results with shapes at convex hull points.
#'
#' This function generates a scatterplot from PCA results with shapes displayed at the convex hull points.
#' The shapes can be adjusted for size, position, and orientation, and the plot includes custom axis labels,
#' tick marks, and title adjustments. Aspect ratio is maintained to prevent shape distortion.
#'
#' @param pca_result The PCA result object from `Momocs::PCA()`.
#' @param coo_object The Coo object from which shapes are extracted.
#' @param pc_x (Optional) The principal component for the x-axis. Default is 1.
#' @param pc_y (Optional) The principal component for the y-axis. Default is 2.
#' @param shape_size (Optional) The scaling factor for the shapes. Default is 0.01.
#' @param shift_distance (Optional) The distance to shift the shapes from the plot center. Default is 0.
#' @param vertical_offset (Optional) The vertical offset applied to the shapes. Default is 0.
#' @param horizontal_offset (Optional) The horizontal offset applied to the shapes. Default is 0.
#' @param title (Optional) The title of the plot. Default is NULL (no title).
#' @param x_label (Optional) Label for the x-axis. Default is NULL (uses PC axis label).
#' @param y_label (Optional) Label for the y-axis. Default is NULL (uses PC axis label).
#' @param point_color (Optional) Color of the PCA scatter points. Default is "black".
#' @param point_size (Optional) Size of the PCA scatter points. Default is 2.
#' @param title_size (Optional) Font size for the plot title. Default is 24.
#' @param label_size (Optional) Font size for the axis labels. Default is 20.
#' @param tick_size (Optional) Font size for the tick labels. Default is 15.
#' @param tick_length (Optional) Length of the tick marks, relative to plot size. Default is 0.005.
#' @param arrow_shift (Optional) Amount to shift the arrowed axes to avoid overlap with tick marks. Default is 0.
#' @param x_label_adjust (Optional) Adjustment for the x-axis label position. Default is 0.
#' @param y_label_adjust (Optional) Adjustment for the y-axis label position. Default is 0.
#'
#' @return A scatterplot of PCA results with shapes displayed at the convex hull points.
#'
#'
#' @examples
#' # Load the Momocs package
#' library(Momocs)
#'
#' # Example using the 'bot' dataset (botanical leaves) from Momocs
#' bot_shapes <- bot %>% coo_center() %>% coo_scale() %>% coo_align()
#'
#' # Perform a Fourier transformation
#' bot_fourier <- bot_shapes %>% efourier(nb.h = 10)
#'
#' # Perform PCA on the Fourier coefficients
#' bot_pca <- PCA(bot_fourier)
#'
#' # Plot PCA results with shapes at convex hull points
#' shape_plot_momocs(bot_pca, bot_shapes,
#'                   pc_x = 1, pc_y = 2,  # PC axes for plotting
#'                   shape_size = 0.01, shift_distance = 0.1,
#'                   vertical_offset = 0, horizontal_offset = 0,
#'                   title = "PCA Shape Plot", x_label = "PC1", y_label = "PC2")
#' @export
#' @import ggplot2
#' @importFrom grDevices chull
#' @importFrom grid arrow unit

shape_plot_momocs <- function(pca_result, coo_object,
                              pc_x = 1, pc_y = 2,  # PC axes for plotting
                              shape_size = 0.01, shift_distance = 0, vertical_offset = 0,
                              horizontal_offset = 0,  # New parameter for horizontal adjustment
                              title = NULL, x_label = NULL, y_label = NULL,
                              point_color = "black", point_size = 2,
                              title_size = 24, label_size = 20, tick_size = 15,
                              tick_length = 0.005, arrow_shift = 0,  # Custom tick length and arrow shift
                              x_label_adjust = 0, y_label_adjust = 0) {  # New label adjustment parameters

  # Extract PCA scores for the plot
  pca_scores <- pca_result$x
  pca_data <- data.frame(PCx = pca_scores[, pc_x], PCy = pca_scores[, pc_y])

  # Create base scatterplot
  plot <- ggplot2::ggplot(pca_data, ggplot2::aes(x = PCx, y = PCy)) +
    ggplot2::geom_point(size = point_size, color = point_color) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),  # Remove grid lines
      axis.line = ggplot2::element_blank(),  # Hide default axis lines
      axis.ticks = ggplot2::element_blank(),  # Remove default ticks
      axis.text = ggplot2::element_blank(),   # Remove default tick labels
      axis.title.x = ggplot2::element_blank(),  # Hide default x-axis label
      axis.title.y = ggplot2::element_blank(),  # Hide default y-axis label
      plot.title = ggplot2::element_text(size = title_size)  # Center the title
    ) +
    ggplot2::coord_fixed()  # Maintain aspect ratio to prevent shape distortion

  # Calculate the convex hull of the PCA points
  hull_indices <- grDevices::chull(pca_data$PCx, pca_data$PCy)

  # Get the range of x and y axes
  x_range <- range(pca_data$PCx, na.rm = TRUE)
  y_range <- range(pca_data$PCy, na.rm = TRUE)

  # Expand the range slightly to prevent the arrow from overlapping the tick marks
  x_expand <- 0.05 * (x_range[2] - x_range[1])
  y_expand <- 0.05 * (y_range[2] - y_range[1])

  # Dynamically adjust the shift distance based on axis ranges
  shift_distance <- shift_distance * (diff(x_range) + diff(y_range)) / 2

  # Generate pretty breaks, excluding the outermost ticks and zero
  x_ticks <- pretty(x_range)
  y_ticks <- pretty(y_range)

  # Remove the outermost ticks and the zero tick
  x_ticks <- x_ticks[x_ticks != 0 & x_ticks > x_range[1] & x_ticks < x_range[2]]
  y_ticks <- y_ticks[y_ticks != 0 & y_ticks > y_range[1] & y_ticks < y_range[2]]

  # Calculate proportional tick lengths based on a fixed scale
  x_tick_length <- tick_length * diff(y_range)  # Consistent tick length on y-scale
  y_tick_length <- tick_length * diff(x_range)  # Consistent tick length on x-scale

  # Add arrowed x-axis, shifting slightly to avoid overlap
  plot <- plot +
    ggplot2::geom_segment(aes(x = x_range[1] - x_expand, xend = x_range[2] + x_expand + arrow_shift, y = 0, yend = 0),
                          arrow = grid::arrow(length = grid::unit(0.3, "cm")), color = "black") +
    ggplot2::geom_segment(aes(x = 0, xend = 0, y = y_range[1] - y_expand, yend = y_range[2] + y_expand + arrow_shift),
                          arrow = grid::arrow(length = grid::unit(0.3, "cm")), color = "black")

  # Add custom tick marks on the x-axis
  plot <- plot +
    ggplot2::geom_segment(data = data.frame(x = x_ticks),
                          ggplot2::aes(x = x, xend = x, y = -x_tick_length, yend = x_tick_length),
                          color = "black")

  # Add custom tick marks on the y-axis
  plot <- plot +
    ggplot2::geom_segment(data = data.frame(y = y_ticks),
                          ggplot2::aes(y = y, yend = y, x = -y_tick_length, xend = y_tick_length),
                          color = "black")

  # Add custom tick labels for the x-axis
  plot <- plot +
    ggplot2::geom_text(data = data.frame(x = x_ticks, y = 0),
                       ggplot2::aes(x = x, y = y, label = x),
                       vjust = 1.5 + tick_length * 50,  # Adjust tick label position based on tick_length
                       size = tick_size / 3)

  # Add custom tick labels for the y-axis
  plot <- plot +
    ggplot2::geom_text(data = data.frame(x = 0, y = y_ticks),
                       ggplot2::aes(x = x, y = y, label = y),
                       hjust = 1.5 + tick_length * 50,  # Adjust tick label position based on tick_length
                       size = tick_size / 3)

  # Add titles and axis labels using labs (after all other layers)
  plot <- plot + ggplot2::labs(title = title) +
    ggplot2::annotate("text",
                      x = max(x_range) - (0.05 * max(x_range)) + x_expand * 0.8 + x_label_adjust,  # Adjust x-axis label position
                      y = -0.05 * diff(y_range), label = x_label,
                      size = 5, hjust = 0) +
    ggplot2::annotate("text",
                      x = -0.02 * diff(x_range),
                      y = max(y_range) + y_label_adjust,  # Adjust y-axis label position
                      label = y_label, size = 5, vjust = 0, angle = 90)

  # Loop through the shapes at the convex hull points
  for (i in seq_along(hull_indices)) {
    shape_index <- hull_indices[i]

    # Extract the shape from the Coo object
    shape_data <- coo_object$coo[[shape_index]]

    # Scale the shape
    shape_scaled <- shape_data * shape_size

    # Determine the position of the convex hull point
    x_pos <- pca_data$PCx[shape_index]
    y_pos <- pca_data$PCy[shape_index]

    # Calculate the direction vector from the center to the point
    direction_x <- x_pos - mean(pca_data$PCx)
    direction_y <- y_pos - mean(pca_data$PCy)

    # Normalize the direction vector
    magnitude <- sqrt(direction_x^2 + direction_y^2)
    direction_x <- direction_x / magnitude
    direction_y <- direction_y / magnitude

    # Apply a fixed shift distance
    direction_x <- direction_x * shift_distance
    direction_y <- direction_y * shift_distance

    # Apply the vertical and horizontal offsets to all shapes
    direction_x <- direction_x + horizontal_offset
    direction_y <- direction_y + vertical_offset

    # Translate the shape to its new position (shifted away from the center and adjusted)
    shape_translated <- data.frame(x = shape_scaled[, 1] + x_pos + direction_x,
                                   y = shape_scaled[, 2] + y_pos + direction_y)

    # Add the shape to the plot as a filled polygon
    plot <- plot + ggplot2::geom_polygon(data = shape_translated,
                                         ggplot2::aes(x = x, y = y),
                                         fill = "black", color = "black", size = 0.5)
  }

  return(plot)
}
