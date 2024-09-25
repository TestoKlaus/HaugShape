#' Create a scatterplot with convex hulls for specified groups.
#'
#' This function generates a scatterplot for two specified columns from a dataset, with optional convex hulls drawn
#' around one or more groups. The plot includes customizable point aesthetics (color, shape, fill, size), custom
#' tick marks, and labeled axes.
#'
#' @param data The data frame containing the variables to plot.
#' @param x_col The column name for the x-axis variable.
#' @param y_col The column name for the y-axis variable.
#' @param group_col (Optional) The column name for the grouping variable. Default is NULL.
#' @param group_vals (Optional) A vector of values from `group_col` to use for creating groups. Default is NULL.
#' @param hull_fill (Optional) Colors to fill the convex hulls for each group. Can be a vector. Default is "black".
#' @param hull_color (Optional) Colors for the borders of the convex hulls. Can be a vector. Default is "black".
#' @param hull_linetype (Optional) The line type for the hull borders. Can be "solid", "dashed", etc. Default is "solid".
#' @param hull_alpha (Optional) Opacity levels for the convex hulls. Can be a vector. Default is 0.1.
#' @param title (Optional) The title of the plot. Default is "Shape Plot".
#' @param x_label (Optional) Label for the x-axis. Default is NULL (uses column name).
#' @param y_label (Optional) Label for the y-axis. Default is NULL (uses column name).
#' @param point_color (Optional) Color of the scatter points. Can be a vector of colors for different groups. Default is "black".
#' @param point_fill (Optional) Fill color of the scatter points. Default is "white".
#' @param point_shape (Optional) Shape of the scatter points. Can be a vector for different groups. Default is 21 (circle).
#' @param point_size (Optional) Size of the scatter points. Can be a vector for different groups. Default is 2.
#' @param title_size (Optional) Font size for the plot title. Default is 24.
#' @param label_size (Optional) Font size for the axis labels. Default is 20.
#' @param tick_size (Optional) Font size for the tick marks. Default is 15.
#' @param tick_length (Optional) The proportional length of tick marks relative to plot size. Default is 0.005.
#' @param tick_margin (Optional) Margin around the ticks. Default is 0.05.
#' @param x_label_adjust (Optional) Custom adjustment for the x-axis label position. Default is 0.
#' @param y_label_adjust (Optional) Custom adjustment for the y-axis label position. Default is 0.
#' @param x_label_size (Optional) Font size for the x-axis label. Default is 5.
#' @param y_label_size (Optional) Font size for the y-axis label. Default is 5.
#' @param show_hulls (Optional) Boolean indicating whether to show convex hulls. Default is TRUE.
#' @param show_hull_for_groups (Optional) A vector specifying the groups for which hulls should be shown. Default is NULL (show hulls for all groups).
#'
#' @return A scatterplot with optional convex hulls, customized points, and axis labels.
#' @export
#'
#' @examples
#' # Example data
#' df <- data.frame(
#'   x = rnorm(100),
#'   y = rnorm(100),
#'   group = sample(c("A", "B", "C"), 100, replace = TRUE)
#' )
#'
#' # Plot with convex hulls for all groups
#' shape_plot(df, "x", "y", group_col = "group", group_vals = c("A", "B", "C"),
#'            hull_fill = c("red", "green", "blue"), hull_alpha = c(0.2, 0.3, 0.5),
#'            point_color = c("red", "green", "blue"), point_fill = "black", point_shape = 21,
#'            point_size = 4, title = "Example Plot", x_label = "PC1 (....%)", y_label = "PC2 (....%)")
#'
#' # Plot with no hulls
#' shape_plot(df, "x", "y", group_col = "group", group_vals = c("A", "B", "C"),
#'            hull_fill = c("red", "green", "blue"), hull_alpha = c(0.2, 0.3, 0.5),
#'            point_color = c("red", "green", "blue"), point_fill = "black", point_shape = 21,
#'            point_size = 4, title = "Example Plot", x_label = "PC1 (....%)", y_label = "PC2 (....%)",
#'            show_hulls = FALSE)
#'
#' # Plot with hulls for only groups A and B
#' shape_plot(df, "x", "y", group_col = "group", group_vals = c("A", "B", "C"),
#'            hull_fill = c("red", "green", "blue"), hull_alpha = c(0.2, 0.3, 0.5),
#'            point_color = c("red", "green", "blue"), point_fill = "black", point_shape = 21,
#'            point_size = 4, title = "Example Plot", x_label = "PC1 (....%)", y_label = "PC2 (....%)",
#'            show_hull_for_groups = c("A", "B"))


shape_plot <- function(data, x_col, y_col, group_col = NULL,
                       group_vals = NULL,  # Optional
                       hull_fill = "black", hull_color = "black", hull_linetype = "solid", hull_alpha = 0.1,
                       title = "Shape Plot", x_label = NULL, y_label = NULL,
                       point_color = "black", point_fill = "white", point_shape = 21, point_size = 2,  # Can be vectors
                       title_size = 24, label_size = 20, tick_size = 15,
                       tick_length = 0.005,  # Proportional custom tick length (relative to plot size)
                       tick_margin = 0.05,
                       x_label_adjust = 0,  # New parameter for x-axis label adjustment
                       y_label_adjust = 0,  # New parameter for y-axis label adjustment
                       x_label_size = 5,  # New parameter for x-axis label size
                       y_label_size = 5,  # New parameter for y-axis label size
                       show_hulls = TRUE,  # Show hulls globally
                       show_hull_for_groups = NULL) {  # New param to control hull display for specific groups

  # Convert column names to symbols for ggplot
  x <- rlang::sym(x_col)
  y <- rlang::sym(y_col)

  # If labels are not provided, use the column names as labels
  if (is.null(x_label)) x_label <- x_col
  if (is.null(y_label)) y_label <- y_col

  # Create the base plot without points first
  plot <- ggplot2::ggplot(data, ggplot2::aes(x = !!x, y = !!y))

  # Handle convex hulls if group_col and group_vals are provided
  if (!is.null(group_col) && show_hulls && !is.null(group_vals)) {
    if (length(group_vals) > 0) {
      # Adjust length of hull_fill, hull_color, hull_alpha to match the group_vals
      hull_fill <- rep_len(hull_fill, length(group_vals))
      hull_color <- rep_len(hull_color, length(group_vals))
      hull_alpha <- rep_len(hull_alpha, length(group_vals))

      for (i in seq_along(group_vals)) {
        group_val <- group_vals[i]
        # Check if the hull for the current group should be displayed
        if (is.null(show_hull_for_groups) || group_val %in% show_hull_for_groups) {
          # Filter the data for the specified group
          group_data <- data %>% dplyr::filter(!!rlang::sym(group_col) == group_val)

          if (nrow(group_data) > 2) {  # A hull requires at least 3 points
            hull_indices <- grDevices::chull(group_data[[x_col]], group_data[[y_col]])
            hull_data <- group_data[hull_indices, ]

            plot <- plot +
              ggplot2::geom_polygon(data = hull_data,
                                    ggplot2::aes_string(x = x_col, y = y_col),
                                    fill = hull_fill[i],
                                    color = hull_color[i],
                                    linetype = hull_linetype,
                                    alpha = hull_alpha[i])
          }
        }
      }
    }
  }

  # Add points for each group separately to ensure correct color and style mapping
  if (!is.null(group_col) && !is.null(group_vals)) {
    point_color <- rep_len(point_color, length(group_vals))
    point_fill <- rep_len(point_fill, length(group_vals))
    point_shape <- rep_len(point_shape, length(group_vals))
    point_size <- rep_len(point_size, length(group_vals))

    for (i in seq_along(group_vals)) {
      group_val <- group_vals[i]
      group_data <- data %>% dplyr::filter(!!rlang::sym(group_col) == group_val)

      # Add the points for the group with the respective aesthetics
      plot <- plot +
        ggplot2::geom_point(data = group_data,
                            ggplot2::aes_string(x = x_col, y = y_col),
                            color = point_color[i],
                            fill = point_fill[i],
                            shape = point_shape[i],
                            size = point_size[i])
    }
  } else {
    # If no groups, just add points without any specific group aesthetic
    plot <- plot +
      ggplot2::geom_point(ggplot2::aes_string(x = x_col, y = y_col),
                          color = point_color,
                          fill = point_fill,
                          shape = point_shape,
                          size = point_size)
  }

  # Get the range of x and y axes
  x_range <- range(data[[x_col]], na.rm = TRUE)
  y_range <- range(data[[y_col]], na.rm = TRUE)

  # Expand the range slightly to prevent points from being cut off
  x_expand <- 0.05 * (x_range[2] - x_range[1])  # 5% padding on x-axis
  y_expand <- 0.05 * (y_range[2] - y_range[1])  # 5% padding on y-axis

  # Generate pretty breaks, excluding the outermost ticks and zero
  x_ticks <- pretty(x_range)
  y_ticks <- pretty(y_range)
  x_ticks <- x_ticks[x_ticks != 0 & x_ticks > x_range[1] & x_ticks < x_range[2]]  # Remove 0 and outermost x-ticks
  y_ticks <- y_ticks[y_ticks != 0 & y_ticks > y_range[1] & y_ticks < y_range[2]]  # Remove 0 and outermost y-ticks

  # Calculate proportional tick lengths based on a fixed scale
  x_tick_length <- tick_length * diff(y_range)  # Consistent tick length on y-scale
  y_tick_length <- tick_length * diff(x_range)  # Consistent tick length on x-scale

  # Customizing the axes to cross at (0,0) with proportional ticks and labels
  plot <- plot +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),  # Remove grid lines
      axis.line = ggplot2::element_blank(),  # Hide default axis lines
      axis.ticks = ggplot2::element_blank(),  # Remove default ticks
      axis.text = ggplot2::element_blank(),   # Remove default tick labels
      axis.title.x = ggplot2::element_blank(),  # Hide default x-axis label
      axis.title.y = ggplot2::element_blank(),  # Hide default y-axis label
      plot.title = ggplot2::element_text(size = title_size)  # Center the title
    )

  # Add arrowed x-axis
  plot <- plot +
    ggplot2::geom_segment(aes(x = x_range[1] - x_expand, xend = x_range[2] + x_expand,
                              y = 0, yend = 0),
                          arrow = grid::arrow(length = grid::unit(0.3, "cm")), color = "black")

  # Add arrowed y-axis
  plot <- plot +
    ggplot2::geom_segment(aes(y = y_range[1] - y_expand, yend = y_range[2] + y_expand,
                              x = 0, xend = 0),
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

  # Adjust position of tick labels for x-axis based on tick_length
  plot <- plot +
    ggplot2::geom_text(data = data.frame(x = x_ticks, y = 0),
                       ggplot2::aes(x = x, y = y, label = x),
                       vjust = 1.5 + tick_length * 50,  # Adjust tick label position based on tick_length
                       size = tick_size / 3)  # Position tick labels below the x-axis and adjust size

  # Adjust position of tick labels for y-axis based on tick_length
  plot <- plot +
    ggplot2::geom_text(data = data.frame(x = 0, y = y_ticks),
                       ggplot2::aes(x = x, y = y, label = y),
                       hjust = 1.5 + tick_length * 50,  # Adjust tick label position based on tick_length
                       size = tick_size / 3)  # Position tick labels to the right of the y-axis and adjust size

  # Add titles and axis labels using labs (after all other layers)
  plot <- plot + ggplot2::labs(title = title)

  # Adjust x-axis label position (far right under the arrow) with custom adjustment and size
  plot <- plot +
    ggplot2::annotate("text",
                      x = max(x_range) - (0.05 * max(x_range)) + x_expand * 0.8 + x_label_adjust,  # Add custom adjustment
                      y = -0.05 * diff(y_range),
                      label = x_label, size = x_label_size, hjust = 0)  # Use custom size

  # Adjust y-axis label position (top, rotated, closer to axis) with custom adjustment and size
  plot <- plot +
    ggplot2::annotate("text",
                      x = -0.02 * diff(x_range),
                      y = max(y_range) + y_label_adjust,  # Add custom adjustment
                      label = y_label, size = y_label_size, vjust = 0, angle = 90)  # Use custom size

  return(plot)
}
