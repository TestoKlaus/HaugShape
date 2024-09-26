#' Create a customizable scatterplot with convex hulls, heatmaps, and contours.
#'
#' This function generates a scatterplot for two specified columns from a dataset. It allows for the addition
#' of convex hulls around groups, heatmaps, and contours. The plot is fully customizable, including options for
#' adjusting axis labels, tick marks, hulls, heatmaps, and contour properties. It also supports two visual styles,
#' "Haug" and "inverted_Haug".
#'
#' @param data A data frame containing the variables to plot.
#' @param x_col The column name (string) for the x-axis variable.
#' @param y_col The column name (string) for the y-axis variable.
#' @param group_col (Optional) The column name (string) for grouping data into categories.
#' @param group_vals (Optional) A vector specifying the group values to be plotted. Must correspond to values in `group_col`.
#' @param hull_fill A color (or vector of colors) for filling the convex hulls. Default is "black".
#' @param hull_color A color for the borders of the convex hulls. Default is "black".
#' @param hull_linetype The line type for the hull borders (e.g., "solid", "dashed"). Default is "solid".
#' @param hull_alpha Opacity levels for the convex hulls. Can be a vector. Default is 0.1.
#' @param title The title of the plot. Default is "Shape Plot".
#' @param x_label Custom label for the x-axis. If NULL, the column name is used. Default is NULL.
#' @param y_label Custom label for the y-axis. If NULL, the column name is used. Default is NULL.
#' @param point_color A color (or vector of colors) for the scatter points. Default is "black".
#' @param point_fill A color for filling the scatter points. Default is "white".
#' @param point_shape The shape of the scatter points. Can be a vector. Default is 21 (circle).
#' @param point_size The size of the scatter points. Can be a vector. Default is 2.
#' @param title_size Font size for the plot title. Default is 24.
#' @param label_size Font size for the axis labels. Default is 20.
#' @param tick_size Font size for the tick marks. Default is 15.
#' @param tick_length The proportional length of tick marks relative to plot size. Default is 0.005.
#' @param tick_margin The margin around the ticks. Default is 0.05.
#' @param x_label_adjust Horizontal adjustment for the x-axis label. Default is 0.
#' @param y_label_adjust Vertical adjustment for the y-axis label. Default is 0.
#' @param x_label_size Font size for the x-axis label. Default is 5.
#' @param y_label_size Font size for the y-axis label. Default is 5.
#' @param show_hulls Logical. Should convex hulls be drawn around the groups? Default is FALSE.
#' @param show_hull_for_groups (Optional) A vector specifying the groups for which hulls should be shown. Default is NULL (shows hulls for all groups if `show_hulls` is TRUE).
#' @param show_heatmaps Logical. Should heatmaps be shown for the groups? Default is FALSE.
#' @param heatmap_colors A list of color gradients for the heatmaps. Default is list(c("white", "red"), c("white", "blue")).
#' @param heatmap_alpha The opacity level for the heatmaps. Default is 1.
#' @param heatmap_bins The number of bins for the heatmap's density estimation. Default is 30.
#' @param show_heatmap_for_groups (Optional) A vector specifying the groups for which heatmaps should be shown. Default is NULL (shows heatmaps for all groups if `show_heatmaps` is TRUE).
#' @param show_contours Logical. Should contour lines be drawn around the groups? Default is FALSE.
#' @param contour_colors A color (or vector of colors) for the contour lines. Default is "black".
#' @param show_contours_for_groups (Optional) A vector specifying the groups for which contours should be shown. Default is NULL (shows contours for all groups if `show_contours` is TRUE).
#' @param contour_linewidth The width of the contour lines. Default is 0.5.
#' @param axis_linewidth The width of the axes and tick marks. Default is 1.
#' @param plot_style A string specifying the plot style. "Haug" uses a white background with black text, while "inverted_Haug" uses a black background with white text. Default is "Haug".
#'
#' @return A `ggplot` object with the customized scatter plot.
#'
#' @examples
#' # Example data
#' df <- data.frame(
#'   x = rnorm(100),
#'   y = rnorm(100),
#'   group = sample(c("A", "B", "C"), 100, replace = TRUE)
#' )
#'
#' # Create a scatterplot with convex hulls for all groups
#' shape_plot(df, "x", "y", group_col = "group", group_vals = c("A", "B", "C"),
#'            show_hulls = TRUE, hull_fill = c("red", "green", "blue"), hull_alpha = 0.3)
#'
#' # Create a scatterplot with heatmaps for specific groups
#' shape_plot(df, "x", "y", group_col = "group", group_vals = c("A", "B"),
#'            show_heatmaps = TRUE, heatmap_colors = list(c("white", "red"), c("white", "green")))
#'
#' @export

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
                       show_hulls = FALSE,  # Default is now FALSE
                       show_hull_for_groups = NULL,  # Show hulls only for specified groups
                       show_heatmaps = FALSE,  # New parameter to control heatmap display
                       heatmap_colors = list(c("white", "red"), c("white", "blue")),  # Custom colors for heatmaps
                       heatmap_alpha = 1, heatmap_bins = 30,
                       show_heatmap_for_groups = NULL,  # New param to control heatmap display for specific groups
                       show_contours = FALSE,  # Renamed parameter for adding heatmap contours
                       contour_colors = "black",  # Color for contour lines
                       show_contours_for_groups = NULL,  # New param to selectively show contours
                       contour_linewidth = 0.5,  # New param to adjust contour linewidth
                       axis_linewidth = 1,  # New param to adjust axis and tick linewidth together
                       plot_style = "Haug") {
  if (missing(data) || missing(x_col) || missing(y_col)) {
    stop("Missing required arguments: 'data', 'x_col', or 'y_col'. Please provide these parameters.")
  }

  # Validate that group_col exists in the data if provided
  if (!is.null(group_col) && !group_col %in% colnames(data)) {
    stop(paste("The column", group_col, "does not exist in the data. Please provide a valid group column."))
  }

  # Validate that the x_col and y_col exist in the data
  if (!x_col %in% colnames(data)) {
    stop(paste("The column", x_col, "does not exist in the data. Please provide a valid x-axis column."))
  }
  if (!y_col %in% colnames(data)) {
    stop(paste("The column", y_col, "does not exist in the data. Please provide a valid y-axis column."))
  }

  # Validate that group_vals corresponds to the values in group_col (if provided)
  if (!is.null(group_vals) && !all(group_vals %in% unique(data[[group_col]]))) {
    stop("Some values in 'group_vals' do not exist in the specified 'group_col'. Please check the group values.")
  }

  # Check if the provided heatmap and contour settings are valid
  if (show_heatmaps && length(heatmap_colors) < length(group_vals)) {
    stop("The number of heatmap colors provided is less than the number of groups. Please provide enough colors for each group.")
  }

  if (show_contours && length(contour_colors) < length(group_vals)) {
    stop("The number of contour colors provided is less than the number of groups. Please provide enough colors for each group.")
  }
  # New param to toggle between different styles

  # Define style-specific parameters
  if (plot_style == "inverted_Haug") {
    # Inverted Haug: Black background, white text
    background_color <- "black"
    text_color <- "white"
    axis_color <- "white"
  } else {
    # Haug: Default style
    background_color <- "white"
    text_color <- "black"
    axis_color <- "black"
  }

  # Convert column names to symbols for ggplot
  x <- rlang::sym(x_col)
  y <- rlang::sym(y_col)

  # If labels are not provided, use the column names as labels
  if (is.null(x_label)) x_label <- x_col
  if (is.null(y_label)) y_label <- y_col

  # Create the base plot without points first
  plot <- ggplot2::ggplot(data, ggplot2::aes(x = !!x, y = !!y))

  # Customize the theme
  plot <- plot +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = background_color, color = background_color),
      plot.background = ggplot2::element_rect(fill = background_color, color = background_color)
    )

  # Handle heatmaps and contours if enabled and group_col and group_vals are provided
  if (!is.null(group_col) && !is.null(group_vals)) {
    # Ensure contour_colors has the same length as group_vals or is a single color
    if (length(contour_colors) == 1) {
      contour_colors <- rep(contour_colors, length(group_vals))
    }

    for (i in seq_along(group_vals)) {
      group_val <- group_vals[i]

      # Filter the data for the specified group
      group_data <- data %>% dplyr::filter(!!rlang::sym(group_col) == group_val)

      # Compute 2D density data for the group with expanded range and higher resolution for smoother contours
      x_range_expanded <- range(data[[x_col]]) + c(-0.05, 0.05) * diff(range(data[[x_col]]))
      y_range_expanded <- range(data[[y_col]]) + c(-0.05, 0.05) * diff(range(data[[y_col]]))
      kde <- MASS::kde2d(group_data[[x_col]], group_data[[y_col]], n = 100,  # Increased n for smoother lines
                         lims = c(x_range_expanded, y_range_expanded))
      kde_df <- data.frame(expand.grid(x = kde$x, y = kde$y), z = as.vector(kde$z))

      # Add 2D density heatmap for the group
      if (show_heatmaps && (is.null(show_heatmap_for_groups) || group_val %in% show_heatmap_for_groups)) {
        plot <- plot +
          ggplot2::geom_tile(data = kde_df, ggplot2::aes(x = x, y = y, fill = scales::rescale(z)),
                             alpha = heatmap_alpha, fill = scales::gradient_n_pal(heatmap_colors[[i]])(scales::rescale(kde_df$z)))
      }

      # Add contours for the group with adjustable linewidth
      if (show_contours && (is.null(show_contours_for_groups) || group_val %in% show_contours_for_groups)) {
        plot <- plot +
          ggplot2::geom_contour(data = kde_df %>% dplyr::filter(x >= x_range_expanded[1] & x <= x_range_expanded[2] &
                                                                  y >= y_range_expanded[1] & y <= y_range_expanded[2]),
                                ggplot2::aes(x = x, y = y, z = z), color = contour_colors[i], size = contour_linewidth)
      }
    }
  }

  # Handle convex hulls if enabled and group_col and group_vals are provided
  if (!show_heatmaps && !is.null(group_col) && show_hulls && !is.null(group_vals)) {
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

  # Customizing the axes and tick marks with adjustable linewidth
  plot <- plot +
    ggplot2::theme_minimal(base_family = "sans") +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),  # Remove grid lines
      axis.line = ggplot2::element_blank(),  # Hide default axis lines
      axis.ticks = ggplot2::element_blank(),  # Remove default ticks
      axis.text = ggplot2::element_blank(),   # Remove default tick labels
      axis.title.x = ggplot2::element_blank(),  # Hide default x-axis label
      axis.title.y = ggplot2::element_blank(),  # Hide default y-axis label
      plot.title = ggplot2::element_text(size = title_size, color = text_color),  # Center the title
      plot.background = ggplot2::element_rect(fill = background_color, color = background_color),  # Background
      plot.margin = ggplot2::margin(tick_margin, tick_margin, tick_margin, tick_margin)  # Margins
    )

  # Add arrowed x-axis with adjustable linewidth and axis color
  plot <- plot +
    ggplot2::geom_segment(aes(x = x_range[1] - x_expand, xend = x_range[2] + x_expand,
                              y = 0, yend = 0),
                          arrow = grid::arrow(length = grid::unit(0.3, "cm")), color = axis_color, size = axis_linewidth)

  # Add arrowed y-axis with adjustable linewidth and axis color
  plot <- plot +
    ggplot2::geom_segment(aes(y = y_range[1] - y_expand, yend = y_range[2] + y_expand,
                              x = 0, xend = 0),
                          arrow = grid::arrow(length = grid::unit(0.3, "cm")), color = axis_color, size = axis_linewidth)

  # Add custom tick marks on the x-axis with adjustable linewidth
  plot <- plot +
    ggplot2::geom_segment(data = data.frame(x = x_ticks),
                          ggplot2::aes(x = x, xend = x, y = -x_tick_length, yend = x_tick_length),
                          color = axis_color, size = axis_linewidth)

  # Add custom tick marks on the y-axis with adjustable linewidth
  plot <- plot +
    ggplot2::geom_segment(data = data.frame(y = y_ticks),
                          ggplot2::aes(y = y, yend = y, x = -y_tick_length, xend = y_tick_length),
                          color = axis_color, size = axis_linewidth)

  # Adjust position of tick labels for x-axis based on tick_length
  plot <- plot +
    ggplot2::geom_text(data = data.frame(x = x_ticks, y = 0),
                       ggplot2::aes(x = x, y = y, label = x),
                       vjust = 1.5 + tick_length * 50,  # Adjust tick label position based on tick_length
                       size = tick_size / 3, color = text_color)  # Position tick labels below the x-axis and adjust size

  # Adjust position of tick labels for y-axis based on tick_length
  plot <- plot +
    ggplot2::geom_text(data = data.frame(x = 0, y = y_ticks),
                       ggplot2::aes(x = x, y = y, label = y),
                       hjust = 1.5 + tick_length * 50,  # Adjust tick label position based on tick_length
                       size = tick_size / 3, color = text_color)  # Position tick labels to the right of the y-axis and adjust size

  # Add titles and axis labels using labs (after all other layers)
  plot <- plot + ggplot2::labs(title = title)

  # Adjust x-axis label position (far right under the arrow) with custom adjustment and size
  plot <- plot +
    ggplot2::annotate("text",
                      x = max(x_range) - (0.05 * max(x_range)) + x_expand * 0.8 + x_label_adjust,  # Add custom adjustment
                      y = -0.05 * diff(y_range),
                      label = x_label, size = x_label_size, hjust = 0, color = text_color)  # Use custom size

  # Adjust y-axis label position (top, rotated, closer to axis) with custom adjustment and size
  plot <- plot +
    ggplot2::annotate("text",
                      x = -0.02 * diff(x_range),
                      y = max(y_range) + y_label_adjust,  # Add custom adjustment
                      label = y_label, size = y_label_size, vjust = 0, angle = 90, color = text_color)  # Use custom size

  return(plot)
}
