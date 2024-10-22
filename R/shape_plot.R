#' Generate a Customizable Scatter Plot with Hulls, Heatmaps, and Contours
#'
#' The `shape_plot` function generates a scatter plot of specified columns with optional grouping. It supports various features such as convex hulls, heatmaps, and contour plots. The function provides extensive customization options for axis labels, colors, shapes, and plot aesthetics.
#'
#' @param data A data frame containing the data to be plotted.
#' @param x_col A character string representing the name of the column to be plotted on the x-axis.
#' @param y_col A character string representing the name of the column to be plotted on the y-axis.
#' @param group_col (Optional) A character string representing the name of the grouping column for color and style customization.
#' @param group_vals (Optional) A vector specifying the group values to display. Only these groups will appear in the plot.
#' @param hull_fill A vector specifying the fill color(s) for convex hull(s). Default is "black".
#' @param hull_color A vector specifying the border color(s) for convex hull(s). Default is "black".
#' @param hull_linetype A character string representing the line type for convex hull(s). Default is "solid".
#' @param hull_alpha A numeric value between 0 and 1 specifying the transparency for convex hull(s). Default is 0.1.
#' @param title A character string specifying the title of the plot. Default is NULL.
#' @param x_label A character string specifying the label for the x-axis. Defaults to the name of `x_col` if not provided.
#' @param y_label A character string specifying the label for the y-axis. Defaults to the name of `y_col` if not provided.
#' @param point_color A character string or vector specifying the color(s) of the points. Default is "black".
#' @param point_fill A character string or vector specifying the fill color(s) of the points. Default is "white".
#' @param point_shape An integer or vector specifying the shape(s) of the points. Default is 21 (circle).
#' @param point_size A numeric value or vector specifying the size(s) of the points. Default is 2.
#' @param title_size A numeric value specifying the font size for the plot title. Default is 24.
#' @param label_size A numeric value specifying the font size for the axis labels. Default is 20.
#' @param tick_size A numeric value specifying the font size for axis tick labels. Default is 15.
#' @param tick_length A numeric value specifying the proportional length of tick marks relative to plot size. Default is 0.005.
#' @param tick_margin A numeric value specifying the margin around the plot. Default is 0.05.
#' @param x_label_adjust_x A numeric value for horizontal adjustment of the x-axis label. Default is 0.
#' @param x_label_adjust_y A numeric value for vertical adjustment of the x-axis label. Default is 0.
#' @param y_label_adjust_x A numeric value for horizontal adjustment of the y-axis label. Default is 0.
#' @param y_label_adjust_y A numeric value for vertical adjustment of the y-axis label. Default is 0.
#' @param x_label_size A numeric value specifying the font size of the x-axis label. Default is 5.
#' @param y_label_size A numeric value specifying the font size of the y-axis label. Default is 5.
#' @param show_hulls A logical value indicating whether to display convex hulls around the groups. Default is FALSE.
#' @param show_hull_for_groups (Optional) A vector specifying which groups to display convex hulls for.
#' @param show_heatmaps A logical value indicating whether to display heatmaps. Default is FALSE.
#' @param heatmap_colors A list of color gradients for the heatmaps, with each list element representing the gradient for a group.
#' @param heatmap_alpha A numeric value between 0 and 1 specifying the transparency of heatmaps. Default is 1.
#' @param heatmap_bins A numeric value specifying the number of bins for the heatmap density estimation. Default is 30.
#' @param show_heatmap_for_groups (Optional) A vector specifying which groups to display heatmaps for.
#' @param show_contours A logical value indicating whether to display contour lines over heatmaps. Default is FALSE.
#' @param contour_colors A vector specifying the color(s) of contour lines. Default is "black".
#' @param show_contours_for_groups (Optional) A vector specifying which groups to display contour lines for.
#' @param contour_linewidth A numeric value specifying the thickness of contour lines. Default is 0.5.
#' @param axis_linewidth A numeric value specifying the width of the axis lines and ticks. Default is 1.
#' @param plot_style A character string specifying the style of the plot. Options include "Haug", "inverted_Haug", and "publication". Default is "Haug".
#' @param rotate_y_label A logical value indicating whether to rotate the y-axis label. Default is TRUE.
#' @param show_label_text_fields A logical value indicating whether to display black borders around axis labels. Default is TRUE.
#'
#' @return A ggplot2 object representing the generated scatter plot.
#'
#' @examples
#' # Create a data frame
#'df <- data.frame(
#'PC1 = rnorm(100),
#'PC2 = rnorm(100),
#'group = sample(c("A", "B", "C"), 100, replace = TRUE)
#'
#' # Scatterplot with hulls around groups A and C
#'shape_plot(data = df, x_col = "PC1", y_col = "PC2", group_col = "group",
#'           x_label = "PC1 (...)", x_label_adjust_x =-0.1, x_label_adjust_y =-0.1,
#'           y_label = "PC2 (...)", y_label_adjust_y = 0.4, rotate_y_label = FALSE,
#'           show_label_text_fields = FALSE,
#'           axis_linewidth = 1.5,
#'           title = "",
#'           group_vals = c("A","B","C"),
#'           point_fill = c("red","blue","green"),
#'           point_shape = c(21,8,21),
#'           point_color = c("red","blue","green"),
#'           show_hulls = TRUE,
#'           hull_fill = c("red","blue","green"),
#'           show_hull_for_groups = c("A","C"))
#' @export
#' @import ggplot2
#' @importFrom dplyr filter %>%
#' @importFrom rlang sym
#' @importFrom MASS kde2d
#' @importFrom grDevices chull
#' @importFrom scales rescale gradient_n_pal
#' @importFrom grid unit arrow


shape_plot <- function(data, x_col, y_col, group_col = NULL,
                       group_vals = NULL,  # Optional
                       hull_fill = "black", hull_color = "black", hull_linetype = "solid", hull_alpha = 0.1,
                       title = NULL, x_label = NULL, y_label = NULL,
                       point_color = "black", point_fill = "white", point_shape = 21, point_size = 2,  # Can be vectors
                       title_size = 24, label_size = 20, tick_size = 15,
                       tick_length = 0.005,  # Proportional custom tick length (relative to plot size)
                       tick_margin = 0.05,
                       x_label_adjust_x = 0,  # New horizontal adjustment for x-axis label
                       x_label_adjust_y = 0,  # New vertical adjustment for x-axis label
                       y_label_adjust_x = 0,  # New parameter for horizontal adjustment of y-axis label
                       y_label_adjust_y = 0,  # New parameter for vertical adjustment of y-axis label
                       x_label_size = 5,  # New parameter for x-axis label size
                       y_label_size = 5,  # New parameter for y-axis label size
                       show_hulls = FALSE,  # Default is now FALSE
                       show_hull_for_groups = NULL,  # Show hulls only for specified groups
                       show_heatmaps = FALSE,  # New parameter to control heatmap display
                       heatmap_colors = list(c("white", "red"), c("white", "blue"),c("white","green")),  # Custom colors for heatmaps
                       heatmap_alpha = 1, heatmap_bins = 30,
                       show_heatmap_for_groups = NULL,  # New param to control heatmap display for specific groups
                       show_contours = FALSE,  # Renamed parameter for adding heatmap contours
                       contour_colors = "black",  # Color for contour lines
                       show_contours_for_groups = NULL,  # New param to selectively show contours
                       contour_linewidth = 0.5,  # New param to adjust contour linewidth
                       axis_linewidth = 1,  # New param to adjust axis and tick linewidth together
                       plot_style = "Haug",
                       rotate_y_label = TRUE,  # New param to rotate the y-axis label and rectangle
                       show_label_text_fields = TRUE  # New param to toggle label text fields
) {
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
    stop("The column", y_col, "does not exist in the data. Please provide a valid y-axis column.")
  }

  # Validate that group_vals corresponds to the values in group_col (if provided)
  if (!is.null(group_vals) && !all(group_vals %in% unique(data[[group_col]]))) {
    stop("Some values in 'group_vals' do not exist in the specified 'group_col'. Please check the group values.")
  }

  # Use column names as default labels if none are provided
  if (is.null(x_label)) {
    x_label <- x_col  # Default to the name of the x-axis column
  }
  if (is.null(y_label)) {
    y_label <- y_col  # Default to the name of the y-axis column
  }

  # Define style-specific parameters
  if (plot_style == "inverted_Haug") {
    background_color <- "black"
    plot_background_color <- "black"
    text_color <- "white"
    axis_color <- "white"
    text_field_fill <- "black"  # Inverted text field color
    text_field_color <- "white"  # Inverted text field text color
  } else if (plot_style == "publication") {
    background_color <- "white"
    plot_background_color <- "lightgrey"
    text_color <- "black"
    axis_color <- "black"
    text_field_fill <- "white"
    text_field_color <- "black"
  } else {
    background_color <- "white"
    plot_background_color <- "white"
    text_color <- "black"
    axis_color <- "black"
    text_field_fill <- "white"
    text_field_color <- "black"
  }

  # Convert column names to symbols for ggplot
  x <- rlang::sym(x_col)
  y <- rlang::sym(y_col)

  # Create the base plot without points first
  plot <- ggplot2::ggplot(data, ggplot2::aes(x = !!x, y = !!y))

  # Add the light grey rectangle for the "publication" style
  if (plot_style == "publication") {
    plot <- plot +
      ggplot2::geom_rect(aes(xmin = min(data[[x_col]]) + 0.02 * diff(range(data[[x_col]])),
                             xmax = max(data[[x_col]]) - 0.02 * diff(range(data[[x_col]])),
                             ymin = min(data[[y_col]]) + 0.02 * diff(range(data[[y_col]])),
                             ymax = max(data[[y_col]]) - 0.02 * diff(range(data[[y_col]]))),
                         fill = plot_background_color, color = NA)
  }

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

  # Add custom axis labels with optional black borders (like text fields)
  if (show_label_text_fields) {
    plot <- plot +
      ggplot2::annotate("label", x = max(x_range) + x_expand + x_label_adjust_x,
                        y = -0.05 * diff(y_range) + x_label_adjust_y,
                        label = x_label, size = x_label_size, label.padding = unit(0.3, "lines"),
                        color = text_field_color, fill = text_field_fill) +
      ggplot2::annotate("label", x = -0.02 * diff(x_range) - y_label_adjust_x, y = max(y_range) + y_expand + y_label_adjust_y,
                        label = y_label, size = y_label_size, label.padding = unit(0.3, "lines"),
                        color = text_field_color, fill = text_field_fill, angle = ifelse(rotate_y_label, 90, 0))
  } else {
    plot <- plot +
      ggplot2::annotate("text", x = max(x_range) + x_expand + x_label_adjust_x,
                        y = -0.05 * diff(y_range) + x_label_adjust_y,
                        label = x_label, size = x_label_size, color = text_color) +
      ggplot2::annotate("text", x = -0.02 * diff(x_range) - y_label_adjust_x, y = max(y_range) + y_expand + y_label_adjust_y,
                        label = y_label, size = y_label_size, color = text_color, angle = ifelse(rotate_y_label, 90, 0))
  }


  return(plot)
}
