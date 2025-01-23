#' Generate a Customizable Scatter Plot with Hulls and Contours
#'
#' The `shape_plot` function generates a scatter plot of specified columns with optional grouping.
#' It supports various features such as convex hulls, contour plots, and shapes at convex hull points.
#' The function provides extensive customization options for axis labels, colors, shapes, and plot aesthetics.
#'
#' @param data (required) A data frame containing the data to be plotted.
#' @param x_col (required) A character string or bare column name representing the x-axis column. Use either `"column_name"` or `column_name`.
#' @param y_col (required) A character string or bare column name representing the y-axis column. Use either `"column_name"` or `column_name`.
#' @param group_col (required) A character string or bare column name representing the grouping column for color and style customization. Use either `"column_name"` or `column_name`.
#' @param group_vals A vector specifying which group values to display. Only these groups will appear in the plot.
#' @param hull_fill A vector specifying the fill color(s) for convex hull(s). Defaults to `point_color` if not specified.
#' @param hull_color A vector specifying the border color(s) for convex hull(s). Default is "black".
#' @param hull_linetype A character string representing the line type for convex hull(s). Default is "solid".
#' @param hull_alpha A numeric value between 0 and 1 specifying the transparency for convex hull(s). Default is 0.1.
#' @param title A character string specifying the title of the plot. Default is NULL.
#' @param x_label A character string specifying the label for the x-axis. Defaults to the name of `x_col` if not provided.
#' @param y_label A character string specifying the label for the y-axis. Defaults to the name of `y_col` if not provided.
#' @param point_color A character string or vector specifying the color(s) of the points. Default is generated based on `group_vals` if available.
#' @param point_fill A character string or vector specifying the fill color(s) of the points. Defaults to `point_color` if not specified.
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
#' @param show_shapes A logical value indicating whether to display shapes at convex hull points. Default is FALSE.
#' @param show_shapes_for_groups (Optional) A vector specifying which groups to display shapes for.
#' @param shape_col A character string specifying the column containing shape objects. Default is "shape".
#' @param shape_size A numeric value specifying the scaling factor for shapes. Default is 0.01.
#' @param shape_shift A numeric value specifying the distance to shift shapes away from the center of the plot. Default is 0.1.
#' @param shape_x_adjust A numeric value specifying manual adjustment for the x-coordinate of the shapes. Default is 0.
#' @param shape_y_adjust A numeric value specifying manual adjustment for the y-coordinate of the shapes. Default is 0.
#' @param show_contours A logical value indicating whether to display contour lines. Default is FALSE.
#' @param contour_colors A vector specifying the color(s) of contour lines. Default is "black".
#' @param show_contours_for_groups (Optional) A vector specifying which groups to display contour lines for.
#' @param contour_linewidth A numeric value specifying the thickness of contour lines. Default is 0.5.
#' @param axis_linewidth A numeric value specifying the width of the axis lines and ticks. Default is 1.
#' @param plot_style A character string specifying the style of the plot. Options include "Haug", "inverted_Haug", and "publication". Default is "Haug".
#' @param rotate_y_label A logical value indicating whether to rotate the y-axis label. Default is FALSE.
#' @param show_label_text_fields A logical value indicating whether to display black borders around axis labels. Default is TRUE.
#' @param export A logical value indicating whether to export the plot as a TIFF file. Default is FALSE.
#' @param file_name A character string specifying the name of the file if `export = TRUE`. Default is "shape_plot_output".
#' @param file_path An optional path for saving the exported file. If NULL, the file is saved in the working directory.
#'
#' @return A ggplot2 object representing the generated scatter plot.
#'
#'
#' @examples
#' test_data <- data.frame(
#'   PC1 = rnorm(100),
#'   PC2 = rnorm(100),
#'   group = sample(c("A", "B", "C"), 100, replace = TRUE)
#' )
#' shape_plot(data = test_data, x_col = "PC1", y_col = "PC2", group_col = "group",
#'            show_hulls = TRUE)
#'
#' @export
#' @import ggplot2
#' @import dplyr
#' @importFrom rlang sym
#' @importFrom scales rescale gradient_n_pal
#' @importFrom MASS kde2d
#' @importFrom grDevices chull
#' @importFrom grid arrow unit

shape_plot <- function(data, x_col, y_col, group_col = NULL,
                       group_vals = NULL,  # Optional
                       hull_fill = NULL, hull_color = "black", hull_linetype = "solid", hull_alpha = 0.1,
                       title = NULL, x_label = NULL, y_label = NULL,
                       point_color = NULL, point_fill = NULL, point_shape = 21, point_size = 2,  # Can be vectors
                       title_size = 24, label_size = 20, tick_size = 15,
                       tick_length = 0.005,  # Fixed tick length (same for all ticks)
                       tick_margin = 0.05,
                       x_label_adjust_x = 0,  # New horizontal adjustment for x-axis label
                       x_label_adjust_y = 0,  # New vertical adjustment for x-axis label
                       y_label_adjust_x = 0,  # New parameter for horizontal adjustment of y-axis label
                       y_label_adjust_y = 0,  # New parameter for vertical adjustment of y-axis label
                       x_label_size = 5,  # New parameter for x-axis label size
                       y_label_size = 5,  # New parameter for y-axis label size
                       show_hulls = FALSE,  # Default is now FALSE
                       show_shapes = FALSE,  # Default is now FALSE
                       show_hull_for_groups = NULL,  # Show hulls only for specified groups
                       show_shapes_for_groups = NULL,  # New param for selectively showing shapes
                       show_contours = FALSE,  # Renamed parameter for adding contours
                       contour_colors = "black",  # Color for contour lines
                       show_contours_for_groups = NULL,  # New param to selectively show contours
                       contour_linewidth = 0.5,  # New param to adjust contour linewidth
                       axis_linewidth = 1,  # New param to adjust axis and tick linewidth together
                       plot_style = "Haug",
                       rotate_y_label = FALSE,  # Default rotation for y-axis label is now FALSE
                       show_label_text_fields = TRUE,  # Default text fields around axis labels is already TRUE
                       export = FALSE,  # New parameter for exporting plot
                       file_name = "shape_plot_output",  # Default file name for export
                       file_path = NULL,  # Optional path for file export
                       shape_col = "shape", shape_size = 0.01, shape_shift = 0.1, # Shape-related parameters
                       shape_x_adjust = 0, shape_y_adjust = 0 # Shape-related parameters
) {

  if (missing(data) || missing(x_col) || missing(y_col)) {
    stop("Missing required arguments: 'data', 'x_col', or 'y_col'. Please provide these parameters.")
  }

  # Validate `shape_col` only if `show_shapes` is TRUE
  if (show_shapes && !shape_col %in% colnames(data)) {
    stop(paste("The column", shape_col, "does not exist in the data. Please provide a valid shape column."))
  }

  # Dynamically filter out NA and non-finite values
  valid_data <- data %>%
    dplyr::filter(
      !is.na(.data[[x_col]]) &
        !is.na(.data[[y_col]]) &
        (is.null(group_col) | !is.na(.data[[group_col]])) &
        is.finite(.data[[x_col]]) &
        is.finite(.data[[y_col]])
    )
  message(paste("Filtered data: ", nrow(data) - nrow(valid_data), " rows removed due to missing or non-finite values."))

  # Dynamically filter out NA values based on `show_shapes`
  if (show_shapes) {
    valid_data <- data %>%
      dplyr::filter(
        !is.na(.data[[x_col]]) &
          !is.na(.data[[y_col]]) &
          (is.null(group_col) | !is.na(.data[[group_col]])) &
          !is.na(.data[[shape_col]])
      )
  } else {
    valid_data <- data %>%
      dplyr::filter(
        !is.na(.data[[x_col]]) &
          !is.na(.data[[y_col]]) &
          (is.null(group_col) | !is.na(.data[[group_col]]))
      )
  }

  # Ensure export uses the same settings as the app
  if (export) {
    # Keep user-defined settings for tick_size, axis_linewidth, etc.
    message("Exporting plot with user-defined settings.")
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
  if (!is.null(group_vals) && !is.vector(group_vals)) {
    group_vals <- as.vector(group_vals)
  }

  if (!all(group_vals %in% unique(data[[group_col]]))) {
    stop("Some values in 'group_vals' do not exist in the specified 'group_col'. Please check the group values.")
  }

  # Automatically assign colors if point_color or point_fill is not provided
  if (is.null(group_vals) && !is.null(group_col)) {
    group_vals <- unique(data[[group_col]])
  }

  if (is.null(point_color)) {
    if (length(group_vals) > 0) {
      # Generate distinct colors for each group
      point_color <- scales::hue_pal()(length(group_vals))  # Generate colors for each group
    } else {
      point_color <- "black"  # Default to black if no groups are defined
    }
  }

  if (is.null(point_fill)) {
    point_fill <- point_color  # Default fill to the same as point_color
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
    plot_background_color <- "#f1f1f1"
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
      ggplot2::geom_rect(aes(xmin = min(data[[x_col]]),
                             xmax = max(data[[x_col]]),
                             ymin = min(data[[y_col]]),
                             ymax = max(data[[y_col]])),
                         fill = plot_background_color, color = NA)
  }

  # Automatically assign colors if point_color or point_fill is not provided
  if (is.null(point_color)) {
    # Generate distinct colors for each group
    n_groups <- length(group_vals)
    point_color <- scales::hue_pal()(n_groups)  # Generate 'n_groups' distinct colors
  }

  if (is.null(point_fill)) {
    point_fill <- point_color  # Default fill to the same as point_color
  }

  # Automatically set hull_fill to point_color if hull_fill is not provided
  if (is.null(hull_fill)) {
    hull_fill <- point_color  # Use the same colors as point_color for the hulls
  }

  # Add the points for each group, even if they have fewer than 3 points
  if (!is.null(group_col) && !is.null(group_vals)) {
    point_color <- rep_len(point_color, length(group_vals))
    point_fill <- rep_len(point_fill, length(group_vals))
    point_shape <- rep_len(point_shape, length(group_vals))
    point_size <- rep_len(point_size, length(group_vals))

    for (i in seq_along(group_vals)) {
      group_val <- group_vals[i]
      group_data <- data %>% dplyr::filter(!!rlang::sym(group_col) == group_val)

      # Always plot the points, regardless of the number of points
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


  # Handle convex hulls if enabled and group_col is provided
  if (!is.null(group_col) && show_hulls) {
    if (is.null(show_hull_for_groups)) {
      show_hull_for_groups <- unique(valid_data[[group_col]])
    }

    hull_fill <- rep_len(hull_fill, length(show_hull_for_groups))
    hull_color <- rep_len(hull_color, length(show_hull_for_groups))
    hull_alpha <- rep_len(hull_alpha, length(show_hull_for_groups))

    for (i in seq_along(show_hull_for_groups)) {
      group_val <- show_hull_for_groups[i]
      group_data <- valid_data %>% dplyr::filter(!!rlang::sym(group_col) == group_val)
      message(paste("Processing group:", group_val, "with", nrow(group_data), "points."))

      if (nrow(group_data) >= 3) {
        hull_indices <- grDevices::chull(group_data[[x_col]], group_data[[y_col]])
        hull_data <- group_data[hull_indices, ]

        plot <- plot +
          ggplot2::geom_polygon(data = hull_data,
                                ggplot2::aes_string(x = x_col, y = y_col),
                                fill = hull_fill[i],
                                color = hull_color[i],
                                linetype = hull_linetype,
                                alpha = hull_alpha[i])
      } else {
        warning(paste("Group", group_val, "has fewer than 3 points, skipping hull calculation."))
      }
    }
  }


  # Compute shapes based on the `show_shapes_for_groups` parameter
  if (show_shapes) {
    if (!is.null(show_shapes_for_groups) && !is.null(group_col)) {
      # Show shapes for specific groups based on their convex hull
      for (group in show_shapes_for_groups) {
        group_data <- valid_data %>% dplyr::filter(.data[[group_col]] == group)

        if (nrow(group_data) >= 3) {
          # Compute convex hull for the group
          hull_indices <- grDevices::chull(group_data[[x_col]], group_data[[y_col]])
          hull_data <- group_data[hull_indices, ]

          # Calculate the center of the plot
          plot_center_x <- mean(valid_data[[x_col]])
          plot_center_y <- mean(valid_data[[y_col]])

          for (j in seq_len(nrow(hull_data))) {
            shape_data <- hull_data[[shape_col]][[j]]

            if (!is.null(shape_data)) {
              # Scale the shape
              shape_scaled <- shape_data$coo[[1]] * shape_size

              # Determine the position of the convex hull point
              x_pos <- hull_data[[x_col]][j]
              y_pos <- hull_data[[y_col]][j]

              # Calculate the direction vector from the center to the point
              direction_x <- x_pos - plot_center_x
              direction_y <- y_pos - plot_center_y

              # Normalize the direction vector
              magnitude <- sqrt(direction_x^2 + direction_y^2)
              if (magnitude > 0) {
                direction_x <- direction_x / magnitude
                direction_y <- direction_y / magnitude
              }

              # Apply the shift distance to the direction vector
              shift_x <- direction_x * shape_shift
              shift_y <- direction_y * shape_shift

              # Apply manual adjustments
              shift_x <- shift_x + shape_x_adjust
              shift_y <- shift_y + shape_y_adjust

              # Translate the shape to its new position (shifted and adjusted)
              shape_translated <- data.frame(
                x = shape_scaled[, 1] + x_pos + shift_x,
                y = shape_scaled[, 2] + y_pos + shift_y
              )

              # Add the shape as a polygon to the plot
              plot <- plot +
                ggplot2::geom_polygon(data = shape_translated,
                                      ggplot2::aes(x = x, y = y),
                                      fill = "black", color = "black", alpha = 1)
            }
          }
        }
      }
    } else if (nrow(valid_data) >= 3) {
      # Default behavior: Compute convex hull for the entire dataset
      hull_indices <- grDevices::chull(valid_data[[x_col]], valid_data[[y_col]])
      hull_data <- valid_data[hull_indices, ]

      # Calculate the center of the plot
      plot_center_x <- mean(valid_data[[x_col]])
      plot_center_y <- mean(valid_data[[y_col]])

      for (j in seq_len(nrow(hull_data))) {
        shape_data <- hull_data[[shape_col]][[j]]

        if (!is.null(shape_data)) {
          # Scale the shape
          shape_scaled <- shape_data$coo[[1]] * shape_size

          # Determine the position of the convex hull point
          x_pos <- hull_data[[x_col]][j]
          y_pos <- hull_data[[y_col]][j]

          # Calculate the direction vector from the center to the point
          direction_x <- x_pos - plot_center_x
          direction_y <- y_pos - plot_center_y

          # Normalize the direction vector
          magnitude <- sqrt(direction_x^2 + direction_y^2)
          if (magnitude > 0) {
            direction_x <- direction_x / magnitude
            direction_y <- direction_y / magnitude
          }

          # Apply the shift distance to the direction vector
          shift_x <- direction_x * shape_shift
          shift_y <- direction_y * shape_shift

          # Apply manual adjustments
          shift_x <- shift_x + shape_x_adjust
          shift_y <- shift_y + shape_y_adjust

          # Translate the shape to its new position (shifted and adjusted)
          shape_translated <- data.frame(
            x = shape_scaled[, 1] + x_pos + shift_x,
            y = shape_scaled[, 2] + y_pos + shift_y
          )

          # Add the shape as a polygon to the plot
          plot <- plot +
            ggplot2::geom_polygon(data = shape_translated,
                                  ggplot2::aes(x = x, y = y),
                                  fill = "black", color = "black", alpha = 1)
        }
      }
    } else {
      warning("Not enough points to compute shapes or convex hull.")
    }
  }

  # Add contours if enabled and group_col and group_vals are provided
  if (!is.null(group_col) && show_contours && !is.null(group_vals)) {
    for (i in seq_along(group_vals)) {
      group_val <- group_vals[i]

      # Only apply contours to the specified groups
      if (group_val %in% show_contours_for_groups) {
        group_data <- data %>% dplyr::filter(!!rlang::sym(group_col) == group_val)

        # Only calculate the contours if there are enough points
        if (nrow(group_data) >= 3) {
          message(paste("Computing contours for group:", group_val, "with", nrow(group_data), "points."))
          # Estimate 2D density using kde2d
          density <- MASS::kde2d(group_data[[x_col]], group_data[[y_col]], n = 100)

          # Convert kde2d output to a data frame for ggplot
          contour_data <- expand.grid(x = density$x, y = density$y)
          contour_data$z <- as.vector(density$z)

          # Add contour plot
          plot <- plot +
            ggplot2::geom_contour(data = contour_data,
                                  ggplot2::aes(x = x, y = y, z = z),
                                  color = contour_colors[i],
                                  size = contour_linewidth)
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

      # Check if the group has fewer than 3 points and skip the hull calculation, but allow plotting
      if (nrow(group_data) < 3) {
        warning(paste("Group", group_val, "has fewer than 3 points. Plotting the points, but skipping any hull calculation."))
      }

      # Plot the points for the group, even if it has fewer than 3 points
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
      axis.title.y = ggplot2::element_blank(),  # **Hide default y-axis label**
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

  # Customizing the axes and tick marks with fixed tick length
  plot <- plot +
    ggplot2::geom_segment(data = data.frame(x = x_ticks),
                          ggplot2::aes(x = x, xend = x, y = -tick_length, yend = tick_length),
                          color = axis_color, size = axis_linewidth)

  plot <- plot +
    ggplot2::geom_segment(data = data.frame(y = y_ticks),
                          ggplot2::aes(y = y, yend = y, x = -tick_length, xend = tick_length),
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
  plot <- plot + ggplot2::labs(title = title, y = NULL)  # Set y to NULL to avoid duplicate y-label

  # Add custom axis labels with optional black borders (like text fields)
  if (show_label_text_fields) {
    plot <- plot +
      ggplot2::annotate("label",
                        x = max(x_range) + x_expand + as.numeric(x_label_adjust_x),  # Use numeric addition for x adjustment
                        y = -0.05 * diff(y_range) + as.numeric(x_label_adjust_y),  # Use numeric addition for y adjustment
                        label = x_label, size = x_label_size, label.padding = unit(0.3, "lines"),
                        color = text_field_color, fill = text_field_fill) +
      ggplot2::annotate("label",
                        x = as.numeric(y_label_adjust_x),  # Fixed position for y-label
                        y = max(y_range) + 0.12 * max(y_range) + y_expand + as.numeric(y_label_adjust_y),  # Fixed vertical adjustment for y-label
                        label = y_label, size = y_label_size, label.padding = unit(0.3, "lines"),
                        color = text_field_color, fill = text_field_fill, angle = ifelse(rotate_y_label, 90, 0))
  } else {
    plot <- plot +
      ggplot2::annotate("text",
                        x = max(x_range) + x_expand + as.numeric(x_label_adjust_x),  # Use numeric addition for x adjustment
                        y = -0.05 * diff(y_range) + as.numeric(x_label_adjust_y),  # Use numeric addition for y adjustment
                        label = x_label, size = x_label_size, color = text_color) +
      ggplot2::annotate("text",
                        x = as.numeric(y_label_adjust_x),  # Fixed position for y-label
                        y = max(y_range) + 0.12 * max(y_range) + y_expand + as.numeric(y_label_adjust_y),  # Fixed vertical adjustment for y-label
                        label = y_label, size = y_label_size, color = text_color, angle = ifelse(rotate_y_label, 90, 0))
  }


  # Dynamically calculate plot dimensions for export
  if (export) {
    # Determine the aspect ratio based on data ranges
    aspect_ratio <- diff(range(data[[y_col]], na.rm = TRUE)) / diff(range(data[[x_col]], na.rm = TRUE))

    # Use fixed width and scale height proportionally
    width_in <- 10  # Fixed width in inches
    height_in <- width_in * aspect_ratio  # Adjust height based on aspect ratio

    # Save the plot using ggsave with the calculated dimensions
    ggsave(
      filename = file_path,
      plot = plot,
      device = "tiff",
      dpi = 600,  # High resolution for export
      width = width_in,
      height = height_in
    )
    message(paste("Plot exported to", file_path, "with width:", width_in, "inches, height:", height_in, "inches."))
  }

  return(plot)
}
