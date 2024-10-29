Haug_panel <- function(data, x_col, y_col, group_col = NULL,
                       group_vals = NULL,  # Optional
                       colors = NULL,  # Set colors to NULL by default
                       point_fill = NULL, point_shape = 21, point_size = 2,
                       title_size = 24, label_size = 20, tick_size = 15,
                       tick_length = 0.005, axis_linewidth = 1,
                       hull_alpha = 0.3,  # Alpha for hulls
                       contour_linewidth = 1,  # Linewidth for contours
                       panel_type = "hulls",  # Choose between "hulls", "contours", "boxplot"
                       plot_style = "Haug",  # New param to toggle between "Haug" and "inverted_Haug"
                       ncol = 2,  # Number of columns for the panel layout
                       plot_width = 10,  # Default width for each individual plot in inches
                       plot_height = 10,  # Default height for each individual plot in inches
                       resolution = 300,  # Resolution in DPI for higher resolution output
                       plot_spacing = 0.3,  # Add spacing between plots to prevent cutting off axis labels
                       x_label_adjust_x = 0,  # Adjust x-axis label position
                       x_label_adjust_y = 0,  # Adjust x-axis label position
                       y_label_adjust_x = 0,  # Adjust y-axis label position
                       y_label_adjust_y = 0,  # Adjust y-axis label position
                       save_path = NULL  # Path to save the final plot, optional
) {

  # Error handling for missing required parameters
  if (missing(data) || missing(x_col) || missing(y_col)) {
    stop("Missing required arguments: 'data', 'x_col', or 'y_col'. Please provide these parameters.")
  }

  # Validate that group_col exists in the data if provided
  if (!is.null(group_col) && !group_col %in% colnames(data)) {
    stop(paste("The column", group_col, "does not exist in the data. Please provide a valid group column."))
  }

  # Validate that x_col and y_col exist in the data
  if (!x_col %in% colnames(data)) {
    stop(paste("The column", x_col, "does not exist in the data. Please provide a valid x-axis column."))
  }
  if (!y_col %in% colnames(data)) {
    stop(paste("The column", y_col, "does not exist in the data. Please provide a valid y-axis column."))
  }

  # Validate that group_vals correspond to the values in group_col (if provided)
  if (!is.null(group_vals) && !all(group_vals %in% unique(data[[group_col]]))) {
    stop("Some values in 'group_vals' do not exist in the specified 'group_col'. Please check the group values.")
  }

  # Validate panel_type input
  valid_panel_types <- c("hulls", "contours", "boxplot")
  if (!panel_type %in% valid_panel_types) {
    stop(paste("Invalid 'panel_type'. Please choose from", paste(valid_panel_types, collapse = ", "), "."))
  }

  # If no colors are provided, use the default color palette from shape_plot
  if (is.null(colors)) {
    # Use the color assignment mechanism from shape_plot
    colors <- scales::hue_pal()(length(group_vals))
  }

  # Modify the generate_plot function to correctly apply colors
  generate_plot <- function(show_hulls = FALSE, show_contours = FALSE,
                            hull_groups = NULL, contour_groups = NULL, show_hull_for_groups = NULL,
                            plot_title = NULL) {
    # Determine hull colors dynamically based on the groups being displayed
    hull_colors <- colors[match(show_hull_for_groups, group_vals)]

    plot <- shape_plot(data, x_col = x_col, y_col = y_col, group_col = group_col, group_vals = group_vals,
                       point_color = colors, point_fill = point_fill, point_shape = point_shape, point_size = point_size,
                       title = plot_title, title_size = title_size, label_size = label_size, tick_size = tick_size, tick_length = tick_length,
                       axis_linewidth = axis_linewidth, show_hulls = show_hulls, show_contours = show_contours,
                       hull_fill = hull_colors, hull_alpha = hull_alpha,  # Use hull_colors here
                       contour_colors = colors, contour_linewidth = contour_linewidth,
                       show_hull_for_groups = show_hull_for_groups,  # Pass show_hull_for_groups here
                       show_contours_for_groups = contour_groups,
                       plot_style = plot_style,
                       x_label_adjust_x = x_label_adjust_x, y_label_adjust_x = y_label_adjust_x,
                       x_label_adjust_y = x_label_adjust_y, y_label_adjust_y = y_label_adjust_y)
    # Ensure plot is a ggplot object
    if (!inherits(plot, "ggplot")) {
      stop("Non-ggplot object detected in plot list (likely from shape_plot).")
    }
    return(plot)
  }

  # Define empty plot list for the panel
  plot_list <- list()

  # If panel type is "hulls", create hull plots as specified: combined, each group individually, then combined again
  if (panel_type == "hulls") {
    # Loop through each group to create an individual hull plot
    for (i in seq_along(group_vals)) {
      group_val <- group_vals[i]
      plot_list[[i]] <- generate_plot(show_hulls = TRUE, show_contours = FALSE,
                                      hull_groups = NULL,  # Let shape_plot use only show_hull_for_groups
                                      show_hull_for_groups = c(group_val),  # Show only the hull for the current group
                                      plot_title = paste("Hull for Group", group_val))
    }

    # Add a final combined plot with hulls for all groups
    plot_list[[length(group_vals) + 1]] <- generate_plot(show_hulls = TRUE, show_contours = FALSE,
                                                         hull_groups = NULL,  # Combined plot with show_hull_for_groups set to all groups
                                                         show_hull_for_groups = group_vals,  # Show hulls for all groups
                                                         plot_title = "Combined Hulls for All Groups")
  }

   else if (panel_type == "contours") {
    # Generate contour plots for each group individually
    for (i in seq_along(group_vals)) {
      group_val <- group_vals[i]
      plot_list[[i]] <- generate_plot(show_hulls = FALSE, show_contours = TRUE,
                                      contour_groups = group_val,  # Plot only for the current group
                                      plot_title = paste("Contours for Group", group_val))
    }

  } else if (panel_type == "boxplot") {
    # Convert data to long format for a single combined boxplot across columns
    data_long <- data %>%
      dplyr::select(all_of(c(group_col, x_col, y_col))) %>%
      tidyr::pivot_longer(cols = c(x_col, y_col), names_to = "PC", values_to = "Value")

    # Create a single boxplot comparing groups across both columns
    boxplot <- ggplot2::ggplot(data_long, ggplot2::aes_string(x = "PC", y = "Value", fill = group_col)) +
      ggplot2::geom_boxplot() +
      ggplot2::theme_minimal() +
      ggplot2::labs(title = "Comparison of Groups Across PCs", x = "Principal Components", y = "Values") +
      ggplot2::scale_fill_manual(values = colors[match(unique(data[[group_col]]), group_vals)]) +
      ggplot2::theme(plot.title = ggplot2::element_text(size = title_size))

    # Add the combined boxplot to the plot list
    plot_list[[1]] <- boxplot
  }

  # Combine the plots into a panel using patchwork, with additional spacing to prevent label cutoffs
  panel <- patchwork::wrap_plots(plot_list) +
    patchwork::plot_layout(ncol = ncol, guides = "collect") &
    theme(plot.margin = unit(rep(plot_spacing, 4), "cm"))  # Adjust spacing between plots

  # If save_path is provided, save the plot with high resolution
  if (!is.null(save_path)) {
    ggsave(filename = save_path, plot = panel, width = plot_width * ncol, height = plot_height * ceiling(length(plot_list) / ncol),
           dpi = resolution, units = "in")
  }

  # Return the panel of plots
  return(panel)
}
