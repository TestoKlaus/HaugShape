Haug_overview <- function(data, cols = c("col1", "col2"), group_col = NULL, group_vals = NULL,
                          colors = NULL, point_color = NULL, point_fill = NULL, point_shape = NULL, point_size = NULL,
                          title_size = 24, label_size = 20, tick_size = 15, tick_length = 0.005,
                          axis_linewidth = 1, hull_alpha = 0.3, contour_linewidth = 1, plot_style = "Haug",
                          ncol = 2, plot_width = 10, plot_height = 10,
                          resolution = 300, plot_spacing = 0.3, save_path = NULL, export_pdf = FALSE, pdf_file_name = "overview_plots.pdf") {

  # Error handling for missing required parameters
  if (length(cols) %% 2 != 0 || length(cols) > 6) {
    stop("Please provide 2, 4, or 6 column names in 'cols'.")
  }

  # Validate that group_col exists in the data if provided
  if (!is.null(group_col) && !group_col %in% colnames(data)) {
    stop(paste("The column", group_col, "does not exist in the data. Please provide a valid group column."))
  }

  # Validate that all cols exist in the data
  for (col in cols) {
    if (!col %in% colnames(data)) {
      stop(paste("The column", col, "does not exist in the data. Please provide a valid column name."))
    }
  }

  # Validate that group_vals correspond to the values in group_col (if provided)
  if (!is.null(group_vals) && !all(group_vals %in% unique(data[[group_col]]))) {
    stop("Some values in 'group_vals' do not exist in the specified 'group_col'. Please check the group values.")
  }

  # Assign default colors for groups if not provided
  if (is.null(colors)) {
    n_groups <- length(unique(data[[group_col]]))
    colors <- scales::hue_pal()(n_groups)  # Generate distinct colors based on group count
  }

  # Use default settings for point color, shape, size, and fill from shape_plot if not provided
  if (is.null(point_color)) {
    point_color <- colors  # Use group-based colors
  }
  if (is.null(point_fill)) {
    point_fill <- colors  # Use group-based colors
  }
  if (is.null(point_shape)) {
    point_shape <- 21  # Default point shape
  }
  if (is.null(point_size)) {
    point_size <- 2  # Default point size
  }

  generate_plots_for_column_pair <- function(data, col1, col2, group_col, group_vals, colors, point_color, point_fill,
                                             point_shape, point_size, title_size, label_size, tick_size, tick_length, axis_linewidth,
                                             hull_alpha, contour_linewidth, plot_style) {

    # Dynamically calculate axis adjustments based on data range for the current column pair
    x_range <- range(data[[col1]], na.rm = TRUE)
    y_range <- range(data[[col2]], na.rm = TRUE)

    # Move x-axis label 1% lower for the current column pair
    x_label_adjust_y <- -0.01 * (y_range[2] - y_range[1])

    # Move y-axis label 2% to the left for the current column pair in contour plots
    y_label_adjust_x <- -0.02 * (x_range[2] - x_range[1])

    # Adjust tick length based on the ranges of the current axes
    tick_length <- 0.005 * min(diff(x_range), diff(y_range))

    # Create hull plot for the column pair
    hull_plot <- shape_plot(data, x_col = col1, y_col = col2, group_col = group_col, group_vals = group_vals,
                            point_color = point_color, point_fill = point_fill, point_shape = point_shape, point_size = point_size,
                            title = paste("Hulls for", col1, "vs", col2), title_size = title_size, label_size = label_size,
                            tick_size = tick_size, tick_length = tick_length, axis_linewidth = axis_linewidth,
                            show_hulls = TRUE, hull_fill = colors, hull_alpha = hull_alpha, plot_style = plot_style,
                            x_label_adjust_y = x_label_adjust_y)

    # Create contour plots for each group for the column pair
    contour_plots <- lapply(group_vals, function(group) {
      shape_plot(data, x_col = col1, y_col = col2, group_col = group_col, group_vals = group_vals,
                 point_color = point_color, point_fill = point_fill, point_shape = point_shape, point_size = point_size,
                 title = paste("Contours for Group", group, "(", col1, "vs", col2, ")"), title_size = title_size, label_size = label_size,
                 tick_size = tick_size, tick_length = tick_length, axis_linewidth = axis_linewidth,
                 show_contours = TRUE, contour_colors = colors, contour_linewidth = contour_linewidth,
                 show_contours_for_groups = group, plot_style = plot_style,
                 x_label_adjust_y = x_label_adjust_y, y_label_adjust_x = y_label_adjust_x)
    })

    return(list(hull_plot = hull_plot, contour_plots = contour_plots))
  }


  # Reshape the data into long format for a single boxplot comparing groups across columns
  data_long <- data %>%
    dplyr::select(all_of(c(group_col, cols))) %>%
    tidyr::pivot_longer(cols = all_of(cols), names_to = "column", values_to = "value")

  # Create a single boxplot comparing groups across all columns
  boxplot <- ggplot2::ggplot(data_long, ggplot2::aes_string(x = "column", y = "value", fill = group_col)) +
    ggplot2::geom_boxplot() +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Comparison of Groups Across Columns", x = "Columns", y = "Values") +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = title_size))

  # Initialize an empty list to store all generated plots
  all_plots <- list()

  # Generate hull and contour plots for each column pair
  for (i in seq(1, length(cols), by = 2)) {
    col1 <- cols[i]
    col2 <- cols[i + 1]
    plots <- generate_plots_for_column_pair(data, col1, col2, group_col, group_vals, colors, point_color, point_fill,
                                            point_shape, point_size, title_size, label_size, tick_size, tick_length, axis_linewidth,
                                            hull_alpha, contour_linewidth, plot_style)
    all_plots <- c(all_plots, list(plots))
  }

  # Save the overview panel as a PDF if export_pdf is TRUE
  if (export_pdf) {
    # Open a PDF device to write the plots
    pdf(file = pdf_file_name, width = plot_width, height = plot_height)

    # 1. Print the hull plots (2 or 3 per page)
    hull_plots <- lapply(all_plots, function(plot_set) plot_set$hull_plot)
    hull_pages <- split(hull_plots, ceiling(seq_along(hull_plots) / 2))  # Split into pages of 2 (or 3 for 6 cols)
    for (page in hull_pages) {
      print(patchwork::wrap_plots(page, ncol = 1))
    }

    # 2. Print the contour plots (2 per page)
    for (plot_set in all_plots) {
      contour_pages <- split(plot_set$contour_plots, ceiling(seq_along(plot_set$contour_plots) / 2))
      for (page in contour_pages) {
        print(patchwork::wrap_plots(page, ncol = 1))
      }
    }

    # 3. Print the single combined boxplot for all columns
    print(boxplot)

    # Close the PDF device
    dev.off()
    message(paste("Plots exported to", pdf_file_name))
  }

  return(list(hull_and_contours = all_plots, boxplot = boxplot))
}
