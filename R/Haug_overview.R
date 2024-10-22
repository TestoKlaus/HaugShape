#' Generate an Overview Panel with Hulls, Heatmaps, Contours, and Boxplots
#'
#' This function generates a comprehensive overview panel that combines multiple types of plots such as hulls, heatmaps,
#' contours, and boxplots. It allows users to customize plot styles, colors, and groupings, and produces an overview of
#' different data visualizations based on the given dataset.
#'
#' @param data A data frame containing the data to be plotted.
#' @param x_col A character string representing the name of the column to be plotted on the x-axis.
#' @param y_col A character string representing the name of the column to be plotted on the y-axis.
#' @param group_col (Optional) A character string representing the name of the grouping column.
#' @param group_vals (Optional) A vector specifying the group values to be plotted. Must correspond to values in `group_col`.
#' @param colors A vector of colors for each group. If NULL, default colors will be generated using RColorBrewer's Set1 palette.
#' @param point_fill The fill color for scatter plot points. Default is "white".
#' @param point_shape The shape of the scatter points. Default is 21 (circle).
#' @param point_size The size of the scatter points. Default is 2.
#' @param title_size A numeric value specifying the font size for plot titles. Default is 24.
#' @param label_size A numeric value specifying the font size for axis labels. Default is 20.
#' @param tick_size A numeric value specifying the font size for axis tick labels. Default is 15.
#' @param tick_length A numeric value specifying the proportional length of axis ticks relative to plot size. Default is 0.005.
#' @param axis_linewidth A numeric value specifying the width of the axes and tick marks. Default is 1.
#' @param hull_alpha A numeric value specifying the transparency level for convex hulls. Default is 0.3.
#' @param heatmap_alpha A numeric value specifying the transparency level for heatmaps. Default is 1.3.
#' @param heatmap_bins A numeric value specifying the number of bins for heatmap density estimation. Default is 100.
#' @param contour_linewidth A numeric value specifying the thickness of contour lines. Default is 1.
#' @param plot_style A character string specifying the plot style. Options are "Haug" (default) and "inverted_Haug".
#' @param ncol A numeric value specifying the number of columns in the panel layout. Default is 2.
#' @param plot_width A numeric value specifying the width (in inches) of each individual plot in the panel. Default is 10.
#' @param plot_height A numeric value specifying the height (in inches) of each individual plot in the panel. Default is 10.
#' @param resolution A numeric value specifying the resolution (in DPI) for the output plot. Default is 300.
#' @param plot_spacing A numeric value specifying the spacing between plots in the panel to prevent label cutoffs. Default is 0.3.
#' @param save_path (Optional) A character string specifying the file path to save the output panel plot. If NULL, the plot is not saved.
#'
#' @return A `patchwork` object representing the combined overview panel of plots.
#'
#' @examples
#' # Example data
#' df <- data.frame(
#'   PC1 = rnorm(100),
#'   PC2 = rnorm(100),
#'   group = sample(c("A", "B", "C"), 100, replace = TRUE)
#' )
#'
#' # Create an overview panel with hulls, heatmaps, contours, and boxplots
#' Haug_overview(df, x_col = "PC1", y_col = "PC2", group_col = "group", group_vals = c("A", "B", "C"))
#'
#' @export
#' @import ggplot2
#' @importFrom patchwork wrap_plots plot_layout
#' @importFrom RColorBrewer brewer.pal
#' @importFrom base lapply
#' @importFrom stats factor


Haug_overview <- function(data, x_col, y_col, group_col = NULL, group_vals = NULL,
                          colors = NULL, point_fill = "white", point_shape = 21, point_size = 2,
                          title_size = 24, label_size = 20, tick_size = 15, tick_length = 0.005,
                          axis_linewidth = 1, hull_alpha = 0.3, heatmap_alpha = 1.3, heatmap_bins = 100,
                          contour_linewidth = 1, plot_style = "Haug", ncol = 2, plot_width = 10, plot_height = 10,
                          resolution = 300, plot_spacing = 0.3, save_path = NULL) {

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

  # Check if required packages are available
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop("The `patchwork` package is required but is not installed. Please install it with install.packages('patchwork').")
  }

  # Assign default colors if not provided
  if (is.null(colors)) {
    n_groups <- length(group_vals)
    colors <- RColorBrewer::brewer.pal(min(n_groups, 8), "Set1")
  }

  # Helper function to create hull plots
  hull_plot <- shape_plot(data, x_col = x_col, y_col = y_col, group_col = group_col, group_vals = group_vals,
                          point_color = colors, point_fill = point_fill, point_shape = point_shape, point_size = point_size,
                          title = "Combined Hulls", title_size = title_size, label_size = label_size,
                          tick_size = tick_size, tick_length = tick_length, axis_linewidth = axis_linewidth,
                          show_hulls = TRUE, hull_fill = colors, hull_alpha = hull_alpha, plot_style = plot_style)

  # Helper function to create heatmap plots for each group
  heatmap_plots <- lapply(group_vals, function(group) {
    shape_plot(data, x_col = x_col, y_col = y_col, group_col = group_col, group_vals = group_vals,
               point_color = colors, point_fill = point_fill, point_shape = point_shape, point_size = point_size,
               title = paste("Heatmap for Group", group), title_size = title_size, label_size = label_size,
               tick_size = tick_size, tick_length = tick_length, axis_linewidth = axis_linewidth,
               show_heatmaps = TRUE, heatmap_colors = lapply(colors, function(col) c("white", col)),
               heatmap_alpha = heatmap_alpha, heatmap_bins = heatmap_bins, show_heatmap_for_groups = group,
               plot_style = plot_style)
  })

  # Helper function to create contour plots for each group
  contour_plots <- lapply(group_vals, function(group) {
    shape_plot(data, x_col = x_col, y_col = y_col, group_col = group_col, group_vals = group_vals,
               point_color = colors, point_fill = point_fill, point_shape = point_shape, point_size = point_size,
               title = paste("Contours for Group", group), title_size = title_size, label_size = label_size,
               tick_size = tick_size, tick_length = tick_length, axis_linewidth = axis_linewidth,
               show_contours = TRUE, contour_colors = colors, contour_linewidth = contour_linewidth,
               show_contours_for_groups = group, plot_style = plot_style)
  })

  # Ensure group_col has the same factor levels as group_vals
  data[[group_col]] <- factor(data[[group_col]], levels = group_vals)

  # Create box plots for the two PCs
  pc1_boxplot <- ggplot2::ggplot(data, ggplot2::aes_string(x = group_col, y = x_col, fill = group_col)) +
    ggplot2::geom_boxplot() +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = paste("Boxplot for", x_col), x = group_col, y = x_col) +
    ggplot2::scale_fill_manual(values = colors) +  # Use consistent colors
    ggplot2::theme(plot.title = ggplot2::element_text(size = title_size))

  pc2_boxplot <- ggplot2::ggplot(data, ggplot2::aes_string(x = group_col, y = y_col, fill = group_col)) +
    ggplot2::geom_boxplot() +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = paste("Boxplot for", y_col), x = group_col, y = y_col) +
    ggplot2::scale_fill_manual(values = colors) +  # Use consistent colors
    ggplot2::theme(plot.title = ggplot2::element_text(size = title_size))

  # Combine all plots using patchwork
  overview_panel <- (
    hull_plot +
      patchwork::wrap_plots(heatmap_plots, ncol = ncol) +
      patchwork::wrap_plots(contour_plots, ncol = ncol) +
      (pc1_boxplot / pc2_boxplot)
  ) + patchwork::plot_layout(guides = "collect") &
    ggplot2::theme(plot.margin = ggplot2::unit(rep(plot_spacing, 4), "cm"))

  # Save plot if save_path is provided
  if (!is.null(save_path)) {
    ggplot2::ggsave(filename = save_path, plot = overview_panel, width = plot_width * ncol, height = plot_height * 4, dpi = resolution, units = "in")
  }

  return(overview_panel)
}
