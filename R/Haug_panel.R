#' Create a Customizable Panel of Plots (Hulls, Heatmaps, Contours, or Boxplots)
#'
#' This function generates a panel of plots based on the specified type. It can create panels with convex hulls, heatmaps, contours,
#' or boxplots. Each plot is customizable with options for group-specific aesthetics, plot style, and layout.
#'
#' @param data A data frame containing the variables to plot.
#' @param x_col A character string representing the name of the column to be plotted on the x-axis.
#' @param y_col A character string representing the name of the column to be plotted on the y-axis.
#' @param group_col (Optional) A character string representing the name of the grouping column.
#' @param group_vals (Optional) A vector specifying the group values to be plotted. Must correspond to values in `group_col`.
#' @param colors A vector of colors for each group. If NULL, the function automatically generates distinct colors.
#' @param point_fill The fill color for scatter plot points. Default is "darkgrey".
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
#' @param panel_type A character string specifying the type of panel to create. Options are "hulls", "heatmaps", "contours", or "boxplot". Default is "hulls".
#' @param plot_style A character string specifying the plot style. Options are "Haug" (default) and "inverted_Haug".
#' @param ncol A numeric value specifying the number of columns in the panel layout. Default is 2.
#' @param plot_width A numeric value specifying the width (in inches) of each individual plot in the panel. Default is 10.
#' @param plot_height A numeric value specifying the height (in inches) of each individual plot in the panel. Default is 10.
#' @param resolution A numeric value specifying the resolution (in DPI) for the output plot. Default is 300.
#' @param plot_spacing A numeric value specifying the spacing between plots in the panel to prevent label cutoffs. Default is 0.3.
#' @param x_label_adjust_x A numeric value for horizontal adjustment of the x-axis label. Default is 0.
#' @param x_label_adjust_y A numeric value for vertical adjustment of the x-axis label. Default is 0.
#' @param y_label_adjust_x A numeric value for horizontal adjustment of the y-axis label. Default is 0.
#' @param y_label_adjust_y A numeric value for vertical adjustment of the y-axis label. Default is 0.
#' @param save_path (Optional) A character string specifying the file path to save the output panel plot. If NULL, the plot is not saved.
#'
#' @return A `patchwork` object representing the combined panel of plots.
#'
#' @examples
#' # Example data
#' df <- data.frame(
#'   PC1 = rnorm(100),
#'   PC2 = rnorm(100),
#'   group = sample(c("A", "B", "C"), 100, replace = TRUE)
#' )
#'
#' # Create a panel of hull plots
#' Haug_panel(df, x_col = "PC1", y_col = "PC2", group_col = "group", group_vals = c("A", "B", "C"), panel_type = "hulls")
#'
#' # Create a panel of heatmap plots for each group
#' Haug_panel(df, x_col = "PC1", y_col = "PC2", group_col = "group", group_vals = c("A", "B", "C"), panel_type = "heatmaps")
#'
#' # Create a panel of boxplots for two PCs
#' Haug_panel(df, x_col = "PC1", y_col = "PC2", group_col = "group", group_vals = c("A", "B"), panel_type = "boxplot")
#'
#' @export
#' @import ggplot2
#' @importFrom patchwork wrap_plots plot_layout
#' @importFrom RColorBrewer brewer.pal
#' @importFrom base lapply length rep_len inherits
#' @importFrom stats factor

Haug_panel <- function(data, x_col, y_col, group_col = NULL,
                       group_vals = NULL,  # Optional
                       colors = NULL,  # Set colors to NULL by default
                       point_fill = "darkgrey", point_shape = 21, point_size = 2,
                       title_size = 24, label_size = 20, tick_size = 15,
                       tick_length = 0.005, axis_linewidth = 1,
                       hull_alpha = 0.3,  # Alpha for hulls
                       heatmap_alpha = 1.3,  # Increased alpha for heatmap visibility
                       heatmap_bins = 100,  # Higher resolution for heatmaps
                       contour_linewidth = 1,  # Linewidth for contours
                       panel_type = "hulls",  # Choose between "hulls", "heatmaps", "contours", "boxplot"
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
  valid_panel_types <- c("hulls", "heatmaps", "contours", "boxplot")
  if (!panel_type %in% valid_panel_types) {
    stop(paste("Invalid 'panel_type'. Please choose from", paste(valid_panel_types, collapse = ", "), "."))
  }

  # Check if `patchwork` is available for combining plots
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop("The `patchwork` package is required but is not installed. Please install it with install.packages('patchwork').")
  }

  # If no colors are provided, automatically generate distinct colors
  if (is.null(colors)) {
    n_groups <- length(group_vals)  # Number of groups
    colors <- RColorBrewer::brewer.pal(min(n_groups, 8), "Set1")  # Automatically assign colors from "Set1"
  }

  # Define a custom function to generate a plot using shape_plot with various settings
  generate_plot <- function(show_hulls = FALSE, show_heatmaps = FALSE, show_contours = FALSE,
                            hull_groups = NULL, heatmap_groups = NULL, contour_groups = NULL, plot_title = NULL) {
    plot <- shape_plot(data, x_col = x_col, y_col = y_col, group_col = group_col, group_vals = group_vals,
                       point_color = colors, point_fill = point_fill, point_shape = point_shape, point_size = point_size,
                       title = plot_title, title_size = title_size, label_size = label_size, tick_size = tick_size, tick_length = tick_length,
                       axis_linewidth = axis_linewidth, show_hulls = show_hulls, show_heatmaps = show_heatmaps,
                       show_contours = show_contours, hull_fill = colors, hull_alpha = hull_alpha,
                       heatmap_colors = lapply(colors, function(col) c("white", col)),  # Use colors for heatmap gradients
                       heatmap_alpha = heatmap_alpha,  # Increased heatmap intensity
                       heatmap_bins = heatmap_bins,  # Higher bin count for finer details
                       contour_colors = colors, contour_linewidth = contour_linewidth,
                       show_hull_for_groups = hull_groups, show_heatmap_for_groups = heatmap_groups,
                       show_contours_for_groups = contour_groups, plot_style = plot_style,
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

  # If panel type is "hulls", create hull plots for each group and combined hulls
  if (panel_type == "hulls") {
    for (i in seq_along(group_vals)) {
      group_val <- group_vals[i]
      plot_list[[i]] <- generate_plot(show_hulls = TRUE, show_contours = FALSE, show_heatmaps = FALSE,
                                      hull_groups = group_val, plot_title = paste("Hull for Group", group_val))
    }
    # Add a plot with combined hulls for all groups
    plot_list[[length(group_vals) + 1]] <- generate_plot(show_hulls = TRUE, show_contours = FALSE, show_heatmaps = FALSE,
                                                         plot_title = "Combined Hulls for All Groups")

    # If panel type is "heatmaps", create heatmap plots for each group separately (non-overlapping)
  } else if (panel_type == "heatmaps") {
    for (i in seq_along(group_vals)) {
      group_val <- group_vals[i]
      plot_list[[i]] <- generate_plot(show_hulls = FALSE, show_contours = FALSE, show_heatmaps = TRUE,
                                      heatmap_groups = group_val, plot_title = paste("Heatmap for Group", group_val))
    }

    # If panel type is "contours", create contour plots for each group (non-overlapping)
  } else if (panel_type == "contours") {
    for (i in seq_along(group_vals)) {
      group_val <- group_vals[i]
      plot_list[[i]] <- generate_plot(show_hulls = FALSE, show_contours = TRUE, show_heatmaps = FALSE,
                                      contour_groups = group_val, plot_title = paste("Contours for Group", group_val))
    }

    # If panel type is "boxplot", create boxplots for each principal component (PC)
  } else if (panel_type == "boxplot") {
    # Create boxplots for the specified PCs (x_col and y_col)
    pc1_boxplot <- ggplot2::ggplot(data, ggplot2::aes_string(x = group_col, y = x_col, fill = group_col)) +
      ggplot2::geom_boxplot() +
      ggplot2::theme_minimal() + ggplot2::labs(title = paste("Boxplot for", x_col), x = group_col, y = x_col) +
      ggplot2::scale_fill_manual(values = colors[match(unique(data[[group_col]]), group_vals)]) +  # Match colors to group_vals
      ggplot2::theme(plot.title = ggplot2::element_text(size = title_size))

    pc2_boxplot <- ggplot2::ggplot(data, ggplot2::aes_string(x = group_col, y = y_col, fill = group_col)) +
      ggplot2::geom_boxplot() +
      ggplot2::theme_minimal() + ggplot2::labs(title = paste("Boxplot for", y_col), x = group_col, y = y_col) +
      ggplot2::scale_fill_manual(values = colors[match(unique(data[[group_col]]), group_vals)]) +  # Match colors to group_vals
      ggplot2::theme(plot.title = ggplot2::element_text(size = title_size))

    # Add the boxplots to the plot list
    plot_list[[1]] <- pc1_boxplot
    plot_list[[2]] <- pc2_boxplot
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
