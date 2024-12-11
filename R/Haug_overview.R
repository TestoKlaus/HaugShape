#' Generate an Overview of Hull, Contour, and Box Plots
#'
#' The `Haug_overview` function creates a multi-panel overview of scatter plots, contour plots, and a boxplot for specified column pairs in the data. Hull plots and contour plots are generated for each pair of columns, with contour plots available for each group specified. If `group_vals` is not provided, contours will be generated for every unique group in the `group_col`. Additionally, a combined boxplot is created to compare groups across all selected columns.
#'
#' @param data A data frame containing the data to be plotted.
#' @param cols A character vector specifying pairs of column names to plot. Must contain 2, 4, or 6 column names.
#' @param group_col (Optional) A character string or bare column name representing the grouping column for color and style customization.
#' @param group_vals (Optional) A vector specifying the group values to display. If NULL, all unique values in `group_col` are used.
#' @param colors A vector specifying the colors for each group. If NULL, colors are automatically generated.
#' @param point_color A character string or vector specifying the color(s) of the points. Default is `colors`.
#' @param point_fill A character string or vector specifying the fill color(s) of the points. Default is `colors`.
#' @param point_shape An integer or vector specifying the shape(s) of the points. Default is 21 (circle).
#' @param point_size A numeric value or vector specifying the size(s) of the points. Default is 2.
#' @param title_size A numeric value specifying the font size for the plot titles. Default is 24.
#' @param label_size A numeric value specifying the font size for the axis labels. Default is 20.
#' @param tick_size A numeric value specifying the font size for axis tick labels. Default is 15.
#' @param tick_length A numeric value specifying the proportional length of tick marks relative to plot size. Default is 0.005.
#' @param axis_linewidth A numeric value specifying the width of the axis lines and ticks. Default is 1.
#' @param hull_alpha A numeric value between 0 and 1 specifying the transparency for convex hulls in the hull plots. Default is 0.3.
#' @param contour_linewidth A numeric value specifying the thickness of contour lines. Default is 1.
#' @param plot_style A character string specifying the style of the plot. Options include "Haug", "inverted_Haug", and "publication". Default is "Haug".
#' @param ncol (unused) An integer specifying the number of columns in the PDF layout. Default is 2.
#' @param plot_width A numeric value specifying the width of each plot in inches for PDF export. Default is 10.
#' @param plot_height A numeric value specifying the height of each plot in inches for PDF export. Default is 10.
#' @param resolution A numeric value specifying the resolution of the exported PDF file in DPI. Default is 300.
#' @param plot_spacing A numeric value specifying the spacing between plots in the PDF. Default is 0.3.
#' @param save_path An optional file path to save the plots. If NULL, the plots are saved in the current working directory.
#' @param export_pdf A logical value indicating whether to export the overview as a multi-page PDF. Default is FALSE.
#' @param pdf_file_name A character string specifying the name of the exported PDF file if `export_pdf = TRUE`. Default is "overview_plots.pdf".
#' @param show_all_hulls A logical value indicating whether to show all group hulls. Default is FALSE.
#' @param show_all_contours A logical value indicating whether to show all group contours. Default is FALSE.
#' @param show_table A logical value indicating whether to display tables of hull specimen information. Default is FALSE.
#'
#' @return A list containing:
#' \describe{
#'   \item{hull_and_contours}{A list of hull and contour plots for each specified column pair.}
#'   \item{boxplot}{A combined boxplot comparing groups across all specified columns.}
#' }
#'
#' @examples
#' # Example usage:
#' test_data <- data.frame(
#'   col1 = rnorm(100),
#'   col2 = rnorm(100),
#'   col3 = rnorm(100),
#'   col4 = rnorm(100),
#'   group = sample(c("A", "B", "C"), 100, replace = TRUE)
#' )
#' Haug_overview(data = test_data, cols = c("col1", "col2", "col3", "col4"), group_col = "group", export_pdf = TRUE)
#'
#' @export
#' @import ggplot2
#' @import dplyr
#' @importFrom scales hue_pal
#' @importFrom patchwork wrap_plots
#' @importFrom gridExtra marrangeGrob arrangeGrob
#' @importFrom grid gpar


Haug_overview <- function(data,
                          cols = c("col1","col2"),
                          group_col = NULL,
                          group_vals = NULL,
                          colors = NULL,
                          point_color = NULL,
                          point_fill = NULL,
                          point_shape = NULL,
                          point_size = NULL,
                          title_size = 24,
                          label_size = 20,
                          tick_size = 15,
                          tick_length = NULL,
                          axis_linewidth = 1,
                          hull_alpha = 0.3,
                          contour_linewidth = 1,
                          plot_style = "Haug",
                          ncol = 2,
                          plot_width = 10,
                          plot_height = 10,
                          resolution = 300,
                          plot_spacing = 0.3,
                          output_dir = NULL,  # Add new parameter for output directory
                          export_pdf = FALSE,
                          pdf_file_name = "overview_plots.pdf",
                          show_all_hulls = FALSE,
                          show_all_contours = FALSE,
                          show_table = FALSE) {

  # Error handling for missing required parameters
  if (length(cols) %% 2 != 0 || length(cols) > 30) {
    stop("Please provide 2, 4, 6, ..., 20 column names in 'cols'.")
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

  # Assign group_vals to all unique values in group_col if not provided
  if (is.null(group_vals) && !is.null(group_col)) {
    group_vals <- unique(data[[group_col]])
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

  # Initialize an empty list for combined tables
  combined_tables <- list()


  generate_plots_for_column_pair <- function(data, col1, col2, group_col, group_vals, colors, point_color, point_fill,
                                             point_shape, point_size, title_size, label_size, tick_size, tick_length, axis_linewidth,
                                             hull_alpha, contour_linewidth, plot_style, show_all_hulls, show_all_contours, show_table) {

    # Dynamically calculate axis adjustments based on data range for the current column pair
    x_range <- range(data[[col1]], na.rm = TRUE)
    y_range <- range(data[[col2]], na.rm = TRUE)

    # Move x-axis label 1% lower for the current column pair
    x_label_adjust_y <- -0.02 * (y_range[2] - y_range[1])

    # Move y-axis label 2% to the left for the current column pair in contour plots
    y_label_adjust_x <- -0.02 * (x_range[2] - x_range[1])

    # Adjust tick length based on the ranges of the current axes
    tick_length <- 0.00001 * min(diff(x_range), diff(y_range))

    # Ensure single values for shape and size for each plot
    point_shape <- if (length(point_shape) == 1) point_shape else point_shape[1]
    point_size <- if (length(point_size) == 1) point_size else point_size[1]

    # Calculate the sample size for each group if group_col is provided
    if (!is.null(group_col)) {
      sample_counts <- data %>%
        dplyr::filter(!!rlang::sym(group_col) %in% group_vals) %>%
        dplyr::count(!!rlang::sym(group_col)) %>%
        dplyr::rename(group = !!rlang::sym(group_col), count = n)
    } else {
      sample_counts <- NULL
    }

    # Create combined hull plot for the column pair
    hull_plot_title <- paste("Hulls for", col1, "vs", col2)
    hull_plot <- shape_plot(data, x_col = col1, y_col = col2, group_col = group_col, group_vals = group_vals,
                            point_color = point_color, point_fill = point_fill, point_shape = point_shape, point_size = point_size,
                            title = hull_plot_title, title_size = title_size, label_size = label_size,
                            tick_size = tick_size, tick_length = tick_length, axis_linewidth = axis_linewidth,
                            show_hulls = TRUE, hull_fill = colors, hull_alpha = hull_alpha, plot_style = plot_style,
                            x_label_adjust_y = x_label_adjust_y)

    # Create individual hull plots if show_all_hulls is TRUE
    individual_hull_plots <- NULL
    if (show_all_hulls && !is.null(group_vals)) {
      individual_hull_plots <- lapply(group_vals, function(group) {
        plot_title <- paste("Hull for Group", group, "(", col1, "vs", col2, ")")
        shape_plot(data, x_col = col1, y_col = col2, group_col = group_col, group_vals = group_vals,
                   point_color = point_color, point_fill = point_fill, point_shape = point_shape, point_size = point_size,
                   title = plot_title, title_size = title_size, label_size = label_size,
                   tick_size = tick_size, tick_length = tick_length, axis_linewidth = axis_linewidth,
                   show_hulls = TRUE, hull_fill = colors, hull_alpha = hull_alpha, plot_style = plot_style,
                   show_hull_for_groups = group, x_label_adjust_y = x_label_adjust_y)
      })
    }

    # Create contour plots for each group for the column pair if show_all_contours is TRUE
    contour_plots <- if (show_all_contours) {
      lapply(group_vals, function(group) {
        count <- sample_counts %>% dplyr::filter(group == !!group) %>% dplyr::pull(count)
        plot_title <- paste("Contours for Group", group, "(", col1, "vs", col2, ", n =", count, ")")
        shape_plot(data, x_col = col1, y_col = col2, group_col = group_col, group_vals = group_vals,
                   point_color = point_color, point_fill = point_fill, point_shape = point_shape, point_size = point_size,
                   title = plot_title, title_size = title_size, label_size = label_size,
                   tick_size = tick_size, tick_length = tick_length, axis_linewidth = axis_linewidth,
                   show_contours = TRUE, contour_colors = colors, contour_linewidth = contour_linewidth,
                   show_contours_for_groups = group, plot_style = plot_style,
                   x_label_adjust_y = x_label_adjust_y, y_label_adjust_x = y_label_adjust_x)
      })
    } else {
      NULL
    }

    return(list(hull_plot = hull_plot, individual_hull_plots = individual_hull_plots, contour_plots = contour_plots))
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

  # **Generate Separate Tables for Each Column Pair** if show_table is TRUE
  if (show_table) {
    for (i in seq(1, length(cols), by = 2)) {
      col1 <- cols[i]
      col2 <- cols[i + 1]
      table <- get_hull_specimen_table(data, x_cols = col1, y_cols = col2, group_col = group_col, group_vals = group_vals)
      combined_tables[[paste(col1, col2, sep = "_vs_")]] <- table
    }
  }

  # Initialize an empty list to store all generated plots
  all_plots <- list()

  # Generate hull and contour plots for each column pair
  for (i in seq(1, length(cols), by = 2)) {
    col1 <- cols[i]
    col2 <- cols[i + 1]

    # Generate plots for the column pair
    plots <- generate_plots_for_column_pair(data, col1, col2, group_col, group_vals, colors, point_color, point_fill,
                                            point_shape, point_size, title_size, label_size, tick_size, tick_length, axis_linewidth,
                                            hull_alpha, contour_linewidth, plot_style, show_all_hulls, show_all_contours)
    all_plots <- c(all_plots, list(plots))
  }

  # Save the overview panel as a PDF if export_pdf is TRUE
  if (export_pdf) {
    if (!is.null(output_dir)) {
      # Ensure the directory exists
      if (!dir.exists(output_dir)) {
        stop("The specified output directory does not exist. Please provide a valid directory.")
      }
      # Combine output directory with file name
      pdf_path <- file.path(output_dir, pdf_file_name)
    } else {
      pdf_path <- pdf_file_name  # Default to current working directory
    }

    pdf(file = pdf_path, width = plot_width, height = plot_height)

    # 1. Print the hull plots (2 or 3 per page)
    hull_plots <- lapply(all_plots, function(plot_set) plot_set$hull_plot)
    hull_pages <- split(hull_plots, ceiling(seq_along(hull_plots) / 2))
    for (page in hull_pages) {
      print(patchwork::wrap_plots(page, ncol = 1))
    }

    # 2. Print individual hull plots if show_all_hulls is TRUE
    if (show_all_hulls) {
      for (plot_set in all_plots) {
        individual_hull_pages <- split(plot_set$individual_hull_plots, ceiling(seq_along(plot_set$individual_hull_plots) / 2))
        for (page in individual_hull_pages) {
          print(patchwork::wrap_plots(page, ncol = 1))
        }
      }
    }

    # 3. Print the contour plots (2 per page) if show_all_contours is TRUE
    if (show_all_contours) {
      for (plot_set in all_plots) {
        contour_pages <- split(plot_set$contour_plots, ceiling(seq_along(plot_set$contour_plots) / 2))
        for (page in contour_pages) {
          print(patchwork::wrap_plots(page, ncol = 1))
        }
      }
    }

    # 4. Print the single combined boxplot for all columns
    print(boxplot)

    # 5. **Print Each Table for Each Column Pair** if show_table is TRUE
    if (show_table && length(combined_tables) > 0) {
      for (table_name in names(combined_tables)) {
        print(combined_tables[[table_name]])
      }
    }

    dev.off()
    message(paste("Plots exported to", pdf_file_name))
  }

  return(list(hull_and_contours = all_plots, boxplot = boxplot, tables = combined_tables))
}
