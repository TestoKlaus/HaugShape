#' Get Specimen Data Tables for Convex Hulls of Multiple Column Pairs
#'
#' This function identifies the specimens from a data frame used to create convex hulls
#' based on multiple specified column pairs for specific groups.
#' It returns a list of ggplot tables, each containing the rows forming the hull with selected columns:
#' the first column, group_col, x_col, and y_col for each group and column pair, with a title added.
#' If a group has fewer than three points or no valid points, it returns a table of all points in the group
#' or an empty table.
#'
#' @param data A data frame containing the data to analyze.
#' @param x_cols A character vector representing the x-axis column names.
#' @param y_cols A character vector representing the y-axis column names.
#' @param group_col A character string representing the grouping column name.
#' @param group_vals (Optional) A vector of specific values within the group_col to filter by for hull calculation.
#'                  If NULL, tables are generated for all groups in `group_col`.
#'
#' @return A list of ggplot objects, each showing a table with the first column, group_col, x_col, and y_col
#'         for specimens on the convex hull, all points in the group if fewer than three points,
#'         or an empty table if no valid points are available.
#'
#' @export
#' @import ggplot2
#' @importFrom ggpubr ggtexttable
get_hull_specimen_table <- function(data, x_cols, y_cols, group_col, group_vals = NULL) {
  if (length(x_cols) != length(y_cols)) {
    stop("x_cols and y_cols must have the same length.")
  }

  # Get unique groups if group_vals is not provided
  if (is.null(group_vals)) {
    group_vals <- unique(data[[group_col]])
  }

  # Initialize a list to store tables for each group and column pair
  tables <- list()

  # Loop over each column pair
  for (i in seq_along(x_cols)) {
    x_col <- x_cols[i]
    y_col <- y_cols[i]

    # Loop over each group
    for (group_val in group_vals) {
      # Filter data for the specified group
      group_data <- data[data[[group_col]] == group_val, ]

      # Remove rows with non-finite values in x_col or y_col
      group_data <- group_data[is.finite(group_data[[x_col]]) & is.finite(group_data[[y_col]]), ]

      # Check if there are any rows left after filtering
      if (nrow(group_data) == 0) {
        message(paste("No valid points for group:", group_val, "with columns:", x_col, "and", y_col))
        # Create an empty table plot
        empty_table <- data.frame(Message = paste("No valid points for group:", group_val, "with columns:", x_col, "and", y_col))
        table_plot <- ggpubr::ggtexttable(empty_table, rows = NULL, theme = ggpubr::ttheme("classic"))
      } else {
        # Determine the name of the first column dynamically
        first_col_name <- names(group_data)[1]

        # Check if there are enough points to create a hull
        if (nrow(group_data) >= 3) {
          # Calculate the convex hull and get row indices
          hull_indices <- grDevices::chull(group_data[[x_col]], group_data[[y_col]])
          # Subset the data to get only the relevant columns and rows that form the hull
          hull_data <- group_data[hull_indices, c(first_col_name, group_col, x_col, y_col), drop = FALSE]
        } else {
          # If fewer than 3 points, just include all points in the group
          hull_data <- group_data[, c(first_col_name, group_col, x_col, y_col), drop = FALSE]
        }

        # Convert the hull data to a ggplot table
        table_plot <- ggpubr::ggtexttable(
          hull_data,
          rows = NULL,  # Disable row names
          theme = ggpubr::ttheme("classic")
        )
      }

      # Create a title for the table that includes x_col and y_col, and align it closely
      title_text <- paste("Specimens for hull of group:", group_val, "for", x_col, "vs.", y_col)
      title_plot <- ggplot2::ggplot() +
        ggplot2::labs(title = title_text) +
        ggplot2::theme_void() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(size = 12, face = "bold", hjust = 0.5, vjust = -1)
        )

      # Combine the title and the table vertically, bringing them closer together
      combined_plot <- patchwork::wrap_plots(title_plot, table_plot, ncol = 1, heights = c(0.2, 1))

      # Store the combined plot in the list with a key indicating the group and column pair
      key <- paste0("Group_", group_val, "_", x_col, "_vs_", y_col)
      tables[[key]] <- combined_plot
    }
  }

  # Return the list of tables
  return(tables)
}
