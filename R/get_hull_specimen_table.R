#' Get Combined Specimen Data Table for Convex Hulls of Multiple Column Pairs
#'
#' This function identifies the specimens from a data frame used to create convex hulls
#' based on multiple specified column pairs for specific groups and combines them into a single table.
#' Each group and column pair has a title row with the format "Specimens for hull of group: ... for X vs Y".
#'
#' @param data A data frame containing the data to analyze.
#' @param x_cols A character vector representing the x-axis column names.
#' @param y_cols A character vector representing the y-axis column names.
#' @param group_col A character string representing the grouping column name.
#' @param group_vals (Optional) A vector of specific values within the group_col to filter by for hull calculation.
#'                  If NULL, tables are generated for all groups in `group_col`.
#' @param max_rows_per_page Maximum number of rows per page before splitting. Default is 50.
#'
#' @return A list of ggplot tables, each displaying a portion of the combined data,
#'         with titles as bold rows and automatic pagination if necessary.
#'
#' @export
#' @import ggplot2
#' @importFrom ggpubr ggtexttable ttheme
get_hull_specimen_table <- function(data, x_cols, y_cols, group_col, group_vals = NULL, max_rows_per_page = 50) {
  if (length(x_cols) != length(y_cols)) {
    stop("x_cols and y_cols must have the same length.")
  }

  # Get unique groups if group_vals is not provided
  if (is.null(group_vals)) {
    group_vals <- unique(data[[group_col]])
  }

  # Initialize a data frame to accumulate all rows
  combined_data <- data.frame()

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

      # Determine the name of the first column dynamically
      first_col_name <- names(group_data)[1]

      # Create a title row with empty values in remaining columns
      title_text <- paste("Specimens for hull of group:", group_val, "for", x_col, "vs.", y_col)
      title_row <- data.frame(matrix("", nrow = 1, ncol = length(c(first_col_name, group_col, x_col, y_col))))
      title_row[1, 1] <- title_text  # Only the first cell has the title
      colnames(title_row) <- colnames(group_data[, c(first_col_name, group_col, x_col, y_col), drop = FALSE])

      # Check if there are any rows left after filtering
      if (nrow(group_data) == 0) {
        # Create a placeholder row if there are no valid points
        hull_data <- data.frame(matrix(paste("No valid points for group:", group_val, "with columns:", x_col, "and", y_col),
                                       nrow = 1, ncol = length(c(first_col_name, group_col, x_col, y_col))))
        colnames(hull_data) <- colnames(group_data[, c(first_col_name, group_col, x_col, y_col), drop = FALSE])
      } else {
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
      }

      # Combine the title row and hull data without adding extra columns
      combined_data <- rbind(combined_data, title_row, hull_data)
    }
  }

  # Define theme with adjusted font size and cell padding
  table_theme <- ggpubr::ttheme(
    base_size = if (nrow(combined_data) <= max_rows_per_page) 10 else max(8, round(500 / nrow(combined_data))),
    padding = unit(c(1.2, 1.2), "mm")
  )

  # Split the table if it has more rows than max_rows_per_page
  pages <- split(combined_data, ceiling(seq_len(nrow(combined_data)) / max_rows_per_page))

  # Create ggtexttable for each page
  tables <- lapply(pages, function(page_data) {
    table <- ggpubr::ggtexttable(
      page_data,
      rows = NULL,
      theme = table_theme
    )
    return(table)
  })

  return(tables)
}
