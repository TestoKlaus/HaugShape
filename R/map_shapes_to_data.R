#' Map Shapes to Data
#'
#' This function maps shapes from a folder of `.jpeg` files to a specified column in a data frame.
#' The shape files are imported and processed into `Out` objects using the `Momocs` package.
#' If a match between the file names (excluding the `.jpeg` extension) and the values in the specified ID column is found,
#' the corresponding shape is added to the data frame in a new column named `shape`.
#' Otherwise, `NULL` is assigned to rows without a match.
#'
#' @param data A data frame containing the IDs used for matching with shape file names.
#' @param id_col A character string specifying the column in the data frame that contains IDs to be matched with the shape file names.
#' @param shape_folder A character string specifying the folder containing the `.jpeg` shape files.
#'
#' @return A data frame with an additional `shape` column containing imported shapes as `Out` objects or `NULL` for unmatched rows.
#'
#' @import Momocs
#'
#' @export
map_shapes_to_data <- function(data, id_col, shape_folder) {
  # Get full paths and file names
  jpg_files <- list.files(shape_folder, full.names = TRUE)
  jpg_file_names <- gsub("\\.(jpeg|jpg)$", "", basename(jpg_files), ignore.case = TRUE)  # Handle .jpeg and .jpg files

  # Match IDs to file names
  matches <- match(data[[id_col]], jpg_file_names)

  # Map shapes into the data
  data$shape <- lapply(matches, function(idx) {
    if (!is.na(idx)) {
      import_jpg(jpg_files[idx]) %>% Out()  # Load and process the shape
    } else {
      NULL  # No match
    }
  })

  return(data)
}

