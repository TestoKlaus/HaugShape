#' Analyze Shape Images and Perform PCA
#'
#' This function processes a directory of shape images, normalizes the shapes,
#' performs Elliptical Fourier Analysis (EFA) and Principal Component Analysis (PCA),
#' displays a plot of PC contributions, and saves PCA scores to an Excel file.
#'
#' @param shape_dir A character string specifying the directory containing shape files
#' (images in JPG format). The directory must exist and contain at least one valid JPG file.
#' @param norm Logical. Should the Fourier descriptors be normalized? If TRUE, shapes are
#' normalized to the first harmonic. If FALSE, shapes are normalized to the longest radius. Default is TRUE.
#' @param output_dir A character string specifying the directory where the Excel file will be saved.
#' If NULL, the file will be saved in the working directory.
#' @param output_file A character string specifying the name of the Excel file where PCA scores will
#' be saved. Default is "shape_analysis.xlsx".
#' @param num_pcs Integer. The number of principal components to display in the PC contribution plot. Default is 10.
#' @param start_point A character string specifying the starting point for alignment during shape normalization.
#' Options are `"up"`, `"left"`, `"down"`, or `"right"`. Default is `"left"`.
#'
#' @return A character string indicating the file path where PCA scores were successfully saved.
#' Additionally, a plot of PC contributions for the specified number of principal components is displayed.
#'
#' @details
#' The function uses the `Momocs` package to import, normalize, and analyze shape outlines:
#' - **Normalization**: The shapes are centered, scaled, and aligned to a consistent orientation.
#' - **Elliptical Fourier Analysis (EFA)**: Generates Fourier descriptors for the shape outlines.
#' - **Principal Component Analysis (PCA)**: Decomposes the Fourier descriptors into principal components.
#'
#' PCA scores for all analyzed shapes are saved to an Excel file. A plot of the contributions of the specified
#' number of principal components is displayed.
#'
#' @section Dependencies:
#' This function requires the following R packages:
#' - \pkg{Momocs} for shape analysis.
#' - \pkg{openxlsx} for writing PCA scores to an Excel file.
#'
#' @examples
#' \dontrun{
#' # Analyze shape images in a directory and save results
#' shape_analysis(
#'   shape_dir = "path/to/shape/directory",
#'   norm = TRUE,
#'   output_dir = "path/to/output/directory",
#'   output_file = "shape_analysis_results.xlsx",
#'   num_pcs = 5,
#'   start_point = "down"
#' )
#' }
#'
#' @export
shape_analysis <- function(shape_dir, norm = TRUE, output_dir = NULL,
                           output_file = "shape_analysis.xlsx", num_pcs = 10, start_point = "left") {
  if (!requireNamespace("Momocs", quietly = TRUE)) {
    stop("The Momocs package is required but is not installed. Please install it.")
  }
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("The openxlsx package is required but is not installed. Please install it.")
  }
  if (!dir.exists(shape_dir)) {
    stop("The specified shape directory does not exist.")
  }

  # Validate start_point
  valid_start_points <- c("up", "left", "down", "right")
  if (!start_point %in% valid_start_points) {
    stop(sprintf("Invalid `start_point`: '%s'. Valid options are: %s",
                 start_point, paste(valid_start_points, collapse = ", ")))
  }

  # Get the list of shape files
  shape_files <- list.files(shape_dir, pattern = "\\.(jpg|jpeg)$", full.names = TRUE, ignore.case = TRUE)
  if (length(shape_files) == 0) {
    stop("No JPG files found in the specified directory.")
  }

  # Set output directory and file path
  if (is.null(output_dir)) {
    output_dir <- getwd()
  }
  if (!dir.exists(output_dir)) {
    stop("The specified output directory does not exist.")
  }
  output_file_path <- file.path(output_dir, output_file)

  # Import shapes and convert to Coo object
  shapes <- Momocs::import_jpg(shape_files) %>% Momocs::Out()
  print("Shapes after import:")
  print(shapes)

  # Normalize shapes
  normalized_shapes <- shapes %>%
    Momocs::coo_center() %>%
    Momocs::coo_scale() %>%
    Momocs::coo_slidedirection(start_point)

  # Perform Elliptical Fourier Analysis
  efourier_results <- Momocs::efourier(normalized_shapes, norm = norm)

  # Perform PCA on the Fourier descriptors
  pca_results <- tryCatch({
    Momocs::PCA(efourier_results, center = TRUE)
  }, error = function(e) {
    stop("PCA failed: ", e$message)
  })

  # Extract PCA scores
  scores <- as.data.frame(pca_results$x)
  scores$ID <- rownames(scores)
  scores <- scores[, c("ID", setdiff(names(scores), "ID"))]

  # Attempt to write to Excel
  tryCatch({
    openxlsx::write.xlsx(scores, file = output_file_path, rowNames = FALSE)
    message("Excel file successfully written to: ", output_file_path)
  }, error = function(e) {
    stop("Failed to write Excel file: ", e$message)
  })

  # Generate PCA summary
  pca_summary <- capture.output(summary(pca_results))

  # Generate PC contribution plot
  pc_contrib_plot <- Momocs::PCcontrib(pca_results, nax = 1:num_pcs, sd.r = c(-2, -1, 0, 1, 2))

  # Return results as a list
  return(list(summary = paste(pca_summary, collapse = "\n"), pc_contrib_plot = pc_contrib_plot))
}
