#' Cut Image in Half Vertically and Retain Right Half
#'
#' This function takes an image, cuts it in half vertically, and retains only the right half.
#' The output file is saved with the same name as the input but in the specified directory.
#'
#' @param file_path The file path to the input image (PNG, JPG, etc.).
#' @param output_dir The directory where the cropped image will be saved. Defaults to the current working directory.
#' @import magick
#' @export
cut_image_right_half <- function(file_path, output_dir = ".") {
  if (!requireNamespace("magick", quietly = TRUE)) {
    stop("The magick package is required but is not installed. Please install it.")
  }

  if (!file.exists(file_path)) {
    stop("Input file does not exist.")
  }

  if (!dir.exists(output_dir)) {
    stop("The specified output directory does not exist.")
  }

  # Load the image
  img <- magick::image_read(file_path)

  # Get image dimensions
  img_info <- magick::image_info(img)
  original_width <- img_info$width
  original_height <- img_info$height

  # Calculate dimensions for the right half
  crop_geometry <- paste0(original_width / 2, "x", original_height, "+", original_width / 2, "+0")

  # Crop the image
  cropped_img <- magick::image_crop(img, geometry = crop_geometry)

  # Derive the output file name and path
  file_name <- basename(file_path)
  output_path <- file.path(output_dir, file_name)

  # Save the cropped image
  magick::image_write(cropped_img, path = output_path, format = tools::file_ext(output_path))

  message("Right half of the image successfully saved to: ", output_path)
}
