#' Import, Resize, Add Padding, and Convert PNG to JPG
#'
#' This function imports a PNG image, resizes it to a fixed height or width (proportional resizing),
#' adds a white background for padding, and then saves it as a JPG file.
#'
#' @param file_path The file path to the input PNG file.
#' @param output_dir The directory where the output JPG file should be saved. Defaults to the current working directory.
#' @param fixed_dim A named list specifying the fixed dimension. Use `list(width = X)` or `list(height = Y)`.
#' @param padding The padding in pixels to add around the resized image. Default is 10 pixels.
#' @param quality JPG quality (0-100). Default is 100.
#' @import magick
#' @export
convert_png_to_jpg <- function(file_path, output_dir = ".", fixed_dim, padding = 10, quality = 100) {
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

  # Get original dimensions
  img_info <- magick::image_info(img)
  original_width <- img_info$width
  original_height <- img_info$height

  # Resize while maintaining aspect ratio
  if (!is.null(fixed_dim$width)) {
    new_width <- fixed_dim$width
    new_height <- round((new_width / original_width) * original_height)
  } else if (!is.null(fixed_dim$height)) {
    new_height <- fixed_dim$height
    new_width <- round((new_height / original_height) * original_width)
  } else {
    stop("You must specify either a fixed width or height in `fixed_dim`.")
  }

  resized_img <- magick::image_resize(img, paste0(new_width, "x", new_height))

  # Add padding
  padded_width <- new_width + 2 * padding
  padded_height <- new_height + 2 * padding

  padded_img <- magick::image_extent(
    resized_img,
    geometry = paste0(padded_width, "x", padded_height),
    color = "white"
  )

  # Derive the output file name and path
  file_name <- gsub("\\.png$", ".jpg", basename(file_path), ignore.case = TRUE)
  output_path <- file.path(output_dir, file_name)

  # Save as JPG
  magick::image_write(padded_img, path = output_path, format = "jpg", quality = quality)

  message("Image successfully processed and saved to: ", output_path)
}
