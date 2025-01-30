#' Import, Resize, Add Padding, and Convert PNG to JPG or BMP (24-bit)
#'
#' This function imports a PNG image, resizes it to a fixed height or width (proportional resizing),
#' adds a white background for padding, and then saves it as a JPG or BMP (24-bit) file.
#'
#' @param file_path The file path to the input PNG file.
#' @param output_dir The directory where the output file should be saved. Defaults to the current working directory.
#' @param fixed_dim A named list specifying the fixed dimension. Use `list(width = X)` or `list(height = Y)`.
#' @param padding The padding in pixels to add around the resized image. Default is 10 pixels.
#' @param quality JPG quality (0-100). Default is 100 (only applies to JPG format).
#' @param format The output file format. Can be `"jpg"` or `"bmp"`. Default is `"jpg"`.
#' @import magick
#' @export
convert_png_to_image <- function(file_path, output_dir = ".", fixed_dim, padding = 10, quality = 100, format = "jpg") {
  if (!requireNamespace("magick", quietly = TRUE)) {
    stop("The magick package is required but is not installed. Please install it.")
  }

  if (!file.exists(file_path)) {
    stop("Input file does not exist: ", file_path)
  }

  if (!dir.exists(output_dir)) {
    stop("The specified output directory does not exist: ", output_dir)
  }

  # Validate format
  format <- tolower(format)
  if (!(format %in% c("jpg", "bmp"))) {
    stop("Invalid format specified. Use 'jpg' or 'bmp'.")
  }

  # Load the image
  img <- magick::image_read(file_path)

  # Convert to true color (24-bit)
  img <- magick::image_convert(img, colorspace = "RGB")

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

  # Ensure filename has proper extension
  file_name <- sub("\\.png$", "", basename(file_path), ignore.case = TRUE)
  output_path <- file.path(output_dir, paste0(file_name, ".", format))

  # Save the image in the specified format
  if (format == "jpg") {
    magick::image_write(padded_img, path = output_path, format = "jpg", quality = quality)
  } else {
    # Force 24-bit BMP
    padded_img <- magick::image_convert(padded_img, type = "truecolor")  # Ensure 24-bit BMP
    magick::image_write(padded_img, path = output_path, format = "bmp")
  }

  message("Image successfully processed and saved to: ", output_path)
}
