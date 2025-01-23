#' Split and Process an Image
#'
#' This function splits a given image at a user-specified position, either horizontally or vertically,
#' processes the halves by optionally mirroring them, and saves the resulting images with appropriate names.
#'
#' @param image_path A string specifying the path to the input image file. The file must be
#'   in a format supported by the `magick` package (e.g., JPG, PNG).
#' @param direction A string indicating the direction of the split. Must be one of
#'   \code{"horizontal"} or \code{"vertical"}. Default is \code{"horizontal"}.
#'   - \code{"horizontal"}: Splits the image into two parts along a horizontal line.
#'   - \code{"vertical"}: Splits the image into two parts along a vertical line.
#' @param split_position A numeric value between \code{0} and \code{1}, indicating the
#'   relative position of the split. Default is \code{0.5} (split at the midpoint).
#' @param output_dir A string specifying the directory where the resulting images will
#'   be saved. Defaults to the current working directory (\code{"."}).
#'
#' @details
#' - For a \code{"horizontal"} split, the split occurs at \code{split_position * height}.
#' - For a \code{"vertical"} split, the split occurs at \code{split_position * width}.
#' - The function saves the resulting halves with filenames indicating their relative positions.
#'
#' @return This function does not return a value. It saves the processed image halves
#'   to the specified output directory and prints their paths as messages.
#'
#' @examples
#' \dontrun{
#' # Split an image vertically at 30% of the width
#' split_image("example.jpg", direction = "vertical", split_position = 0.3, output_dir = ".")
#'
#' # Split an image horizontally at 75% of the height
#' split_image("example.jpg", direction = "horizontal", split_position = 0.75, output_dir = "output_images")
#' }
#'
#' @import magick
#' @export

split_image <- function(image_path, direction = c("horizontal", "vertical"), split_position = 0.5, output_dir = ".") {
  # Ensure the direction is valid
  direction <- match.arg(direction)

  # Validate split_position
  if (split_position < 0 || split_position > 1) {
    stop("split_position must be between 0 and 1.")
  }

  # Extract file name and extension
  file_name <- tools::file_path_sans_ext(basename(image_path))
  file_ext <- tools::file_ext(image_path)

  # Load the image
  img <- magick::image_read(image_path)

  # Get image dimensions
  img_info <- magick::image_info(img)
  width <- img_info$width
  height <- img_info$height

  # Calculate the split position in pixels
  if (direction == "horizontal") {
    split_pixel <- round(split_position * height)
    upper_half <- magick::image_crop(img, geometry = sprintf("%dx%d+0+0", width, split_pixel))
    lower_half <- magick::image_crop(img, geometry = sprintf("%dx%d+0+%d", width, height - split_pixel, split_pixel))

    # Mirror the lower half vertically
    lower_half <- magick::image_flip(lower_half)

    # Save the images with appropriate names
    upper_path <- file.path(output_dir, paste0(file_name, "_upper_half.", file_ext))
    lower_path <- file.path(output_dir, paste0(file_name, "_lower_half.", file_ext))
    magick::image_write(upper_half, upper_path)
    magick::image_write(lower_half, lower_path)

    message("Saved: ", upper_path)
    message("Saved: ", lower_path)

  } else { # Vertical split
    split_pixel <- round(split_position * width)
    left_half <- magick::image_crop(img, geometry = sprintf("%dx%d+0+0", split_pixel, height))
    right_half <- magick::image_crop(img, geometry = sprintf("%dx%d+%d+0", width - split_pixel, height, split_pixel))

    # Mirror the left half horizontally
    left_half <- magick::image_flop(left_half)

    # Save the images with appropriate names
    left_path <- file.path(output_dir, paste0(file_name, "_left_half.", file_ext))
    right_path <- file.path(output_dir, paste0(file_name, "_right_half.", file_ext))
    magick::image_write(left_half, left_path)
    magick::image_write(right_half, right_path)

    message("Saved: ", left_path)
    message("Saved: ", right_path)
  }
}
