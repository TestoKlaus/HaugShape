#' Split and Process an Image
#'
#' This function splits a given image into two halves either horizontally or vertically,
#' processes the halves by optionally mirroring them, and saves the resulting images
#' with appropriate names.
#'
#' @param image_path A string specifying the path to the input image file. The file must be
#'   in a format supported by the `magick` package (e.g., JPG, PNG).
#' @param direction A string indicating the direction of the split. Must be one of
#'   \code{"horizontal"} or \code{"vertical"}. Default is \code{"horizontal"}.
#'   - \code{"horizontal"}: Splits the image into upper and lower halves. The lower half
#'     is mirrored vertically.
#'   - \code{"vertical"}: Splits the image into left and right halves. The left half is
#'     mirrored horizontally.
#' @param output_dir A string specifying the directory where the resulting images will
#'   be saved. Defaults to the current working directory (\code{"."}).
#'
#' @details
#' - For a \code{"horizontal"} split, the upper half is saved as \code{"<original_name>_upper_half.<ext>"}
#'   and the lower half (mirrored vertically) is saved as \code{"<original_name>_lower_half.<ext>"}.
#' - For a \code{"vertical"} split, the left half (mirrored horizontally) is saved as
#'   \code{"<original_name>_left_half.<ext>"} and the right half is saved as
#'   \code{"<original_name>_right_half.<ext>"}.
#'
#' @return This function does not return a value. It saves the processed image halves
#'   to the specified output directory and prints their paths as messages.
#'
#' @examples
#' \dontrun{
#' # Split an image vertically and save to the current directory
#' split_image("example.jpg", direction = "vertical", output_dir = ".")
#'
#' # Split an image horizontally and save to a specific directory
#' split_image("example.jpg", direction = "horizontal", output_dir = "output_images")
#' }
#'
#' @import magick
#' @export

# Function to split and process an image
split_image <- function(image_path, direction = c("horizontal", "vertical"), output_dir = ".") {
  # Ensure the direction is valid
  direction <- match.arg(direction)

  # Extract file name and extension
  file_name <- tools::file_path_sans_ext(basename(image_path))
  file_ext <- tools::file_ext(image_path)

  # Load the image
  img <- image_read(image_path)

  # Get image dimensions
  img_info <- image_info(img)
  width <- img_info$width
  height <- img_info$height

  # Process based on the splitting direction
  if (direction == "horizontal") {
    # Split into upper and lower halves
    upper_half <- image_crop(img, geometry = sprintf("%dx%d+0+0", width, height / 2))
    lower_half <- image_crop(img, geometry = sprintf("%dx%d+0+%d", width, height / 2, height / 2))

    # Mirror the lower half vertically
    lower_half <- image_flip(lower_half)

    # Save the images
    upper_path <- file.path(output_dir, paste0(file_name, "_upper_half.", file_ext))
    lower_path <- file.path(output_dir, paste0(file_name, "_lower_half.", file_ext))
    image_write(upper_half, upper_path)
    image_write(lower_half, lower_path)

    message("Saved: ", upper_path)
    message("Saved: ", lower_path)

  } else { # Vertical split
    # Split into left and right halves
    left_half <- image_crop(img, geometry = sprintf("%dx%d+0+0", width / 2, height))
    right_half <- image_crop(img, geometry = sprintf("%dx%d+%d+0", width / 2, height, width / 2))

    # Mirror the left half horizontally
    left_half <- image_flop(left_half)

    # Save the images
    left_path <- file.path(output_dir, paste0(file_name, "_left_half.", file_ext))
    right_path <- file.path(output_dir, paste0(file_name, "_right_half.", file_ext))
    image_write(left_half, left_path)
    image_write(right_half, right_path)

    message("Saved: ", left_path)
    message("Saved: ", right_path)
  }
}
