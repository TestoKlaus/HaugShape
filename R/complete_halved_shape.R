#' Complete a Halved Shape into a Symmetrical Image
#'
#' This function takes an image that is already halved vertically, mirrors it,
#' and appends the mirrored half to the left to form a symmetrical image.
#'
#' @param file_path The file path to the input halved image (PNG, JPG, etc.).
#' @param output_dir The directory where the completed symmetrical image will be saved. Defaults to the current working directory.
#' @import magick
#' @export
complete_halved_shape <- function(file_path, output_dir = ".") {
  if (!requireNamespace("magick", quietly = TRUE)) {
    stop("The magick package is required but is not installed. Please install it.")
  }

  if (!file.exists(file_path)) {
    stop("Input file does not exist.")
  }

  if (!dir.exists(output_dir)) {
    stop("The specified output directory does not exist.")
  }

  # Load the halved image
  img <- magick::image_read(file_path)

  # Mirror the halved image
  mirrored_img <- magick::image_flop(img)

  # Combine the mirrored image and the original image
  symmetrical_image <- magick::image_append(c(mirrored_img, img), stack = FALSE)

  # Derive the output file name and path
  file_name <- gsub("\\.png$|\\.jpg$", "_completed.jpg", basename(file_path), ignore.case = TRUE)
  output_path <- file.path(output_dir, file_name)

  # Save the completed image
  magick::image_write(symmetrical_image, path = output_path, format = "jpg")

  message("Completed symmetrical image successfully saved to: ", output_path)
}
