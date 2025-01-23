#' Morph Two Shapes into an Intermediate Binary Shape
#'
#' This function takes two grayscale image files containing shapes (black on white background),
#' computes a gradient representing the distance from the shapes, and generates an intermediate
#' binary shape based on a threshold.
#'
#' @param image1_path A string specifying the file path to the first image. The image should be
#'   in grayscale format, with black shapes on a white background.
#' @param image2_path A string specifying the file path to the second image. The image should
#'   also be in grayscale format.
#' @param output_path A string specifying the file path where the resulting binary image
#'   will be saved.
#' @param threshold A numeric value between 0 and 1 specifying the cutoff for thresholding
#'   the gradient. Pixels with gradient values above the threshold are set to white
#'   (\code{1}), and those below are set to black (\code{0}). Default is \code{0.1}.
#' @param gamma A numeric value greater than 0 to control the gamma correction applied to
#'   the gradient. Lower values (\code{< 1}) enhance contrast for darker regions, while
#'   higher values (\code{> 1}) enhance contrast for lighter regions. Default is \code{1}.
#'
#' @details
#' The function performs the following steps:
#' - Loads two grayscale images.
#' - Resizes the first image to match the dimensions of the second image.
#' - Computes the distance transform for each image, calculating the distance of each pixel
#'   to the nearest black pixel (\code{value = 0}).
#' - Normalizes the distance transform values to a range of \code{[0, 1]}.
#' - Blends the distance transforms to create a combined gradient.
#' - Applies gamma correction to enhance contrast in the gradient.
#' - Thresholds the gradient to create a binary intermediate shape.
#' - Saves the resulting binary shape as a new image at the specified \code{output_path}.
#'
#' @return None. The function saves the resulting binary image to the specified output path.
#'
#' @examples
#' \dontrun{
#' morph_shapes(
#'   image1_path = "circle.jpg",
#'   image2_path = "rectangle.jpg",
#'   output_path = "morphed.jpg",
#'   threshold = 0.5,
#'   gamma = 1.1
#' )
#' }
#'
#' @import imager
#' @export

morph_shapes <- function(image1_path, image2_path, output_path, threshold = 0.1, gamma = 1) {
  library(imager)

  # Load grayscale images
  img1 <- grayscale(load.image(image1_path))
  img2 <- grayscale(load.image(image2_path))

  # Resize img1 to match img2 dimensions
  img1 <- resize(img1, size_x = dim(img2)[1], size_y = dim(img2)[2])

  # Compute distance transforms for black pixels (value = 0)
  dist1 <- imager::distance_transform(img1, value = 0)
  dist2 <- imager::distance_transform(img2, value = 0)

  # Normalize distance transforms to [0, 1]
  dist1 <- as.cimg(dist1 / max(dist1))
  dist2 <- as.cimg(dist2 / max(dist2))

  # Combine distance transforms to create a gradient
  gradient_img <- (dist1 + dist2) / 2

  # Apply gamma correction to enhance the gradient
  gradient_img <- gradient_img ^ gamma

  # Normalize the gradient to [0, 1]
  gradient_img <- gradient_img / max(gradient_img)

  # Apply thresholding to create the intermediate shape
  filtered_img <- as.cimg(gradient_img > threshold)

  # Save the final binary shape
  save.image(filtered_img, output_path)
  cat("Filtered intermediate shape saved to", output_path, "\n")
}

