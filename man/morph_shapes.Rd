% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/morph_shapes.R
\name{morph_shapes}
\alias{morph_shapes}
\title{Morph Two Shapes into an Intermediate Binary Shape}
\usage{
morph_shapes(image1_path, image2_path, output_path, threshold = 0.1, gamma = 1)
}
\arguments{
\item{image1_path}{A string specifying the file path to the first image. The image should be
in grayscale format, with black shapes on a white background.}

\item{image2_path}{A string specifying the file path to the second image. The image should
also be in grayscale format.}

\item{output_path}{A string specifying the file path where the resulting binary image
will be saved.}

\item{threshold}{A numeric value between 0 and 1 specifying the cutoff for thresholding
the gradient. Pixels with gradient values above the threshold are set to white
(\code{1}), and those below are set to black (\code{0}). Default is \code{0.1}.}

\item{gamma}{A numeric value greater than 0 to control the gamma correction applied to
the gradient. Lower values (\code{< 1}) enhance contrast for darker regions, while
higher values (\code{> 1}) enhance contrast for lighter regions. Default is \code{1}.}
}
\value{
None. The function saves the resulting binary image to the specified output path.
}
\description{
This function takes two grayscale image files containing shapes (black on white background),
computes a gradient representing the distance from the shapes, and generates an intermediate
binary shape based on a threshold.
}
\details{
The function performs the following steps:
\itemize{
\item Loads two grayscale images.
\item Resizes the first image to match the dimensions of the second image.
\item Computes the distance transform for each image, calculating the distance of each pixel
to the nearest black pixel (\code{value = 0}).
\item Normalizes the distance transform values to a range of \code{[0, 1]}.
\item Blends the distance transforms to create a combined gradient.
\item Applies gamma correction to enhance contrast in the gradient.
\item Thresholds the gradient to create a binary intermediate shape.
\item Saves the resulting binary shape as a new image at the specified \code{output_path}.
}
}
\examples{
\dontrun{
morph_shapes(
  image1_path = "circle.jpg",
  image2_path = "rectangle.jpg",
  output_path = "morphed.jpg",
  threshold = 0.5,
  gamma = 1.1
)
}

}
