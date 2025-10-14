#' Convert and Process PNG Images to JPG or BMP Format
#'
#' Imports PNG images, applies resizing with aspect ratio preservation, 
#' adds padding, and converts to JPG or BMP format for morphometric analysis 
#' workflows.
#'
#' @param file_path Character string specifying the path to the input PNG file.
#' @param output_dir Character string specifying the output directory. 
#'   Defaults to current working directory.
#' @param fixed_dim Named list specifying fixed dimension for resizing. 
#'   Use `list(width = X)` or `list(height = Y)`. If `NULL`, no resizing is performed.
#' @param padding Integer specifying padding in pixels around the image. Default is 10.
#' @param quality Integer between 1-100 specifying JPG quality. Default is 100.
#'   Only applies to JPG format.
#' @param format Character string specifying output format: "jpg" or "bmp". Default is "jpg".
#'   
#' @return Character string of the output file path.
#' 
#' @details 
#' This function is designed for preparing images for morphometric analysis by:
#' \itemize{
#'   \item Maintaining aspect ratios during resizing
#'   \item Adding consistent padding for standardization
#'   \item Converting to appropriate color spaces
#'   \item Ensuring compatibility with analysis software
#' }
#' 
#' The function requires the \code{magick} package for image processing operations.
#' 
#' @examples
#' \dontrun{
#' # Convert single PNG to JPG with fixed width
#' convert_png_to_image(
#'   file_path = "input.png",
#'   output_dir = "output/",
#'   fixed_dim = list(width = 800),
#'   padding = 20,
#'   quality = 95
#' )
#' 
#' # Convert to BMP with fixed height
#' convert_png_to_image(
#'   file_path = "shape.png",
#'   fixed_dim = list(height = 600),
#'   format = "bmp"
#' )
#' }
#' 
#' @seealso \link{cut_image_right_half}, \link{split_image}
#' @export
convert_png_to_image <- function(file_path, 
                                   output_dir = ".", 
                                   fixed_dim = NULL, 
                                   padding = 10, 
                                   quality = 100, 
                                   format = "jpg") {
  
  # Input validation
  if (missing(file_path) || !is.character(file_path) || length(file_path) != 1) {
    stop("'file_path' must be a single character string", call. = FALSE)
  }
  if (!is.character(output_dir) || length(output_dir) != 1) {
    stop("'output_dir' must be a single character string", call. = FALSE)
  }
  if (!is.null(fixed_dim) && (!is.list(fixed_dim) || 
      (!("width" %in% names(fixed_dim)) && !("height" %in% names(fixed_dim))))) {
    stop("'fixed_dim' must be a named list with 'width' or 'height'", call. = FALSE)
  }
  if (!is.numeric(padding) || length(padding) != 1 || padding < 0) {
    stop("'padding' must be a non-negative number", call. = FALSE)
  }
  if (!is.numeric(quality) || length(quality) != 1 || quality < 1 || quality > 100) {
    stop("'quality' must be a number between 1 and 100", call. = FALSE)
  }
  if (!is.character(format) || length(format) != 1) {
    stop("'format' must be a single character string", call. = FALSE)
  }
  
  # Check package availability
  if (!requireNamespace("magick", quietly = TRUE)) {
    stop(
      "Package 'magick' is required but not installed.\n",
      "Install it with: install.packages('magick')",
      call. = FALSE
    )
  }
  
  # Validate file existence
  if (!file.exists(file_path)) {
    stop("Input file does not exist: '", file_path, "'", call. = FALSE)
  }
  
  # Validate and create output directory
  if (!dir.exists(output_dir)) {
    tryCatch({
      dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    }, error = function(e) {
      stop("Cannot create output directory: '", output_dir, "'", call. = FALSE)
    })
  }
  
  # Validate and normalize format
  format <- tolower(trimws(format))
  valid_formats <- c("jpg", "jpeg", "bmp")
  if (!format %in% valid_formats) {
    stop("'format' must be one of: ", paste(valid_formats, collapse = ", "), call. = FALSE)
  }
  
  # Normalize jpeg to jpg
  if (format == "jpeg") format <- "jpg"

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
