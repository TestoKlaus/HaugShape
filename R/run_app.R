#' Launch HaugShape Interactive Application
#'
#' Starts the interactive Shiny application for HaugShape, providing a 
#' user-friendly interface for shape analysis, plotting, and image processing.
#' 
#' @param port Port number to run the application on. If `NULL` (default), 
#'   Shiny will automatically select an available port.
#' @param host Host IP address. Default is "127.0.0.1" (localhost).
#' @param launch.browser Logical indicating whether to launch the app in 
#'   the default web browser. Default is `TRUE`.
#' @param quiet Logical indicating whether to suppress startup messages. 
#'   Default is `FALSE`.
#'   
#' @return No return value. Starts the Shiny application.
#' 
#' @details
#' The Shiny application provides interactive access to all HaugShape 
#' functionality including:
#' \itemize{
#'   \item Advanced shape plotting with customizable aesthetics
#'   \item Image processing and shape preparation tools
#'   \item Morphometric analysis with Fourier descriptors
#'   \item Data upload and visualization
#'   \item Export capabilities for plots and results
#'   \item Contributors guide and documentation
#' }
#' 
#' @examples
#' \dontrun{
#' # Launch the app with default settings
#' run_app()
#' 
#' # Launch on specific port without opening browser
#' run_app(port = 3838, launch.browser = FALSE)
#' 
#' # Launch with quiet startup
#' run_app(quiet = TRUE)
#' }
#' 
#' @seealso \link{shape_plot}, \link{Haug_panel}, \link{shape_analysis}
#' @export
run_app <- function(port = NULL, 
                    host = "127.0.0.1", 
                    launch.browser = TRUE,
                    quiet = FALSE) {
  
  # Validate inputs
  if (!is.null(port) && (!is.numeric(port) || length(port) != 1 || port < 1 || port > 65535)) {
    stop("'port' must be a single integer between 1 and 65535, or NULL", call. = FALSE)
  }
  if (!is.character(host) || length(host) != 1) {
    stop("'host' must be a single character string", call. = FALSE)
  }
  if (!is.logical(launch.browser) || length(launch.browser) != 1) {
    stop("'launch.browser' must be a single logical value", call. = FALSE)
  }
  if (!is.logical(quiet) || length(quiet) != 1) {
    stop("'quiet' must be a single logical value", call. = FALSE)
  }
  
  # Find the Shiny app directory
  app_dir <- system.file("shiny", package = "HaugShape")
  if (app_dir == "" || !dir.exists(app_dir)) {
    stop(
      "Could not find the Shiny app directory. ",
      "Please try re-installing HaugShape with: ",
      "devtools::install_github('TestoKlaus/HaugShape')",
      call. = FALSE
    )
  }
  
  # Check for required app file
  app_file <- file.path(app_dir, "app.R")
  if (!file.exists(app_file)) {
    stop(
      "Could not find app.R in the Shiny directory. ",
      "The package installation may be corrupted. Please re-install HaugShape.",
      call. = FALSE
    )
  }
  
  if (!quiet) {
    message("Starting HaugShape Shiny application...")
    message("App directory: ", app_dir)
    if (!is.null(port)) {
      message("Running on port: ", port)
    }
  }
  
  # Check for suggested packages that enhance functionality
  check_suggested_packages <- function() {
    suggested_pkgs <- c("colourpicker", "shinyFiles", "DT", "readxl", "fs")
    missing_pkgs <- suggested_pkgs[!sapply(suggested_pkgs, requireNamespace, quietly = TRUE)]
    
    if (length(missing_pkgs) > 0 && !quiet) {
      message("Note: Some optional packages are not installed. For full functionality, consider installing:")
      message("install.packages(c(", paste0("'", missing_pkgs, "'", collapse = ", "), "))")
    }
  }
  
  check_suggested_packages()
  
  # Launch the app
  tryCatch({
    shiny::runApp(
      appDir = app_dir,
      port = port,
      host = host,
      launch.browser = launch.browser,
      display.mode = "normal",
      quiet = quiet
    )
  }, error = function(e) {
    stop("Failed to start Shiny application: ", e$message, call. = FALSE)
  })
}
