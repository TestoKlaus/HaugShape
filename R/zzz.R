# Package setup and configuration for HaugShape

# Global variables and constants used across the package
# This helps avoid R CMD CHECK notes about undefined global variables

# ggplot2 aesthetics and data references
globalVariables(c(
  # Common ggplot2 aesthetics
  "x", "y", "z", "group", "color", "fill", "shape", "size", "alpha", "linetype",

  # dplyr and data manipulation
  ".data", "cluster", "center_x", "center_y",

  # Statistical functions
  "PC1", "PC2", "PCx", "PCy",

  # Shape analysis variables
  "shape_id", "specimen", "hull_points",

  # Plot elements
  "xmin", "xmax", "ymin", "ymax", "density", "level",

  # Group operations
  "n", "count", "proportion",

  # File operations
  "file_path", "output_path", "input_dir", "output_dir"
))# Package startup message
.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "HaugShape ", utils::packageVersion("HaugShape"), " loaded.\n",
    "Advanced Shape Analysis and Morphometric Visualization Tools\n",
    "Use run_app() to launch the interactive interface.\n",
    "Visit https://github.com/TestoKlaus/HaugShape for documentation."
  )
}

# Package namespace imports that need to be explicit
.onLoad <- function(libname, pkgname) {
  # Check for suggested packages and provide informative messages
  suggested_packages <- list(
    Momocs = "morphometric analysis with shape_analysis() and shape_plot_momocs()", # nolint: line_length_linter
    magick = "image processing functions (convert_png_to_image, split_image, etc.)", # nolint: line_length_linter
    shiny = "interactive application with run_app()",
    DT = "enhanced data tables in the Shiny app",
    colourpicker = "color selection in the Shiny interface",
    shinyFiles = "file browser functionality in the Shiny app"
  )

  # Store information about available optional functionality
  options(HaugShape.suggested_packages = suggested_packages)

  invisible()
}

# Helper function to check and suggest missing packages
check_suggested_package <- function(package_name, feature_name = NULL,
                                   quiet = FALSE) {
  if (!requireNamespace(package_name, quietly = TRUE)) {
    if (!quiet) {
      suggested <- getOption("HaugShape.suggested_packages", list())
      feature_desc <- if (!is.null(feature_name)) {
        feature_name
      } else if (package_name %in% names(suggested)) {
        suggested[[package_name]]
      } else {
        "this functionality"
      }

      message("Package '", package_name,
              "' is not installed but is recommended for ",
              feature_desc, ".\n",
              "Install it with: install.packages('", package_name, "')")
    }
    return(FALSE)
  }
  return(TRUE)
}

# Validate HaugShape installation
validate_installation <- function() {
  required_files <- c(
    "inst/shiny/app.R"
  )

  package_path <- system.file(package = "HaugShape")

  for (file in required_files) {
    full_path <- file.path(package_path, file)
    if (!file.exists(full_path)) {
      warning("Missing required file: ", file,
              ". Package installation may be incomplete.",
              call. = FALSE)
    }
  }

  invisible(TRUE)
}

# Export the check function for internal use
#' @keywords internal
.check_suggested_package <- check_suggested_package
