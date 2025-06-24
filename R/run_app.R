#' Run the Shiny Application
#'
#' Launches the Shiny application provided with the package.
#' @import shiny
#' @param ... Additional arguments passed to shiny::runApp()
#' @export
run_app <- function(...) {
  app_dir <- system.file("shiny", package = "HaugShape")
  if (app_dir == "") {
    stop("Could not find the Shiny app directory. Ensure the package is installed correctly and contains 'inst/shiny/app.R'.", call. = FALSE)
  }
  shiny::runApp(app_dir, display.mode = "normal", ...)
}
