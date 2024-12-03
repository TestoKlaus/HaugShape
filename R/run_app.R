#' Run the Shiny Application
#'
#' Launches the Shiny application provided with the package.
#' @import shiny
#' @export
run_app <- function() {
  app_dir <- system.file("shiny", package = "HaugShape")
  if (app_dir == "") {
    stop("Could not find the Shiny app directory. Try re-installing `HaugShape`.", call. = FALSE)
  }
  shiny::runApp(app_dir, display.mode = "normal")
}
