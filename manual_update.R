# Manual Package Update Instructions for HaugShape
# ==================================================
# 
# Since automated installation had issues, please run these commands
# manually in RStudio or R Console:

# Step 1: Update core packages
install.packages(c(
  "dplyr",
  "ggplot2", 
  "patchwork",
  "rlang",
  "scales"
), dependencies = TRUE)

# Step 2: Update suggested packages  
install.packages(c(
  "colourpicker",
  "DT",
  "fs", 
  "magick",
  "Momocs",
  "openxlsx",
  "readxl",
  "shiny",
  "shinyFiles",
  "testthat"
), dependencies = TRUE)

# Step 3: Update development tools
install.packages(c(
  "devtools",
  "roxygen2", 
  "pkgdown",
  "usethis"
), dependencies = TRUE)

# Step 4: Verify installation
library(dplyr)
library(ggplot2)
library(shiny)
library(devtools)

# Step 5: Update your package documentation
setwd("c:/Users/colin/OneDrive/Desktop/HaugShape")
devtools::document()

# Step 6: Check your package
devtools::check()

cat("✅ Package update complete! Your HaugShape package is now modernized.\n")