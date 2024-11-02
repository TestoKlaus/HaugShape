# HaugShape

HaugShape is an R package designed for advanced shape analyses and morphometric visualizations, especially useful in morphometrics research. The package provides customizable plotting functions for visualizing shapes, convex hulls, and contours, enabling the creation of publication-ready figures.

Features
Flexible Shape Plotting: Generate customizable scatterplots with convex hulls and optional contour lines.
Group Analysis: Easily visualize and compare shapes across multiple groups with separate or combined plots.
Customizable Styles: Choose from styles like Haug, inverted_Haug, and publication for tailored visual output.
Export Options: Easily export plots as high-resolution TIFFs or multi-page PDFs for comprehensive reporting.

# Installation
To install HaugShape from GitHub:
# Install the devtools package if you haven't already
install.packages("devtools")

# Install HaugShape from GitHub
devtools::install_github("TestoKlaus/HaugShape")


# Usage
library(HaugShape)

# Basic scatterplot with convex hulls for specified groups
data <- data.frame(
  PC1 = rnorm(100),
  PC2 = rnorm(100),
  group = sample(c("A", "B", "C"), 100, replace = TRUE)
)

# Generate a plot
shape_plot(data, x_col = "PC1", y_col = "PC2", group_col = "group", show_hulls = TRUE)


# Main Functions
shape_plot(): Create scatterplots with convex hulls and contours, with extensive customization for colors, labels, and plot aesthetics.
Haug_panel(): Display panels of hulls or contours for multiple groups in one overview.
Haug_overview(): Generate multi-page PDF overviews for hulls, contours, and combined group boxplots.
shape_plot_momocs(): Plot PCA results with shapes at convex hull points for morphometric analysis with Momocs.


# Contributing
Feedback and contributions are welcome! Feel free to open issues or submit pull requests on GitHub.
