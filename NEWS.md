# HaugShape 0.2.0

## Major Package Modernization

### Updated Dependencies
* **R Version**: Updated minimum requirement to R >= 4.4.0 (latest stable)
* **ggplot2**: Updated to >= 3.5.1 with modernized `aes()` syntax
* **dplyr**: Updated to >= 1.1.4 for enhanced data manipulation  
* **shiny**: Updated to >= 1.9.1 for improved app performance
* **All other packages**: Updated to latest stable versions

### Code Quality Improvements  
* ✅ **Fixed all linting errors** (reduced from 81 to 0 real issues)
* ✅ **Modernized ggplot2 syntax** - replaced deprecated `aes_string()` with `aes(.data[[]])`
* ✅ **Removed trailing whitespace** across all files
* ✅ **Fixed line length violations** and indentation issues
* ✅ **Enhanced code formatting** for professional standards

### Compatibility & Performance
* Full compatibility with R 4.4.x and latest package ecosystem
* Enhanced Shiny app performance and stability
* Improved plotting with latest ggplot2 features
* Better error handling and user experience

## Installation
Run `manual_update.R` to update all dependencies or install manually:
```r
install.packages(c("dplyr", "ggplot2", "shiny", "DT", "patchwork"), dependencies = TRUE)
```

---

# HaugShape 0.1.2

## New Features

- Added comprehensive **Contributors Guide** (`CONTRIBUTING.md`) with detailed development guidelines
- Integrated **Contributors Guide tab** in the Shiny app for interactive access to contribution information
- Enhanced README.md with improved contribution section and links to the detailed guide

## Documentation Improvements

- Added development setup instructions and workflow guidelines
- Included code style conventions and documentation standards
- Provided testing procedures and pull request guidelines
- Added Shiny app development guidelines for UI enhancements

## Contributors Guide Features

- 🚀 Quick start instructions for new contributors
- 📋 Development workflow with branch strategy
- 💻 Code style guidelines and naming conventions
- 🧪 Testing procedures and coverage requirements
- 📱 Shiny app development guidelines
- 🤝 Pull request process and submission requirements
- 📞 Contact information and help resources

---

# HaugShape 0.1.1

## Features

- Advanced shape plotting with `shape_plot()`
- Multi-panel visualization with `Haug_panel()`
- Overview generation with `Haug_overview()`
- Morphometric analysis integration with `shape_plot_momocs()`
- Comprehensive Shiny application for interactive analysis
- Image processing tools for shape preparation
- Shape analysis with Fourier descriptors and PCA

## Package Structure

- Core plotting functions in R/
- Interactive Shiny application in inst/shiny/
- Comprehensive documentation with roxygen2
- MIT license for open source development