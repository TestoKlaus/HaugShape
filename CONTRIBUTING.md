# Contributing to HaugShape

We welcome contributions to HaugShape! This guide will help you get started with developing and contributing to this R package for advanced shape analyses and morphometric visualizations.

## Table of Contents

1. [Getting Started](#getting-started)
2. [Development Setup](#development-setup)
3. [Development Workflow](#development-workflow)
4. [Code Style Guidelines](#code-style-guidelines)
5. [Documentation Standards](#documentation-standards)
6. [Testing](#testing)
7. [Submitting Contributions](#submitting-contributions)
8. [Shiny App Development](#shiny-app-development)
9. [Package Structure](#package-structure)
10. [Getting Help](#getting-help)

## Getting Started

### Prerequisites

Before contributing to HaugShape, ensure you have:

- **R** (version 4.0 or higher)
- **RStudio** (recommended IDE)
- **Git** for version control
- **Rtools** (on Windows) for package building

### Required R Packages

Install the development dependencies:

```r
# Development tools
install.packages(c("devtools", "roxygen2", "testthat", "usethis"))

# Package dependencies
install.packages(c(
  "shiny", "dplyr", "ggplot2", "patchwork", "MASS", 
  "RColorBrewer", "grDevices", "grid", "rlang", "scales",
  "colourpicker", "shinyFiles", "DT", "readxl", "fs", "magick"
))

# Optional: Morphometric analysis
install.packages("Momocs")
```

## Development Setup

### 1. Fork and Clone the Repository

```bash
git clone https://github.com/YourUsername/HaugShape.git
cd HaugShape
```

### 2. Set up the R Development Environment

```r
# Load the package for development
devtools::load_all()

# Check package integrity
devtools::check()

# Build documentation
devtools::document()
```

### 3. Install Package in Development Mode

```r
devtools::install(".", dependencies = TRUE)
```

## Development Workflow

### Branch Strategy

- **stable**: Main production branch
- **feature/**: New features or enhancements
- **bugfix/**: Bug fixes
- **hotfix/**: Critical fixes for production

### Making Changes

1. **Create a new branch** from `stable`:
   ```bash
   git checkout stable
   git pull origin stable
   git checkout -b feature/your-feature-name
   ```

2. **Make your changes** following the code style guidelines

3. **Test your changes**:
   ```r
   devtools::check()
   devtools::test()
   ```

4. **Update documentation** if needed:
   ```r
   devtools::document()
   ```

5. **Commit your changes**:
   ```bash
   git add .
   git commit -m "feat: descriptive commit message"
   ```

### Commit Message Convention

Use conventional commit format:
- `feat:` New features
- `fix:` Bug fixes
- `docs:` Documentation changes
- `style:` Code style changes
- `refactor:` Code refactoring
- `test:` Adding or updating tests
- `chore:` Maintenance tasks

## Code Style Guidelines

### R Code Style

Follow these conventions for consistency:

#### Function Definitions
```r
#' Function Title
#'
#' Detailed description of what the function does.
#'
#' @param param_name Description of parameter
#' @param another_param Description with type info
#' @return Description of return value
#' @export
#' @examples
#' \dontrun{
#' example_function(data, param = "value")
#' }
function_name <- function(param_name, 
                         another_param = default_value,
                         optional_param = NULL) {
  
  # Input validation
  if (missing(param_name)) {
    stop("param_name is required")
  }
  
  # Function logic here
  result <- some_operation(param_name)
  
  return(result)
}
```

#### Variable Naming
- Use **snake_case** for variables and functions: `shape_plot()`, `hull_alpha`
- Use **PascalCase** for exported function names when appropriate: `Haug_panel()`
- Use descriptive names: `contour_linewidth` instead of `lw`

#### Code Organization
- Keep functions focused and modular
- Use meaningful variable names
- Include comments for complex logic
- Maintain consistent indentation (2 spaces)

### Shiny App Style

#### UI Structure
```r
# Clear section headers
h3("Section Name"),

# Consistent input organization
selectInput("input_id", "Label:", 
           choices = choices_list, 
           selected = default_value),

# Use consistent spacing
tags$hr(),  # Section separators
```

#### Server Logic
```r
# Use reactive values appropriately
reactive_data <- reactive({
  req(input$required_input)
  # Processing logic
})

# Clear output rendering
output$plot_output <- renderPlot({
  # Plot generation logic
})
```

## Documentation Standards

### Function Documentation (roxygen2)

All exported functions must include complete roxygen2 documentation:

```r
#' Create Advanced Shape Plots
#'
#' Generate customizable scatterplots with convex hulls, contours, and 
#' extensive styling options for morphometric visualization.
#'
#' @param data A data frame containing the plotting data
#' @param x_col Character string specifying the x-axis column name
#' @param y_col Character string specifying the y-axis column name
#' @param group_col Optional character string for grouping column
#' @param show_hulls Logical indicating whether to show convex hulls
#' @param hull_alpha Numeric value (0-1) for hull transparency
#' @param ... Additional parameters passed to ggplot2
#'
#' @return A ggplot2 object
#'
#' @details
#' This function creates publication-ready plots with extensive customization
#' options. It supports multiple plot styles including "Haug", "inverted_Haug",
#' and "publication" themes.
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' data <- data.frame(PC1 = rnorm(100), PC2 = rnorm(100), 
#'                    group = sample(c("A", "B"), 100, replace = TRUE))
#' shape_plot(data, x_col = "PC1", y_col = "PC2", group_col = "group")
#' }
#'
#' @seealso \code{\link{Haug_panel}}, \code{\link{Haug_overview}}
#' @export
#' @import ggplot2
#' @importFrom dplyr filter mutate
```

### Key Documentation Elements

1. **Title**: Clear, descriptive function name
2. **Description**: What the function does
3. **Parameters**: All parameters with types and descriptions
4. **Return**: What the function returns
5. **Details**: Additional implementation details
6. **Examples**: Working code examples (use `\dontrun{}` for complex examples)
7. **See Also**: Related functions
8. **Import statements**: Required packages

## Testing

### Writing Tests

Create tests in the `tests/testthat/` directory:

```r
# tests/testthat/test-shape_plot.R
test_that("shape_plot creates valid ggplot object", {
  # Test data
  test_data <- data.frame(
    PC1 = rnorm(50),
    PC2 = rnorm(50),
    group = sample(c("A", "B"), 50, replace = TRUE)
  )
  
  # Create plot
  p <- shape_plot(test_data, x_col = "PC1", y_col = "PC2", 
                  group_col = "group", show_hulls = TRUE)
  
  # Tests
  expect_s3_class(p, "ggplot")
  expect_true("PC1" %in% names(p$data))
  expect_true("PC2" %in% names(p$data))
})

test_that("shape_plot handles missing columns gracefully", {
  test_data <- data.frame(x = 1:10, y = 1:10)
  
  expect_error(
    shape_plot(test_data, x_col = "missing_col", y_col = "y"),
    "Column 'missing_col' not found"
  )
})
```

### Running Tests

```r
# Run all tests
devtools::test()

# Run specific test file
testthat::test_file("tests/testthat/test-shape_plot.R")

# Check test coverage
covr::package_coverage()
```

## Submitting Contributions

### Pull Request Process

1. **Ensure all tests pass**:
   ```r
   devtools::check()
   ```

2. **Update documentation**:
   ```r
   devtools::document()
   ```

3. **Update NEWS.md** with your changes:
   ```markdown
   # HaugShape 0.1.2
   
   ## New Features
   - Added new plotting function for enhanced visualization
   
   ## Bug Fixes
   - Fixed issue with missing group handling in shape_plot()
   ```

4. **Create Pull Request** with:
   - Clear description of changes
   - Reference to related issues
   - Screenshots for UI changes
   - Test results

### Pull Request Template

Use this template for your pull requests:

```markdown
## Description
Brief summary of changes made.

## Type of Change
- [ ] Bug fix
- [ ] New feature
- [ ] Documentation update
- [ ] Performance improvement
- [ ] Code refactoring

## Testing
- [ ] All existing tests pass
- [ ] New tests added for new functionality
- [ ] Manual testing completed

## Checklist
- [ ] Code follows style guidelines
- [ ] Documentation updated
- [ ] NEWS.md updated
- [ ] No merge conflicts

## Screenshots (if applicable)
Include screenshots for UI changes.
```

## Shiny App Development

### App Structure

The Shiny app (`inst/shiny/app.R`) follows this structure:

```r
# Libraries and dependencies
library(shiny)
library(HaugShape)
# ... other libraries

# UI definition
ui <- fluidPage(
  titlePanel("HaugShape Shiny App"),
  sidebarLayout(
    sidebarPanel(...),
    mainPanel(
      tabsetPanel(
        tabPanel("Data Preview", ...),
        tabPanel("Output Plot", ...),
        tabPanel("Image Processing", ...),
        # Add new tabs here
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  # Reactive values and observers
}

# App launch
shinyApp(ui = ui, server = server)
```

### Adding New Features to the Shiny App

1. **UI Components**: Add new inputs to the sidebar or new tabs
2. **Server Logic**: Implement reactive functions and outputs
3. **File Handling**: Use `shinyFiles` for directory/file selection
4. **Plot Integration**: Use `renderPlot()` for HaugShape functions

### Shiny App Testing

Test the app functionality:

```r
# Run the app locally
HaugShape::run_app()

# Test specific functionality
# - Upload test data files
# - Try different plot parameters
# - Test file operations
# - Verify download functionality
```

## Package Structure

### Directory Layout

```
HaugShape/
├── DESCRIPTION          # Package metadata
├── NAMESPACE           # Package namespace
├── LICENSE.md          # MIT License
├── README.md           # Package overview
├── CONTRIBUTING.md     # This file
├── NEWS.md            # Version changelog
├── R/                 # R source code
│   ├── shape_plot.R   # Main plotting functions
│   ├── Haug_*.R       # Haug-style functions
│   ├── shape_analysis.R # Morphometric analysis
│   └── ...
├── man/               # Documentation (auto-generated)
├── inst/              # Installed files
│   └── shiny/
│       └── app.R      # Shiny application
├── tests/             # Unit tests
│   └── testthat/
└── vignettes/         # Package vignettes (future)
```

### Key Files

- **DESCRIPTION**: Package dependencies and metadata
- **NAMESPACE**: Export/import definitions (auto-generated)
- **R/*.R**: All R functions with roxygen2 documentation
- **inst/shiny/app.R**: Interactive Shiny application
- **man/*.Rd**: Function help files (auto-generated)

## Getting Help

### Resources

- **Package Documentation**: `?HaugShape::function_name`
- **GitHub Issues**: Report bugs or request features
- **R Package Development**: [R Packages book](https://r-pkgs.org/)
- **Shiny Development**: [Mastering Shiny](https://mastering-shiny.org/)

### Contact

- **Maintainer**: Colin Hassenbach (colin.hassenbach@gmail.com)
- **GitHub**: [TestoKlaus/HaugShape](https://github.com/TestoKlaus/HaugShape)

### Common Issues

1. **Package Check Errors**: Run `devtools::check()` and address warnings
2. **Documentation Issues**: Update roxygen2 comments and run `devtools::document()`
3. **Dependency Problems**: Check DESCRIPTION file imports
4. **Shiny App Issues**: Test locally with different data files

### Development Tips

1. **Regular Testing**: Run `devtools::check()` frequently
2. **Documentation First**: Write roxygen2 docs as you code
3. **Small Commits**: Make focused, well-documented commits
4. **User Perspective**: Consider how researchers will use functions
5. **Performance**: Profile code for large datasets

## Code of Conduct

Please note that this project adheres to a code of conduct based on respect, collaboration, and inclusive participation. By contributing, you agree to maintain a welcoming environment for all contributors.

---

Thank you for contributing to HaugShape! Your efforts help make morphometric analysis more accessible to researchers worldwide.