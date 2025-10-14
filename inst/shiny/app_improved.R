# Improved HaugShape Shiny Application
# This version uses modular components and improved error handling

# Source the modules file
source("modules.R")

# Load required libraries with error handling
required_packages <- c("shiny", "HaugShape", "dplyr", "ggplot2")
optional_packages <- c("colourpicker", "shinyFiles", "Momocs", "DT", "readxl", "fs", "magick")

# Check required packages
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("Required package '", pkg, "' is not installed. Please install it with: install.packages('", pkg, "')")
  }
}

# Load required libraries
library(shiny)
library(HaugShape)
library(dplyr)
library(ggplot2)

# Load optional libraries with warnings
load_optional <- function(pkg) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    library(pkg, character.only = TRUE)
    TRUE
  } else {
    message("Optional package '", pkg, 
            "' not available. Some features may be disabled.")
    FALSE
  }
}

# Track which optional packages are available
optional_available <- sapply(optional_packages, load_optional)

# Source modular components
source("modules.R", local = TRUE)

# Define UI
ui <- fluidPage(
  # Application title and styling
  titlePanel("HaugShape: Advanced Shape Analysis & Morphometric Visualization"),

  # Add custom CSS
  tags$head(
    tags$style(HTML("
      .content-header {
        background-color: #f8f9fa;
        padding: 15px;
        margin-bottom: 20px;
        border-radius: 5px;
        border-left: 4px solid #007bff;
      }
      .sidebar-section {
        margin-bottom: 25px;
        padding: 15px;
        background-color: #ffffff;
        border: 1px solid #dee2e6;
        border-radius: 5px;
      }
      .alert {
        padding: 10px 15px;
        margin: 10px 0;
        border-radius: 4px;
      }
      .alert-danger {
        background-color: #f8d7da;
        border: 1px solid #f5c6cb;
        color: #721c24;
      }
      .alert-success {
        background-color: #d4edda;
        border: 1px solid #c3e6cb;
        color: #155724;
      }
      .nav-tabs .nav-link.active {
        background-color: #007bff;
        color: white;
      }
    "))
  ),

  # Check for required functionality
  conditionalPanel(
    condition = "false", # This will be updated by server
    id = "package_check",
    div(
      class = "alert alert-warning",
      icon("exclamation-triangle"),
      paste(" Some optional packages are missing. Certain features may",
            "not be available. See the Contributors Guide tab for",
            "installation instructions.")
    )
  ),

  sidebarLayout(
    sidebarPanel(
      width = 3,

      # Data Upload Section
      div(
        class = "sidebar-section",
        h4("Data Upload", style = "margin-top: 0;"),
        data_upload_ui("data_upload")
      ),

      # Function Selection Section
      div(
        class = "sidebar-section",
        h4("Analysis Function"),
        function_selection_ui("function_select")
      ),

      # Column Selection Section
      conditionalPanel(
        condition = "output.data_available",
        div(
          class = "sidebar-section",
          h4("Column Selection"),
          column_selection_ui("columns", show_group = TRUE)
        )
      ),

      # Download Section
      conditionalPanel(
        condition = "output.plot_available",
        div(
          class = "sidebar-section",
          h4("Export"),
          downloadButton("download_plot", "Download Plot",
                         class = "btn-primary", style = "width: 100%;"),
          br(), br(),
          selectInput("download_format", "Format:",
                      choices = c("PNG" = "png", "PDF" = "pdf", 
                                  "TIFF" = "tiff"),
                      selected = "png"),
          numericInput("download_width", "Width (pixels):", 
                       value = 1200, min = 300, max = 4800),
          numericInput("download_height", "Height (pixels):", 
                       value = 800, min = 300, max = 4800),
          numericInput("download_dpi", "DPI:", 
                       value = 300, min = 72, max = 600)
        )
      ),

      # Optional: Shapes Configuration (if shinyFiles available)
      conditionalPanel(
        condition = paste0(optional_available["shinyFiles"]),
        div(
          class = "sidebar-section",
          h4("Shape Files (Optional)"),
          p("Configure shape files for enhanced visualization", style = "font-size: 0.9em; color: #666;"),
          # Shape file configuration would go here
          disabled(actionButton("shape_config", "Configure Shapes", class = "btn-secondary"))
        )
      )
    ),

    mainPanel(
      width = 9,

      # Error and success displays
      error_display_ui("main_error"),
      success_display_ui("main_success"),

      tabsetPanel(
        id = "main_tabs",

        # Data Preview Tab
        tabPanel(
          "Data Preview",
          div(
            class = "content-header",
            h4("Data Overview", style = "margin: 0;"),
            p("Preview your uploaded data and verify column types", 
              style = "margin: 5px 0 0 0; color: #666;")
          ),

          conditionalPanel(
            condition = "output.data_available",
            if (optional_available["DT"]) {
              DT::dataTableOutput("data_preview")
            } else {
              div(
                tableOutput("data_preview_basic"),
                p("Install the 'DT' package for enhanced data table features",
                  style = "color: #666; font-style: italic;")
              )
            }
          ),

          conditionalPanel(
            condition = "!output.data_available",
            div(
              style = "text-align: center; padding: 50px;",
              icon("upload", style = "font-size: 3em; color: #ccc;"),
              h4("No Data Uploaded", style = "color: #666;"),
              p("Please upload a data file to see the preview", 
                style = "color: #999;")
            )
          )
        ),

        # Analysis Results Tab
        tabPanel(
          "Analysis Results",
          div(
            class = "content-header",
            h4("Analysis Output", style = "margin: 0;"),
            p("Generated plots and analysis results", style = "margin: 5px 0 0 0; color: #666;")
          ),

          conditionalPanel(
            condition = "output.plot_available",
            plotOutput("main_plot", height = "600px")
          ),

          conditionalPanel(
            condition = "!output.plot_available && output.data_available",
            div(
              style = "text-align: center; padding: 50px;",
              icon("chart-line", style = "font-size: 3em; color: #ccc;"),
              h4("Ready for Analysis", style = "color: #666;"),
              p("Configure your analysis parameters and column selections", style = "color: #999;")
            )
          ),

          conditionalPanel(
            condition = "!output.data_available",
            div(
              style = "text-align: center; padding: 50px;",
              icon("database", style = "font-size: 3em; color: #ccc;"),
              h4("Upload Data First", style = "color: #666;"),
              p("Please upload data to begin analysis", style = "color: #999;")
            )
          )
        ),

        # Image Processing Tab (if magick available)
        conditionalPanel(
          condition = paste0(optional_available["magick"]),
          tabPanel(
            "Image Processing",
            div(
              class = "content-header",
              h4("Image Processing Tools", style = "margin: 0;"),
              p("Convert, split, and process images for morphometric analysis", style = "margin: 5px 0 0 0; color: #666;")
            ),
            # Image processing interface would go here
            p("Image processing tools are available but not yet implemented in this modular version.")
          )
        ),

        # Shape Analysis Tab (if Momocs available)
        conditionalPanel(
          condition = paste0(optional_available["Momocs"]),
          tabPanel(
            "Shape Analysis",
            div(
              class = "content-header",
              h4("Morphometric Shape Analysis", style = "margin: 0;"),
              p("Fourier analysis and morphometric computations", style = "margin: 5px 0 0 0; color: #666;")
            ),
            # Shape analysis interface would go here
            p("Shape analysis tools are available but not yet implemented in this modular version.")
          )
        ),

        # Contributors Guide Tab
        tabPanel(
          "Contributors Guide",
          div(
            style = "padding: 20px; max-width: 1000px;",
            h3("Contributing to HaugShape", style = "color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 10px;"),

            p(strong("Welcome to the HaugShape contributors guide!"),
              "This guide will help you get started with developing and contributing to this R package for advanced shape analyses and morphometric visualizations."),

            # Include the contributors guide content here (truncated for brevity)
            # ... (include the full contributors guide content as before)

            div(
              style = "text-align: center; margin-top: 20px;",
              tags$a(
                "📖 View Full Contributing Guide",
                href = "https://github.com/TestoKlaus/HaugShape/blob/stable/CONTRIBUTING.md",
                target = "_blank",
                class = "btn btn-primary",
                style = "background-color: #007bff; border-color: #007bff; color: white; text-decoration: none; padding: 10px 20px; border-radius: 5px;"
              )
            )
          )
        )
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {

  # Initialize reactive values
  values <- reactiveValues(
    error_message = NULL,
    success_message = NULL,
    current_plot = NULL
  )

  # Module servers
  uploaded_data <- data_upload_server("data_upload")
  selected_function <- function_selection_server("function_select")
  column_selections <- column_selection_server("columns", uploaded_data, show_group = TRUE)

  # Error and success display servers
  error_display_server("main_error", reactive(values$error_message))
  success_display_server("main_success", reactive(values$success_message))

  # Data availability reactive
  output$data_available <- reactive({
    !is.null(uploaded_data()) && nrow(uploaded_data()) > 0
  })
  outputOptions(output, "data_available", suspendWhenHidden = FALSE)

  # Plot availability reactive
  output$plot_available <- reactive({
    !is.null(values$current_plot)
  })
  outputOptions(output, "plot_available", suspendWhenHidden = FALSE)

  # Data preview
  output$data_preview <- if (optional_available[["DT"]]) {
    DT::renderDataTable({
      req(uploaded_data())
      DT::datatable(
        uploaded_data(),
        options = list(
          scrollX = TRUE,
          pageLength = 10,
          dom = 'frtip'
        ),
        class = "cell-border stripe hover"
      )
    })
  } else {
    NULL
  }

  # Basic data preview (fallback)
  output$data_preview_basic <- renderTable({
    req(uploaded_data())
    head(uploaded_data(), 10)
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  # Main plot generation
  observe({
      # Clear previous messages
      values$error_message <- NULL
      values$success_message <- NULL

      # Check if we have required inputs
      if (is.null(uploaded_data()) ||
          is.null(column_selections$x_col()) ||
          is.null(column_selections$y_col()) ||
          column_selections$x_col() == "" ||
          column_selections$y_col() == "") {
        values$current_plot <- NULL
        return()
      }

    # Generate plot based on selected function
    tryCatch({
      data <- uploaded_data()
      x_col <- column_selections$x_col()
      y_col <- column_selections$y_col()
      group_col <- column_selections$group_col()

      # Validate columns exist and are appropriate
      if (!x_col %in% colnames(data)) {
        values$error_message <- paste("X column '", x_col, "' not found in data")
        return()
      }
      if (!y_col %in% colnames(data)) {
        values$error_message <- paste("Y column '", y_col, "' not found in data")
        return()
      }
      if (!is.null(group_col) && !group_col %in% colnames(data)) {
        values$error_message <- paste("Group column '", group_col, "' not found in data")
        return()
      }

      # Generate plot based on function selection
      plot_result <- switch(
        selected_function(),

        "shape_plot" = {
          HaugShape::shape_plot(
            data = data,
            x_col = x_col,
            y_col = y_col,
            group_col = group_col,
            show_hulls = !is.null(group_col),
            plot_style = "Haug"
          )
        },

        "cluster_plot" = {
          HaugShape::cluster_plot(
            data = data,
            x_col = x_col,
            y_col = y_col,
            method = "kmeans",
            k = 3,
            show_shapes = FALSE
          )
        },

        "elbow_plot" = {
          HaugShape::elbow_plot(
            data = data,
            x_col = x_col,
            y_col = y_col,
            max_k = 10
          )
        },

        # Default case
        {
          values$error_message <- "Selected function not yet implemented"
          return()
        }
      )

      values$current_plot <- plot_result
      values$success_message <- "Plot generated successfully!"

    }, error = function(e) {
      values$error_message <- paste("Error generating plot:", e$message)
      values$current_plot <- NULL
    })
  })

  # Render main plot
  output$main_plot <- renderPlot({
    req(values$current_plot)
    values$current_plot
  })

  # Download handler
  output$download_plot <- downloadHandler(
    filename = function() {
      paste0("HaugShape_plot_", Sys.Date(), ".", input$download_format)
    },
    content = function(file) {
      req(values$current_plot)

      tryCatch({
        if (input$download_format == "png") {
          png(file, width = input$download_width, 
              height = input$download_height,
              res = input$download_dpi, units = "px")
        } else if (input$download_format == "pdf") {
          pdf(file, width = input$download_width / input$download_dpi,
              height = input$download_height / input$download_dpi)
        } else if (input$download_format == "tiff") {
          tiff(file, width = input$download_width, 
               height = input$download_height,
               res = input$download_dpi, units = "px", compression = "lzw")
        }

        print(values$current_plot)
        dev.off()

        values$success_message <- "Plot downloaded successfully!"

      }, error = function(e) {
        values$error_message <- paste("Error downloading plot:", e$message)
      })
    }
  )

  # Check for missing optional packages on startup
  observe({
    missing_packages <- names(optional_available)[!optional_available]
    if (length(missing_packages) > 0) {
      showNotification(
        paste("Optional packages not available:", 
              paste(missing_packages, collapse = ", "),
              ". Some features may be disabled."),
        duration = 10,
        type = "warning"
      )
    }
  })

  # Session info for debugging
  session$onSessionEnded(function() {
    message("HaugShape Shiny session ended at ", Sys.time())
  })
}

# Run the application
shinyApp(ui = ui, server = server)
