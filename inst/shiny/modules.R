# Shiny modules for HaugShape application
# This file contains reusable UI and server components

#' Data Upload Module UI
#'
#' @param id Module namespace id
#' @return Shiny UI elements
data_upload_ui <- function(id) {
  ns <- NS(id)

  tagList(
    fileInput(
      ns("datafile"),
      "Upload Your Data (Excel/CSV)",
      accept = c(".xlsx", ".xls", ".csv"),
      multiple = FALSE
    ),

    conditionalPanel(
      condition = paste0("output['", ns("data_uploaded"), "']"),
      div(
        style = paste0("margin-top: 10px; padding: 10px; ",
                      "background-color: #d4edda; border: 1px solid #c3e6cb; ",
                      "border-radius: 4px;"),
        icon("check-circle", style = "color: #155724; margin-right: 5px;"),
        "Data uploaded successfully!",
        br(),
        textOutput(ns("data_info"))
      )
    ),

    conditionalPanel(
      condition = paste0("!output['", ns("data_uploaded"), "']"),
      div(
        style = paste0("margin-top: 10px; padding: 10px; ",
                      "background-color: #f8d7da; border: 1px solid #f5c6cb; ",
                      "border-radius: 4px;"),
        icon("info-circle", style = "color: #721c24; margin-right: 5px;"),
        paste0("Please upload data to continue. Supported formats: ",
               "Excel (.xlsx, .xls) and CSV (.csv)")
      )
    )
  )
}

#' Data Upload Module Server
#'
#' @param id Module namespace id
#' @return Reactive data frame
data_upload_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Reactive value to store uploaded data
    data <- reactive({
      req(input$datafile)

      ext <- tools::file_ext(input$datafile$datapath)

      tryCatch({
        if (ext %in% c("xlsx", "xls")) {
          if (!requireNamespace("readxl", quietly = TRUE)) {
            showNotification("Package 'readxl' required for Excel files",
                           type = "error")
            return(NULL)
          }
          readxl::read_excel(input$datafile$datapath)
        } else if (ext == "csv") {
          read.csv(input$datafile$datapath, stringsAsFactors = FALSE)
        } else {
          showNotification("Unsupported file format", type = "error")
          return(NULL)
        }
      }, error = function(e) {
        showNotification(paste("Error reading file:", e$message),
                        type = "error")
        return(NULL)
      })
    })

    # Check if data is uploaded
    output$data_uploaded <- reactive({
      !is.null(data()) && nrow(data()) > 0
    })
    outputOptions(output, "data_uploaded", suspendWhenHidden = FALSE)

    # Data information
    output$data_info <- renderText({
      if (!is.null(data())) {
        paste("Rows:", nrow(data()), "| Columns:", ncol(data()))
      }
    })

    return(data)
  })
}

#' Function Selection Module UI
#'
#' @param id Module namespace id
#' @return Shiny UI elements
function_selection_ui <- function(id) {
  ns <- NS(id)

  tagList(
    selectInput(
      ns("function_select"),
      "Choose Analysis Function",
      choices = list(
        "Plotting" = list(
          "Shape Plot" = "shape_plot",
          "Cluster Plot" = "cluster_plot",
          "Elbow Plot" = "elbow_plot"
        ),
        "Analysis" = list(
          "Overview Plot" = "overview_plot",
          "Shape Analysis" = "shape_analysis"
        )
      ),
      selected = "shape_plot"
    ),

    div(id = ns("function_info"),
        style = "margin-top: 10px; padding: 10px; background-color: #e2f3ff; border-radius: 4px;",
        uiOutput(ns("function_description"))
    )
  )
}

#' Function Selection Module Server
#'
#' @param id Module namespace id
#' @return Reactive function selection
function_selection_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Function descriptions
    function_descriptions <- list(
      shape_plot = paste("Create customizable scatter plots with convex hulls,",
                         "contours, and shape overlays."),
      cluster_plot = paste("Perform clustering analysis and visualize results",
                           "with different clustering methods."),
      elbow_plot = paste("Generate elbow plots to determine optimal number of",
                         "clusters for your data."),
      overview_plot = paste("Create comprehensive multi-panel overviews with",
                           "hulls, contours, and boxplots."),
      shape_analysis = paste("Perform morphometric analysis using Fourier",
                            "descriptors and PCA.")
    )

    # Display function description
    output$function_description <- renderUI({
      desc <- function_descriptions[[input$function_select]]
      if (is.null(desc)) {
        desc <- "Select a function to see its description."
      }

      tagList(
        icon("info-circle", style = "color: #0066cc; margin-right: 5px;"),
        desc
      )
    })

    return(reactive(input$function_select))
  })
}

#' Column Selection Module UI
#'
#' @param id Module namespace id
#' @param show_group Logical, whether to show group column selector
#' @return Shiny UI elements
column_selection_ui <- function(id, show_group = TRUE) {
  ns <- NS(id)

  ui_elements <- list(
    selectInput(ns("x_col"), "X-axis Column", choices = NULL),
    selectInput(ns("y_col"), "Y-axis Column", choices = NULL)
  )

  if (show_group) {
    ui_elements <- append(ui_elements, list(
      selectInput(ns("group_col"), "Group Column (Optional)",
                  choices = NULL, selected = "")
    ), after = 2)
  }

  return(tagList(ui_elements))
}

#' Column Selection Module Server
#'
#' @param id Module namespace id
#' @param data Reactive data frame
#' @param show_group Logical, whether to show group column selector
#' @return List of reactive column selections
column_selection_server <- function(id, data, show_group = TRUE) {
  moduleServer(id, function(input, output, session) {

    # Update column choices when data changes
    observe({
      req(data())

      numeric_cols <- names(data())[sapply(data(), is.numeric)]
      all_cols <- names(data())

      # Update x and y column choices (numeric only)
      updateSelectInput(session, "x_col",
                        choices = numeric_cols,
                        selected = if (length(numeric_cols) > 0) {
                          numeric_cols[1]
                        } else {
                          NULL
                        })

      updateSelectInput(session, "y_col",
                        choices = numeric_cols,
                        selected = if (length(numeric_cols) > 1) {
                          numeric_cols[2]
                        } else {
                          NULL
                        })

      # Update group column choices (all columns)
      if (show_group) {
        updateSelectInput(session, "group_col",
                          choices = c("None" = "", all_cols),
                          selected = "")
      }
    })

    # Return reactive values
    result <- list(
      x_col = reactive(input$x_col),
      y_col = reactive(input$y_col)
    )

    if (show_group) {
      result$group_col <- reactive({
        if (input$group_col == "") {
          NULL
        } else {
          input$group_col
        }
      })
    }

    return(result)
  })
}

#' Error Display Module UI
#'
#' @param id Module namespace id
#' @return Shiny UI elements
error_display_ui <- function(id) {
  ns <- NS(id)

  conditionalPanel(
    condition = paste0("output['", ns("has_error"), "']"),
    div(
      class = "alert alert-danger",
      style = "margin-top: 10px;",
      icon("exclamation-triangle"),
      " ",
      textOutput(ns("error_message"), inline = TRUE)
    )
  )
}

#' Error Display Module Server
#'
#' @param id Module namespace id
#' @param error_reactive Reactive expression containing error message or NULL
#' @return Nothing (side effects only)
error_display_server <- function(id, error_reactive) {
  moduleServer(id, function(input, output, session) {

    output$has_error <- reactive({
      !is.null(error_reactive()) && nchar(error_reactive()) > 0
    })
    outputOptions(output, "has_error", suspendWhenHidden = FALSE)

    output$error_message <- renderText({
      error_reactive()
    })
  })
}

#' Success Display Module UI
#'
#' @param id Module namespace id
#' @return Shiny UI elements
success_display_ui <- function(id) {
  ns <- NS(id)

  conditionalPanel(
    condition = paste0("output['", ns("has_success"), "']"),
    div(
      class = "alert alert-success",
      style = "margin-top: 10px;",
      icon("check-circle"),
      " ",
      textOutput(ns("success_message"), inline = TRUE)
    )
  )
}

#' Success Display Module Server
#'
#' @param id Module namespace id
#' @param success_reactive Reactive expression containing success message or NULL
#' @return Nothing (side effects only)
success_display_server <- function(id, success_reactive) {
  moduleServer(id, function(input, output, session) {

    output$has_success <- reactive({
      !is.null(success_reactive()) && nchar(success_reactive()) > 0
    })
    outputOptions(output, "has_success", suspendWhenHidden = FALSE)

    output$success_message <- renderText({
      success_reactive()
    })
  })
}
