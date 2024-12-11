library(shiny)
library(HaugShape)
library(colourpicker)
library(dplyr)
library(shinyFiles)
library(Momocs)
library(ggplot2)

ui <- fluidPage(
  titlePanel("HaugShape Shiny App"),
  sidebarLayout(
    sidebarPanel(
      downloadButton("download_plot", "Download Plot"),

      fileInput("datafile", "Upload Your Data (Excel)"),
      selectInput(
        "function_select",
        "Choose Function to Use",
        choices = c("Shape Plot", "Cluster Plot", "Elbow Plot",  "Overview Plot"),
        selected = "Shape Plot"
      ),
      uiOutput("dynamic_ui"),
      h3("Shapes Configuration"),
      shinyDirButton("shape_dir", "Select Shape Folder", "Please select a folder"),
      textOutput("selected_dir"),
      selectInput("shape_id_col", "Select ID Column for Shapes", choices = NULL),
      actionButton("map_shapes", "Map Shapes to Data"),
      textOutput("shape_mapping_status")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data Preview", DT::dataTableOutput("data_preview")),
        tabPanel("Output Plot", plotOutput("output_plot")),
        tabPanel(
          "Overview Results",
          uiOutput("overview_results")),  # This dynamically shows all plots
        tabPanel(
          "Image Processing",
          h4("Convert PNG Files to JPG"),
          shinyDirButton("png_input_dir", "Select PNG Folder", "Choose a folder with PNG files"),
          textOutput("png_input_dir_text"),
          shinyDirButton("jpg_output_dir", "Select Output Folder", "Choose a folder for JPG files"),
          textOutput("jpg_output_dir_text"),
          numericInput("resize_width", "Resize Width (pixels, optional):", value = NULL, min = 1),
          numericInput("resize_height", "Resize Height (pixels, optional):", value = NULL, min = 1),
          numericInput("padding", "Padding (pixels):", value = 10, min = 0),
          numericInput("quality", "JPG Quality (0-100):", value = 100, min = 0, max = 100),
          actionButton("convert_images", "Convert Images"),
          textOutput("conversion_status"),
          tags$hr(),  # Adds a horizontal line to separate sections
          h4("Cut Images in Half (Retain Right Half)"),
          shinyDirButton("cut_input_dir", "Select Input Folder", "Choose a folder with images"),
          textOutput("cut_input_dir_text"),
          shinyDirButton("cut_output_dir", "Select Output Folder", "Choose a folder for processed images"),
          textOutput("cut_output_dir_text"),
          actionButton("cut_images", "Process Images"),
          textOutput("cut_status"),
          tags$hr(),  # Adds a horizontal line to separate sections
          h4("Complete Halved Shapes to Symmetrical Images"),
          shinyDirButton("complete_input_dir", "Select Input Folder", "Choose a folder with halved images"),
          textOutput("complete_input_dir_text"),
          shinyDirButton("complete_output_dir", "Select Output Folder", "Choose a folder for completed images"),
          textOutput("complete_output_dir_text"),
          actionButton("complete_shapes", "Complete Halved Shapes"),
          textOutput("complete_status"),
          tags$hr()  # Adds a horizontal line to separate sections

        ),
        tabPanel(
          "Shape Analysis",
          h4("Run Shape Analysis"),
          shinyDirButton("shape_analysis_dir", "Select Shape Folder", "Choose a folder with shape files"),
          textOutput("shape_analysis_dir_text"),
          shinyDirButton("output_dir", "Select Output Directory", "Choose a folder for Excel file"),
          textOutput("selected_output_dir"),
          checkboxInput("norm", "Normalize Fourier Descriptors (norm)", value = TRUE),
          numericInput("num_pcs", "Number of PCs to Display in Contribution Plot:", value = 10, min = 1),
          selectInput(
            "start_point",
            "Starting Point for Normalization:",
            choices = c("left", "up", "down", "right"),
            selected = "left"
          ),
          textInput("output_file", "Output File Name (Excel):", value = "shape_analysis.xlsx"),
          actionButton("run_analysis", "Run Analysis"),
          tags$hr(),
          h4("Summary Output"),
          verbatimTextOutput("pca_summary"),
          tags$hr(),
          h4("PC Contribution Plot"),
          plotOutput("pc_contrib_plot")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # Initialize shinyFiles
  volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
  shinyDirChoose(input, "shape_dir", roots = volumes, session = session)

  # Reactive directory path
  shape_directory <- reactiveVal(NULL)

  observeEvent(input$shape_dir, {
    dir_path <- parseDirPath(volumes, input$shape_dir)
    shape_directory(dir_path)
    output$selected_dir <- renderText({ paste("Selected Directory:", dir_path) })
  })

  # Reactive values for directories
  input_dir <- reactiveVal(NULL)
  output_dir <- reactiveVal(NULL)

  # Handle input directory selection
  shinyDirChoose(input, "png_input_dir", roots = volumes, session = session)
  observeEvent(input$png_input_dir, {
    dir_path <- parseDirPath(volumes, input$png_input_dir)
    input_dir(dir_path)
    output$png_input_dir_text <- renderText({ paste("Selected PNG Folder:", dir_path) })
  })

  # Handle output directory selection
  shinyDirChoose(input, "jpg_output_dir", roots = volumes, session = session)
  observeEvent(input$jpg_output_dir, {
    dir_path <- parseDirPath(volumes, input$jpg_output_dir)
    output_dir(dir_path)
    output$jpg_output_dir_text <- renderText({ paste("Selected Output Folder:", dir_path) })
  })

  # Reactive values for directories
  cut_input_dir <- reactiveVal(NULL)
  cut_output_dir <- reactiveVal(NULL)

  # Handle input directory selection
  shinyDirChoose(input, "cut_input_dir", roots = volumes, session = session)
  observeEvent(input$cut_input_dir, {
    dir_path <- parseDirPath(volumes, input$cut_input_dir)
    cut_input_dir(dir_path)
    output$cut_input_dir_text <- renderText({ paste("Selected Input Folder:", dir_path) })
  })

  # Handle output directory selection
  shinyDirChoose(input, "cut_output_dir", roots = volumes, session = session)
  observeEvent(input$cut_output_dir, {
    dir_path <- parseDirPath(volumes, input$cut_output_dir)
    cut_output_dir(dir_path)
    output$cut_output_dir_text <- renderText({ paste("Selected Output Folder:", dir_path) })
  })

  # Reactive values for directories
  complete_input_dir <- reactiveVal(NULL)
  complete_output_dir <- reactiveVal(NULL)

  # Handle input directory selection
  shinyDirChoose(input, "complete_input_dir", roots = volumes, session = session)
  observeEvent(input$complete_input_dir, {
    dir_path <- parseDirPath(volumes, input$complete_input_dir)
    complete_input_dir(dir_path)
    output$complete_input_dir_text <- renderText({ paste("Selected Input Folder:", dir_path) })
  })

  # Handle output directory selection
  shinyDirChoose(input, "complete_output_dir", roots = volumes, session = session)
  observeEvent(input$complete_output_dir, {
    dir_path <- parseDirPath(volumes, input$complete_output_dir)
    complete_output_dir(dir_path)
    output$complete_output_dir_text <- renderText({ paste("Selected Output Folder:", dir_path) })
  })


  observeEvent(input$convert_images, {
    req(input_dir(), output_dir())  # Ensure both directories are selected

    tryCatch({
      # Retrieve file paths of PNG files in the input directory
      png_files <- list.files(input_dir(), pattern = "\\.png$", full.names = TRUE, ignore.case = TRUE)

      # Validate that there are PNG files to process
      if (length(png_files) == 0) {
        output$conversion_status <- renderText("No PNG files found in the selected folder.")
        return(NULL)
      }

      # Create a fixed_dim list based on user inputs
      fixed_dim <- list(
        width = if (!is.null(input$resize_width) && !is.na(input$resize_width) && input$resize_width > 0) input$resize_width else NULL,
        height = if (!is.null(input$resize_height) && !is.na(input$resize_height) && input$resize_height > 0) input$resize_height else NULL
      )

      # Process each PNG file and save as JPG
      lapply(png_files, function(file) {
        convert_png_to_jpg(
          file_path = file,
          output_dir = output_dir(),
          fixed_dim = fixed_dim,
          padding = input$padding,
          quality = input$quality
        )
      })

      output$conversion_status <- renderText("All PNG images have been successfully converted to JPG!")
    }, error = function(e) {
      # Handle errors and display message
      output$conversion_status <- renderText(paste("Error during conversion:", e$message))
    })
  })

  observeEvent(input$cut_images, {
    req(cut_input_dir(), cut_output_dir())  # Ensure both directories are selected

    tryCatch({
      # Retrieve file paths of all images in the input directory
      image_files <- list.files(cut_input_dir(), pattern = "\\.(png|jpg|jpeg)$", full.names = TRUE, ignore.case = TRUE)

      # Validate that there are images to process
      if (length(image_files) == 0) {
        output$cut_status <- renderText("No images found in the selected folder.")
        return(NULL)
      }

      # Process each image and save the right half
      lapply(image_files, function(file) {
        cut_image_right_half(
          file_path = file,
          output_dir = cut_output_dir()
        )
      })

      output$cut_status <- renderText("All images have been successfully processed!")
    }, error = function(e) {
      # Handle errors and display message
      output$cut_status <- renderText(paste("Error during processing:", e$message))
    })
  })

  observeEvent(input$complete_shapes, {
    req(complete_input_dir(), complete_output_dir())  # Ensure both directories are selected

    tryCatch({
      # Retrieve file paths of all images in the input directory
      halved_images <- list.files(complete_input_dir(), pattern = "\\.(png|jpg|jpeg)$", full.names = TRUE, ignore.case = TRUE)

      # Validate that there are images to process
      if (length(halved_images) == 0) {
        output$complete_status <- renderText("No images found in the selected folder.")
        return(NULL)
      }

      # Process each halved image and save the symmetrical result
      lapply(halved_images, function(file) {
        complete_halved_shape(
          file_path = file,
          output_dir = complete_output_dir()
        )
      })

      output$complete_status <- renderText("All halved shapes have been successfully completed to symmetrical images!")
    }, error = function(e) {
      # Handle errors and display message
      output$complete_status <- renderText(paste("Error during processing:", e$message))
    })
  })

  # Reactive directory for shape analysis
  shape_analysis_dir <- reactiveVal(NULL)

  # Handle shape analysis directory selection
  shinyDirChoose(input, "shape_analysis_dir", roots = volumes, session = session)
  observeEvent(input$shape_analysis_dir, {
    dir_path <- parseDirPath(volumes, input$shape_analysis_dir)
    shape_analysis_dir(dir_path)
    output$shape_analysis_dir_text <- renderText({ paste("Selected Shape Folder:", dir_path) })
  })

  # Reactive value for output directory
  output_dir <- reactiveVal(NULL)

  # Handle output directory selection
  shinyDirChoose(input, "output_dir", roots = volumes, session = session)
  observeEvent(input$output_dir, {
    dir_path <- parseDirPath(volumes, input$output_dir)
    output_dir(dir_path)
    output$selected_output_dir <- renderText({ paste("Selected Output Directory:", dir_path) })
  })

  # Run Shape Analysis with output directory
  observeEvent(input$run_analysis, {
    req(shape_analysis_dir(), output_dir())  # Ensure directories are selected

    tryCatch({
      result <- shape_analysis(
        shape_dir = shape_analysis_dir(),
        norm = input$norm,
        output_dir = output_dir(),
        output_file = input$output_file,
        num_pcs = input$num_pcs,
        start_point = input$start_point
      )

      output$pca_summary <- renderText({ paste("PCA Summary:\n", result$summary) })
      output$pc_contrib_plot <- renderPlot({ result$pc_contrib_plot })

      showNotification("Shape analysis completed successfully!", type = "message")
    }, error = function(e) {
      showNotification(paste("Error during analysis:", e$message), type = "error")
    })
  })

  # Update column choices for Overview Plot
  observeEvent(dataset(), {
    req(dataset())
    cols <- colnames(dataset())
    updateSelectizeInput(session, "overview_cols", choices = cols, selected = NULL)
    updateSelectInput(session, "overview_group_col", choices = c("", cols))
  })

  # Update group values when a grouping column is selected
  observeEvent(input$overview_group_col, {
    req(dataset(), input$overview_group_col)
    groups <- unique(dataset()[[input$overview_group_col]])
    updateSelectInput(session, "overview_group_vals", choices = groups)
  })

  observeEvent(input$generate_overview, {
    req(input$overview_cols)

    # Validate column selection
    if (length(input$overview_cols) %% 2 != 0) {
      showNotification("Please select 2, 4, 6, ... columns for the overview.", type = "error")
      return(NULL)
    }

    # Call Haug_overview with new parameters
    result <- Haug_overview(
      data = dataset(),
      cols = input$overview_cols,
      group_col = ifelse(input$overview_group_col == "", NULL, input$overview_group_col),
      group_vals = if (is.null(input$overview_group_vals) || length(input$overview_group_vals) == 0) NULL else input$overview_group_vals,
      show_all_hulls = input$overview_show_all_hulls,
      show_all_contours = input$overview_show_all_contours,
      show_table = input$overview_show_table,
      export_pdf = input$overview_export_pdf,
      pdf_file_name = input$overview_pdf_name
    )

    # Display all generated plots dynamically
    output$overview_results <- renderUI({
      plot_list <- result$hull_and_contours
      plot_outputs <- lapply(seq_along(plot_list), function(i) {
        plotname <- paste0("plot_", i)
        plotOutput(plotname, height = "400px", width = "100%")
      })

      boxplot_output <- plotOutput("boxplot_output", height = "400px", width = "100%")

      # Combine all plots into a single UI output
      do.call(tagList, c(plot_outputs, list(boxplot_output)))
    })

    # Render each hull plot individually
    for (i in seq_along(result$hull_and_contours)) {
      local({
        plot_index <- i
        output[[paste0("plot_", plot_index)]] <- renderPlot({
          result$hull_and_contours[[plot_index]]$hull_plot
        })
      })
    }

    # Render the boxplot
    output$boxplot_output <- renderPlot({
      result$boxplot
    })

    # Optionally display specimen tables if show_table is TRUE
    if (input$overview_show_table) {
      output$specimen_tables <- renderUI({
        tables <- result$tables
        if (is.null(tables) || length(tables) == 0) {
          return(h4("No specimen tables available."))
        }

        table_outputs <- lapply(names(tables), function(name) {
          tagList(
            h4(paste("Table:", name)),
            DT::dataTableOutput(outputId = paste0("table_", name))
          )
        })

        do.call(tagList, table_outputs)
      })

      # Render each table
      for (name in names(result$tables)) {
        local({
          table_name <- name
          output[[paste0("table_", table_name)]] <- DT::renderDataTable({
            result$tables[[table_name]]
          })
        })
      }
    }
  })


  # Reactive: Dataset
  dataset <- reactiveVal(NULL)

  observeEvent(input$datafile, {
    req(input$datafile)
    dataset(readxl::read_excel(input$datafile$datapath))
  })

  # Data Preview
  output$data_preview <- DT::renderDataTable({
    req(dataset())
    dataset()
  })

  observeEvent(input$function_select, {
    req(dataset())
    cols <- colnames(dataset())

    if (input$function_select == "Shape Plot") {
      updateSelectInput(session, "x_col", choices = cols, selected = NULL)
      updateSelectInput(session, "y_col", choices = cols, selected = NULL)
      updateSelectInput(session, "group_col", choices = c("", cols), selected = "")
      updateSelectInput(session, "group_vals", choices = NULL, selected = NULL)
    } else if (input$function_select == "Overview Plot") {
      updateSelectizeInput(session, "overview_cols", choices = cols, selected = NULL)
      updateSelectInput(session, "overview_group_col", choices = c("", cols), selected = NULL)
      updateSelectInput(session, "overview_group_vals", choices = NULL, selected = NULL)
    } else if (input$function_select == "Cluster Plot") {
      updateSelectInput(session, "x_col", choices = cols, selected = cols[1])
      updateSelectInput(session, "y_col", choices = cols, selected = cols[2])
      updateSelectInput(session, "clustering_method",
                        choices = c("kmeans", "hierarchical", "distance_threshold"),
                        selected = "kmeans")
      updateNumericInput(session, "distance_threshold", value = NULL)  # Reset distance_threshold
      updateSliderInput(session, "num_clusters", value = 3)            # Reset num_clusters
    } else if (input$function_select == "Elbow Plot") {
      updateSelectInput(session, "x_col", choices = cols, selected = cols[1])
      updateSelectInput(session, "y_col", choices = cols, selected = cols[2])
      updateNumericInput(session, "max_k", value = 10)                 # Reset max_k
    }
  })

  # Update column dropdowns when dataset changes
  observeEvent(dataset(), {
    req(dataset())
    cols <- colnames(dataset())
    updateSelectInput(session, "x_col", choices = cols, selected = cols[1])
    updateSelectInput(session, "y_col", choices = cols, selected = cols[2])
    updateSelectInput(session, "group_col", choices = c("", cols), selected = "")
    updateSelectInput(session, "shape_id_col", choices = cols, selected = cols[1])  # Shape ID column
  })

  output$download_plot <- downloadHandler(
    filename = function() {
      paste0(input$function_select, "_output", ".tiff")  # File name based on the selected plot type
    },
    content = function(file) {
      tryCatch({
        req(input$function_select, dataset(), input$x_col, input$y_col)

        # Generate the appropriate plot based on the selected function
        plot_to_save <- switch(input$function_select,
                               "Shape Plot" = shape_plot(
                                 data = dataset(),
                                 x_col = input$x_col,
                                 y_col = input$y_col,
                                 group_col = input$group_col,
                                 group_vals = if ("All" %in% input$group_vals) {
                                   unique(dataset()[[input$group_col]])
                                 } else {
                                   input$group_vals
                                 },
                                 point_color = sapply(seq_along(input$group_vals), function(i) input[[paste0("point_color_", i)]]),
                                 point_fill = sapply(seq_along(input$group_vals), function(i) input[[paste0("point_fill_", i)]]),
                                 point_shape = as.numeric(sapply(seq_along(input$group_vals), function(i) input[[paste0("point_shape_", i)]])),
                                 point_size = sapply(seq_along(input$group_vals), function(i) input[[paste0("point_size_", i)]]),
                                 show_hulls = input$show_hulls,
                                 hull_fill = sapply(seq_along(input$group_vals), function(i) input[[paste0("hull_fill_", i)]]),
                                 hull_color = sapply(seq_along(input$group_vals), function(i) input[[paste0("hull_color_", i)]]),
                                 hull_alpha = input$hull_alpha,
                                 show_shapes = input$show_shapes,
                                 show_shapes_for_groups = if ("All" %in% input$show_shapes_for_groups) {
                                   unique(dataset()[[input$group_col]])
                                 } else {
                                   input$show_shapes_for_groups
                                 },
                                 shape_size = input$shape_size,
                                 shape_shift = input$shape_shift,
                                 shape_x_adjust = input$shape_x_adjust,
                                 shape_y_adjust = input$shape_y_adjust,
                                 title = input$plot_title,
                                 x_label = ifelse(input$x_axis_label == "", input$x_col, input$x_axis_label),
                                 y_label = ifelse(input$y_axis_label == "", input$y_col, input$y_axis_label),
                                 tick_size = input$tick_size,
                                 axis_linewidth = input$axis_linewidth,
                                 plot_style = input$plot_style
                               ),
                               "Cluster Plot" = cluster_plot(
                                 data = dataset(),
                                 x_col = input$x_col,
                                 y_col = input$y_col,
                                 method = input$clustering_method,
                                 k = input$num_clusters,
                                 distance_threshold = input$distance_threshold,
                                 show_shapes = input$show_shapes,
                                 shape_size = input$shape_size,
                                 shape_x_adjust = input$shape_x_adjust,
                                 shape_y_adjust = input$shape_y_adjust,
                                 title = input$plot_title,
                                 x_label = ifelse(input$x_axis_label == "", input$x_col, input$x_axis_label),
                                 y_label = ifelse(input$y_axis_label == "", input$y_col, input$y_axis_label)
                               ),
                               "Elbow Plot" = elbow_plot(
                                 data = dataset(),
                                 x_col = input$x_col,
                                 y_col = input$y_col,
                                 max_k = input$max_k
                               )
        )

        # Save the plot as a TIFF file
        ggsave(
          filename = file,
          plot = plot_to_save,
          device = "tiff",
          dpi = 300,
          width = 10,
          height = 7
        )

        showNotification("Plot successfully saved!", type = "message")
      }, error = function(e) {
        showNotification(paste("Error saving plot:", e$message), type = "error")
      })
    }
  )


  observeEvent(input$group_col, {
    req(dataset(), input$group_col)
    groups <- unique(dataset()[[input$group_col]])

    # Update shape groups dropdown
    updateSelectInput(session, "show_shapes_for_groups", choices = c("All", groups), selected = groups)
  })


  # Observe the Map Shapes button click
  observeEvent(input$map_shapes, {
    req(dataset(), input$shape_id_col, shape_directory())

    # Get the directory path
    dir_path <- normalizePath(shape_directory(), winslash = "/", mustWork = FALSE)

    # Call the function and update the dataset
    updated_data <- map_shapes_to_data(dataset(), id_col = input$shape_id_col, shape_folder = dir_path)

    # Update the reactive dataset
    dataset(updated_data)
  })

  observeEvent(input$map_shapes, {
    req(dataset(), input$shape_id_col, shape_directory())

    # Validate the folder path
    dir_path <- normalizePath(shape_directory(), winslash = "/", mustWork = FALSE)

    tryCatch({
      shiny::withProgress({
        setProgress(0.1, message = "Processing shapes...")

        # Call the function
        updated_data <- map_shapes_to_data(dataset(), id_col = input$shape_id_col, shape_folder = dir_path)

        setProgress(0.8, message = "Updating dataset...")
        dataset(updated_data)  # Update reactive dataset

        setProgress(1, message = "Mapping complete!")
        output$shape_mapping_status <- renderText("Shapes successfully mapped!")
      })
    }, error = function(e) {
      output$shape_mapping_status <- renderText(paste("Error during shape mapping:", e$message))
    })
  })


  # Update group values dynamically when group_col changes
  observeEvent(input$group_col, {
    req(dataset(), input$group_col)
    groups <- unique(dataset()[[input$group_col]])

    # Automatically select all groups for group_vals and show_hull_for_groups
    updateSelectInput(session, "group_vals", choices = c("All", groups), selected = groups)
    updateSelectInput(session, "show_hull_for_groups", choices = groups, selected = groups)
  })

  # Update hull style values dynamically when show_hull_for_groups changes
  observeEvent(input$show_hull_for_groups, {
    req(input$group_col, input$show_hull_for_groups)

    # Get the selected groups
    selected_groups <- input$show_hull_for_groups

    # Regenerate the dynamic UI for hull styles
    output$dynamic_hull_fill <- renderUI({
      lapply(seq_along(selected_groups), function(i) {
        colourpicker::colourInput(
          inputId = paste0("hull_fill_", i),
          label = paste("Hull Fill for Group", selected_groups[i]),
          value = scales::hue_pal()(length(selected_groups))[i]  # Default color
        )
      })
    })

    output$dynamic_hull_color <- renderUI({
      lapply(seq_along(selected_groups), function(i) {
        colourpicker::colourInput(
          inputId = paste0("hull_color_", i),
          label = paste("Hull Border Color for Group", selected_groups[i]),
          value = "black"  # Default border color
        )
      })
    })
  })

  observeEvent(input$group_col, {
    req(dataset(), input$group_col)
    groups <- unique(dataset()[[input$group_col]])

    updateSelectInput(session, "show_contours_for_groups", choices = groups, selected = groups[1])
  })

  observeEvent(input$group_vals, {
    req(input$group_vals)

    # Get selected groups or all groups
    groups <- if ("All" %in% input$group_vals) {
      unique(dataset()[[input$group_col]])
    } else {
      input$group_vals
    }

    # Initialize default point styles for all groups
    lapply(seq_along(groups), function(i) {
      group <- groups[i]

      # Update point color
      if (is.null(input[[paste0("point_color_", i)]])) {
        updateColourInput(
          session, paste0("point_color_", i),
          value = scales::hue_pal()(length(groups))[i]  # Default color
        )
      }

      # Update point fill
      if (is.null(input[[paste0("point_fill_", i)]])) {
        updateColourInput(
          session, paste0("point_fill_", i),
          value = scales::hue_pal()(length(groups))[i]  # Default fill
        )
      }

      # Update point size
      if (is.null(input[[paste0("point_size_", i)]])) {
        updateNumericInput(
          session, paste0("point_size_", i),
          value = 2  # Default size
        )
      }

      # Update point shape
      if (is.null(input[[paste0("point_shape_", i)]])) {
        updateSelectInput(
          session, paste0("point_shape_", i),
          selected = 21  # Default shape
        )
      }
    })
  })

  # Dynamic UI for point colors
  output$dynamic_point_color <- renderUI({
    req(input$group_vals)  # Ensure groups are selected
    groups <- input$group_vals

    lapply(seq_along(groups), function(i) {
      colourpicker::colourInput(
        inputId = paste0("point_color_", i),
        label = paste("Point Border Color for Group", groups[i]),
        value = scales::hue_pal()(length(groups))[i]  # Default color
      )
    })
  })

  # Dynamic UI for point fills
  output$dynamic_point_fill <- renderUI({
    req(input$group_vals)  # Ensure groups are selected
    groups <- input$group_vals

    lapply(seq_along(groups), function(i) {
      colourpicker::colourInput(
        inputId = paste0("point_fill_", i),
        label = paste("Point Fill for Group", groups[i]),
        value = scales::hue_pal()(length(groups))[i]  # Default fill
      )
    })
  })

  # Dynamic UI for point sizes
  output$dynamic_point_size <- renderUI({
    req(input$group_vals)  # Ensure groups are selected
    groups <- input$group_vals

    tagList(
      lapply(seq_along(groups), function(i) {
        group <- groups[i]
        numericInput(
          inputId = paste0("point_size_", i),
          label = paste("Point Size for Group", group),
          value = 2,  # Default size
          min = 0.1, step = 0.1
        )
      })
    )
  })

  # Dynamic UI for point shapes
  output$dynamic_point_shape <- renderUI({
    req(input$group_vals)  # Ensure groups are selected
    groups <- input$group_vals

    tagList(
      lapply(seq_along(groups), function(i) {
        group <- groups[i]
        selectInput(
          inputId = paste0("point_shape_", i),
          label = paste("Point Shape for Group", group),
          choices = c(
            "Circle (21)" = 21,
            "Square (22)" = 22,
            "Diamond (23)" = 23,
            "Triangle (up, 24)" = 24,
            "Triangle (down, 25)" = 25
          ),
          selected = 21  # Default shape
        )
      })
    )
  })

  output$dynamic_hull_fill <- renderUI({
    req(input$group_vals)
    group_vals <- if ("All" %in% input$group_vals) {
      unique(dataset()[[input$group_col]])
    } else {
      input$group_vals
    }

    lapply(seq_along(group_vals), function(i) {
      colourpicker::colourInput(
        inputId = paste0("hull_fill_", i),
        label = paste("Hull Fill for Group", group_vals[i]),
        value = scales::hue_pal()(length(group_vals))[i]
      )
    })
  })

  output$dynamic_hull_color <- renderUI({
    req(input$group_vals)
    group_vals <- if ("All" %in% input$group_vals) {
      unique(dataset()[[input$group_col]])
    } else {
      input$group_vals
    }

    lapply(seq_along(group_vals), function(i) {
      colourpicker::colourInput(
        inputId = paste0("hull_color_", i),
        label = paste("Hull Border Color for Group", group_vals[i]),
        value = "black"
      )
    })
  })


  # Dynamic UI for function-specific inputs
  output$dynamic_ui <- renderUI({
    req(input$function_select)
    switch(input$function_select,
           "Shape Plot" = tagList(
             selectInput("x_col", "Select X-Axis Column", choices = NULL),
             selectInput("y_col", "Select Y-Axis Column", choices = NULL),
             selectInput("group_col", "Select Grouping Column", choices = NULL),
             selectInput("group_vals", "Select Groups to Show", multiple = TRUE, choices = NULL),

             # Inside output$dynamic_ui (under "Shape Plot" tagList)
             checkboxInput("toggle_plot_settings", "Show Plot & Coordinate Settings Menu", value = FALSE),
             conditionalPanel(
               condition = "input.toggle_plot_settings == true",  # Only show if checkbox is checked
               h4("Plot & Coordinate Settings"),

               # Title and axis labels
               textInput("plot_title", "Plot Title", value = ""),
               textInput("x_axis_label", "X-Axis Label", value = ""),
               textInput("y_axis_label", "Y-Axis Label", value = ""),

               # Font sizes
               numericInput("title_size", "Title Font Size", value = 24, min = 1),
               numericInput("x_label_size", "X-Label Font Size", value = 5, min = 1),
               numericInput("y_label_size", "Y-Label Font Size", value = 5, min = 1),

               # Tick settings
               numericInput("tick_size", "Tick Font Size", value = 15, min = 1),
               numericInput("tick_length", "Tick Length", value = 0.005, min = 0, step = 0.001),

               # Axis label adjustments
               numericInput("x_label_adjust_x", "X-Label Horizontal Adjust", value = 0),
               numericInput("x_label_adjust_y", "X-Label Vertical Adjust", value = 0),
               numericInput("y_label_adjust_x", "Y-Label Horizontal Adjust", value = 0),
               numericInput("y_label_adjust_y", "Y-Label Vertical Adjust", value = 0),

               # Axis and plot style settings
               numericInput("axis_linewidth", "Axis Line Width", value = 1, min = 0.1, step = 0.1),
               selectInput(
                 "plot_style",
                 "Plot Style",
                 choices = c("Haug", "inverted_Haug", "publication"),
                 selected = "Haug"
               ),
               checkboxInput("rotate_y_label", "Rotate Y-Axis Label", value = FALSE),
               checkboxInput("show_label_text_fields", "Show Label Borders", value = TRUE)
             ),

             # Point Styles Section with toggle visibility
             checkboxInput("toggle_point_styles", "Show Point Styles Menu", value = TRUE),
             conditionalPanel(
               condition = "input.toggle_point_styles == true",  # Only show if checkbox is checked
               h4("Point Styles"),
               uiOutput("dynamic_point_fill"),  # Dynamic color pickers for point fill
               uiOutput("dynamic_point_color"),  # Dynamic color pickers for point border
               uiOutput("dynamic_point_size"),  # Dynamic inputs for point sizes
               uiOutput("dynamic_point_shape")  # Dynamic inputs for point shapes
             ),

             # Hull Styles Section based on show_hulls
             checkboxInput("show_hulls", "Show Convex Hulls", value = FALSE),
             conditionalPanel(
               condition = "input.show_hulls == true",  # Show only if 'Show Convex Hulls' is checked
               h4("Hull Styles"),
               selectInput("show_hull_for_groups", "Select Hulls to Show", multiple = TRUE, choices = NULL),
               uiOutput("dynamic_hull_fill"),  # Dynamic color pickers for hull fill
               uiOutput("dynamic_hull_color"),  # Dynamic color pickers for hull border
               selectInput(
                 "hull_linetype",
                 "Hull Line Type",
                 choices = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash"),
                 selected = "solid"
               ),
               sliderInput("hull_alpha", "Hull Transparency (Alpha)", min = 0, max = 1, value = 0.1, step = 0.1)
             ),

             # Shape Styles Section with toggle visibility
             checkboxInput("show_shapes", "Show Shapes", value = FALSE),
             conditionalPanel(
               condition = "input.show_shapes == true",  # Only show if checkbox is checked
               h4("Shape Styles"),

               # Groups for shapes
               selectInput(
                 "show_shapes_for_groups",
                 "Select Groups for Shapes",
                 multiple = TRUE,
                 choices = NULL
               ),

               # Shape size
               numericInput(
                 "shape_size",
                 "Shape Size",
                 value = 0.01,
                 min = 0.001,
                 step = 0.001
               ),

               # Shape shift
               numericInput(
                 "shape_shift",
                 "Shape Shift",
                 value = 0.1,
                 min = 0,
                 step = 0.01
               ),

               # Adjustments for shape positions
               numericInput(
                 "shape_x_adjust",
                 "Shape X-Adjust",
                 value = 0,
                 step = 0.01
               ),
               numericInput(
                 "shape_y_adjust",
                 "Shape Y-Adjust",
                 value = 0,
                 step = 0.01
               )
             ),


             # Contours Section
             checkboxInput("show_contours", "Show Contours", value = FALSE),
             conditionalPanel(
               condition = "input.show_contours == true",
               h4("Contours Settings"),

               # Group selection for contours
               selectInput(
                 "show_contours_for_groups",
                 "Select Group for Contours",
                 multiple = FALSE,
                 choices = NULL
               ),

               # Contour line color
               colourpicker::colourInput(
                 inputId = "contour_colors",
                 label = "Contour Line Color",
                 value = "black"
               ),

               # Contour line width
               numericInput(
                 inputId = "contour_linewidth",
                 label = "Contour Line Width",
                 value = 0.5,
                 min = 0.1,
                 step = 0.1
               )
             )
           ),

           "Cluster Plot" = tagList(
             selectInput("x_col", "Select X-Axis Column", choices = NULL),
             selectInput("y_col", "Select Y-Axis Column", choices = NULL),
             sliderInput("num_clusters", "Number of Clusters", min = 2, max = 10, value = 3),
             selectInput("clustering_method", "Clustering Method",
                         choices = c("kmeans", "hierarchical", "distance_threshold")),
             numericInput("distance_threshold", "Distance Threshold (if applicable)", value = NULL, min = 0, step = 0.1),

             # Shape Styles Section
             checkboxInput("show_shapes", "Show Shapes", value = FALSE),
             conditionalPanel(
               condition = "input.show_shapes == true",  # Only show if checkbox is checked
               h4("Shape Styles"),

               # Shape parameters
               numericInput("shape_size", "Shape Size", value = 0.01, min = 0.001, step = 0.001),
               numericInput("shape_x_adjust", "Shape X-Adjust", value = 0, step = 0.01),
               numericInput("shape_y_adjust", "Shape Y-Adjust", value = 0, step = 0.01)
             )
           ),

           "Elbow Plot" = tagList(
             selectInput("x_col", "Select X-Axis Column", choices = NULL),
             selectInput("y_col", "Select Y-Axis Column", choices = NULL),
             numericInput("max_k", "Maximum Number of Clusters", value = 10, min = 2)
           ),
           "Overview Plot" = tagList(
             h4("Overview Plot Settings"),
             selectizeInput(
               inputId = "overview_cols",
               label = "Select Columns for Overview (must be even number up to 20)",
               choices = NULL,  # Dynamically updated
               multiple = TRUE,
               options = list(maxItems = 6)
             ),
             selectInput(
               inputId = "overview_group_col",
               label = "Select Grouping Column",
               choices = NULL
             ),
             selectInput(
               inputId = "overview_group_vals",
               label = "Select Groups to Include (Optional)",
               choices = NULL,
               multiple = TRUE
             ),
             checkboxInput("overview_export_pdf", "Export Overview to PDF", value = FALSE),
             conditionalPanel(
               condition = "input.overview_export_pdf == true",
               tagList(
                 textInput("overview_pdf_name", "PDF File Name", value = "overview_plots.pdf"),
                 checkboxInput("overview_show_all_hulls", "Show All Hulls (in PDF)", value = FALSE),
                 checkboxInput("overview_show_all_contours", "Show All Contours (in PDF)", value = FALSE),
                 checkboxInput("overview_show_table", "Show Specimen Table (in PDF)", value = FALSE)
               )

             ),
             actionButton("generate_overview", "Generate Overview Plot")
           )
    )
  })

  # Output Plot
  output$output_plot <- renderPlot({
    req(input$function_select, dataset(), input$x_col, input$y_col)

    # Determine group_vals based on user selection
    group_vals <- if ("All" %in% input$group_vals) {
      unique(dataset()[[input$group_col]])  # Include all groups
    } else {
      input$group_vals  # Use selected groups
    }

    shapes_for_groups <- if ("All" %in% input$show_shapes_for_groups) {
      unique(dataset()[[input$group_col]])
    } else {
      input$show_shapes_for_groups
    }

    # Extract dynamic point styles
    point_color <- sapply(seq_along(group_vals), function(i) {
      input[[paste0("point_color_", i)]]
    })

    point_fill <- sapply(seq_along(group_vals), function(i) {
      input[[paste0("point_fill_", i)]]
    })

    point_size <- sapply(seq_along(group_vals), function(i) {
      input[[paste0("point_size_", i)]]
    })

    point_shape <- as.numeric(sapply(seq_along(group_vals), function(i) {
      input[[paste0("point_shape_", i)]]
    }))

    # Extract dynamic hull styles
    hull_fill <- sapply(seq_along(group_vals), function(i) {
      input[[paste0("hull_fill_", i)]]
    })

    hull_color <- sapply(seq_along(group_vals), function(i) {
      input[[paste0("hull_color_", i)]]
    })

    switch(input$function_select,
           # Inside output$output_plot renderPlot (under "Shape Plot" in the switch statement)
           "Shape Plot" = shape_plot(
             data = dataset(),
             x_col = input$x_col,
             y_col = input$y_col,
             group_col = input$group_col,
             group_vals = group_vals,
             point_color = point_color,
             point_fill = point_fill,
             point_size = point_size,
             point_shape = point_shape,
             show_hulls = input$show_hulls,
             show_hull_for_groups = input$show_hull_for_groups,
             hull_fill = hull_fill,
             hull_color = hull_color,
             hull_linetype = input$hull_linetype,
             hull_alpha = input$hull_alpha,

             # Shape parameters
             show_shapes = input$show_shapes,
             show_shapes_for_groups = shapes_for_groups,
             shape_size = input$shape_size,
             shape_shift = input$shape_shift,
             shape_x_adjust = input$shape_x_adjust,
             shape_y_adjust = input$shape_y_adjust,

             # Plot & Coordinate Settings
             title = ifelse(input$plot_title == "", "", input$plot_title),  # Default to empty string for title
             x_label = ifelse(input$x_axis_label == "", input$x_col, input$x_axis_label),  # Default to x_col value
             y_label = ifelse(input$y_axis_label == "", input$y_col, input$y_axis_label),  # Default to y_col value
             title_size = input$title_size,
             x_label_size = input$x_label_size,
             y_label_size = input$y_label_size,
             tick_size = input$tick_size,
             tick_length = input$tick_length,
             x_label_adjust_x = input$x_label_adjust_x,
             x_label_adjust_y = input$x_label_adjust_y,
             y_label_adjust_x = input$y_label_adjust_x,
             y_label_adjust_y = input$y_label_adjust_y,
             axis_linewidth = input$axis_linewidth,
             plot_style = input$plot_style,
             rotate_y_label = input$rotate_y_label,
             show_label_text_fields = input$show_label_text_fields
           ),

           "Cluster Plot" = cluster_plot(
             data = dataset(),
             x_col = input$x_col,
             y_col = input$y_col,
             method = input$clustering_method,
             k = input$num_clusters,
             distance_threshold = input$distance_threshold,
             show_shapes = input$show_shapes,
             shape_size = input$shape_size,
             shape_x_adjust = input$shape_x_adjust,
             shape_y_adjust = input$shape_y_adjust,
             title = input$plot_title,
             x_label = ifelse(input$x_axis_label == "", input$x_col, input$x_axis_label),
             y_label = ifelse(input$y_axis_label == "", input$y_col, input$y_axis_label)
           ),

           "Elbow Plot" = elbow_plot(
             data = dataset(),
             x_col = input$x_col,
             y_col = input$y_col,
             max_k = input$max_k
           )
    )
  })

}

shinyApp(ui, server)
