library(shiny)
library(HaugShape)
library(colourpicker)
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
        choices = c("Shape Plot", "Cluster Plot", "Elbow Plot"),
        selected = "Shape Plot"
      ),
      uiOutput("dynamic_ui")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data Preview", DT::dataTableOutput("data_preview")),
        tabPanel("Output Plot", plotOutput("output_plot"))
      )
    )
  )
)

server <- function(input, output, session) {
  # Reactive: Load dataset
  dataset <- reactive({
    req(input$datafile)
    readxl::read_excel(input$datafile$datapath)
  })

  # Data Preview
  output$data_preview <- DT::renderDataTable({
    req(dataset())
    dataset()
  })

  # Update column dropdowns when dataset changes
  observeEvent(dataset(), {
    req(dataset())
    cols <- colnames(dataset())
    updateSelectInput(session, "x_col", choices = cols, selected = cols[1])
    updateSelectInput(session, "y_col", choices = cols, selected = cols[2])
    updateSelectInput(session, "group_col", choices = c("", cols), selected = "")
  })


  output$download_plot <- downloadHandler(
    filename = function() {
      paste0("shape_plot_output", ".tiff")  # Default file name
    },
    content = function(file) {
      tryCatch({
        req(dataset(), input$x_col, input$y_col)

        # Generate the plot
        plot_to_save <- shape_plot(
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
          title = input$plot_title,
          x_label = ifelse(input$x_axis_label == "", input$x_col, input$x_axis_label),
          y_label = ifelse(input$y_axis_label == "", input$y_col, input$y_axis_label),
          tick_size = input$tick_size,
          axis_linewidth = input$axis_linewidth
        )

        # Save the plot to the specified file
        ggsave(
          filename = file,
          plot = plot_to_save,
          device = "tiff",
          dpi = 600,
          width = 10,
          height = 7
        )

        showNotification("Plot successfully saved!", type = "message")
      }, error = function(e) {
        showNotification(paste("Error saving plot:", e$message), type = "error")
      })
    }
  )

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
                         choices = c("kmeans", "hierarchical", "distance_threshold"))
           ),

           "Elbow Plot" = tagList(
             selectInput("x_col", "Select X-Axis Column", choices = NULL),
             selectInput("y_col", "Select Y-Axis Column", choices = NULL),
             numericInput("max_k", "Maximum Number of Clusters", value = 10, min = 2)
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
             show_contours = input$show_contours,
             show_contours_for_groups = input$show_contours_for_groups,
             contour_colors = input$contour_colors,
             contour_linewidth = input$contour_linewidth,

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
             k = input$num_clusters
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
