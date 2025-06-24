library(shiny)
library(HaugShape)
library(colourpicker)
library(dplyr)
library(shinyFiles)
library(Momocs)
library(ggplot2)
library(DT)
library(readxl)
library(fs)
library(magick)

ui <- navbarPage(
  "HaugShape Shiny App",
  tabPanel("Data & Mapping",
    fluidRow(
      column(4,
        fileInput("datafile", "Upload Your Data (Excel)"),
        shinyDirButton("shape_dir", "Select Shape Folder", "Please select a folder"),
        textOutput("selected_dir"),
        selectInput("shape_id_col", "Select ID Column for Shapes", choices = NULL),
        actionButton("map_shapes", "Map Shapes to Data"),
        textOutput("shape_mapping_status")
      ),
      column(8,
        DT::dataTableOutput("data_preview")
      )
    )
  ),
  tabPanel("Plotting",
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "function_select",
          "Choose Function to Use",
          choices = c("Shape Plot", "Cluster Plot", "Elbow Plot", "Overview Plot"),
          selected = "Shape Plot"
        ),
        uiOutput("dynamic_ui"),
        downloadButton("download_plot", "Download Plot")
      ),
      mainPanel(
        plotOutput("output_plot"),
        tabsetPanel(
          tabPanel("Overview Results", uiOutput("overview_results"))
        )
      )
    )
  ),
  tabPanel("Image Processing",
    tabsetPanel(
      tabPanel("Convert PNG to JPG/BMP",
        selectInput("output_format", "Select Output Format:", choices = c("jpg", "bmp"), selected = "jpg"),
        shinyDirButton("png_input_dir", "Select PNG Folder", "Choose a folder with PNG files"),
        textOutput("png_input_dir_text"),
        shinyDirButton("jpg_output_dir", "Select Output Folder", "Choose a folder for JPG files"),
        textOutput("jpg_output_dir_text"),
        numericInput("resize_width", "Resize Width (pixels, optional):", value = NULL, min = 1),
        numericInput("resize_height", "Resize Height (pixels, optional):", value = NULL, min = 1),
        numericInput("padding", "Padding (pixels):", value = 10, min = 0),
        numericInput("quality", "JPG Quality (0-100):", value = 100, min = 0, max = 100),
        actionButton("convert_images", "Convert Images"),
        textOutput("conversion_status")
      ),
      tabPanel("Cut Images in Half",
        shinyDirButton("cut_input_dir", "Select Input Folder", "Choose a folder with images"),
        textOutput("cut_input_dir_text"),
        shinyDirButton("cut_output_dir", "Select Output Folder", "Choose a folder for processed images"),
        textOutput("cut_output_dir_text"),
        actionButton("cut_images", "Process Images"),
        textOutput("cut_status")
      ),
      tabPanel("Split Images",
        radioButtons(
          "split_mode",
          "Select Mode:",
          choices = c("Single File" = "single", "Batch Processing (Folder)" = "batch"),
          inline = TRUE
        ),
        conditionalPanel(
          condition = "input.split_mode == 'single'",
          fileInput("split_single_file", "Select an Image File (PNG/JPG):")
        ),
        conditionalPanel(
          condition = "input.split_mode == 'batch'",
          shinyDirButton("split_input_dir", "Select Input Folder", "Choose a folder with image files"),
          textOutput("split_input_dir_text")
        ),
        shinyDirButton("split_output_dir", "Select Output Folder", "Choose a folder for processed images"),
        textOutput("split_output_dir_text"),
        selectInput("split_direction", "Split Direction:", choices = c("Horizontal" = "horizontal", "Vertical" = "vertical")),
        sliderInput("split_position", "Split Position (as a fraction):", min = 0, max = 1, value = 0.5, step = 0.01),
        actionButton("process_split_images", "Split Image(s)"),
        textOutput("split_status"),
        conditionalPanel(
          condition = "input.split_mode == 'single' && !is.null(input.split_single_file)",
          h4("Preview of Selected Image"),
          plotOutput("image_preview", height = "400px", width = "100%")
        )
      ),
      tabPanel("Morph Shapes",
        fileInput("morph_image1", "Select First Grayscale Image (PNG/JPG):"),
        fileInput("morph_image2", "Select Second Grayscale Image (PNG/JPG):"),
        shinyDirButton("morph_output_dir", "Select Output Directory", "Choose a folder to save the output"),
        textOutput("morph_output_dir_text"),
        textInput("morph_output_name", "Output File Name (e.g., result.jpg):", value = "morphed.jpg"),
        sliderInput("morph_threshold", "Threshold (0 to 1):", min = 0, max = 1, value = 0.1, step = 0.01),
        numericInput("morph_gamma", "Gamma Correction (e.g., 1.0):", value = 1.0, min = 0.1, step = 0.1),
        actionButton("morph_shapes_button", "Morph Shapes"),
        textOutput("morph_status"),
        h4("Image Previews"),
        fluidRow(
          column(4, h5("First Image"), uiOutput("dynamic_preview_image1")),
          column(4, h5("Second Image"), uiOutput("dynamic_preview_image2")),
          column(4, h5("Morphed Image"), uiOutput("dynamic_preview_morphed"))
        )
      ),
      tabPanel("Complete Halved Shapes",
        shinyDirButton("complete_input_dir", "Select Input Folder", "Choose a folder with halved images"),
        textOutput("complete_input_dir_text"),
        shinyDirButton("complete_output_dir", "Select Output Folder", "Choose a folder for completed images"),
        textOutput("complete_output_dir_text"),
        actionButton("complete_shapes", "Complete Halved Shapes"),
        textOutput("complete_status")
      )
    )
  ),
  tabPanel("Shape Analysis",
    fluidRow(
      column(4,
        h4("Elliptic Fourier Shape Analysis (Momocs)"),
        shinyDirButton("efourier_shape_dir", "Select Shape Folder", "Choose a folder with shape images (JPG)", multiple = FALSE),
        textOutput("efourier_shape_dir_text"),
        shinyDirButton("efourier_output_dir", "Select Output Directory", "Choose a folder for results"),
        textOutput("efourier_output_dir_text"),
        numericInput("efourier_nharm", "Number of Harmonics (nharm)", value = 10, min = 1, step = 1),
        checkboxInput("efourier_norm", "Normalize Descriptors (norm)", value = TRUE),
        selectInput("efourier_start", "Starting Point", choices = c("left", "up", "down", "right"), selected = "left"),
        textInput("efourier_output_file", "Output File Name (Excel)", value = "efourier_pca_scores.xlsx"),
        actionButton("run_efourier_analysis", "Run Elliptic Fourier Analysis")
      ),
      column(8,
        h4("PCA Summary"),
        verbatimTextOutput("efourier_pca_summary"),
        h4("PC Contribution Plot"),
        plotOutput("efourier_pc_contrib_plot"),
        downloadButton("download_efourier_pc_contrib", "Download PC Contribution Plot (TIFF)"),
        h4("PCA Scores (first 10 rows)"),
        DT::dataTableOutput("efourier_pca_scores_table"),
        downloadButton("download_efourier_scores", "Download PCA Scores (Excel)")
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


  # Reactive values for folder paths
  split_input_dir <- reactiveVal(NULL)
  split_output_dir <- reactiveVal(NULL)

  # Handle input folder selection for batch processing
  shinyDirChoose(input, "split_input_dir", roots = volumes, session = session)
  observeEvent(input$split_input_dir, {
    dir_path <- parseDirPath(volumes, input$split_input_dir)
    split_input_dir(dir_path)
    output$split_input_dir_text <- renderText({ paste("Selected Input Folder:", dir_path) })
  })

  # Handle output folder selection
  shinyDirChoose(input, "split_output_dir", roots = volumes, session = session)
  observeEvent(input$split_output_dir, {
    dir_path <- parseDirPath(volumes, input$split_output_dir)
    split_output_dir(dir_path)
    output$split_output_dir_text <- renderText({ paste("Selected Output Folder:", dir_path) })
  })

  # Process images for splitting
  observeEvent(input$process_split_images, {
    # Ensure necessary inputs are provided
    req(input$split_mode, split_output_dir())
    if (input$split_mode == "single") {
      req(input$split_single_file)
      single_file_path <- input$split_single_file$datapath
      tryCatch({
        split_image(
          image_path = single_file_path,
          direction = input$split_direction,
          split_position = input$split_position,
          output_dir = split_output_dir()
        )
        output$split_status <- renderText("Image successfully split and saved!")
      }, error = function(e) {
        output$split_status <- renderText(paste("Error:", e$message))
      })
    } else if (input$split_mode == "batch") {
      req(split_input_dir())
      tryCatch({
        # Retrieve all image files from the folder
        image_files <- list.files(
          split_input_dir(),
          pattern = "\\.(png|jpg|jpeg)$",
          full.names = TRUE,
          ignore.case = TRUE
        )

        # Ensure there are images to process
        if (length(image_files) == 0) {
          output$split_status <- renderText("No valid image files found in the selected folder.")
          return()
        }

        # Process each image
        lapply(image_files, function(file) {
          split_image(
            image_path = file,
            direction = input$split_direction,
            split_position = input$split_position,
            output_dir = split_output_dir()
          )
        })
        output$split_status <- renderText("All images successfully split and saved!")
      }, error = function(e) {
        output$split_status <- renderText(paste("Error during batch processing:", e$message))
      })
    }
  })

  # Reactive value to hold the uploaded image
  uploaded_image <- reactiveVal(NULL)

  # Load the image when a file is uploaded
  observeEvent(input$split_single_file, {
    req(input$split_single_file)
    tryCatch({
      img <- magick::image_read(input$split_single_file$datapath)
      uploaded_image(img)
    }, error = function(e) {
      showNotification("Error loading image: Please ensure it's a valid PNG/JPG file.", type = "error")
    })
  })

  # Preview the image with a dynamic red line
  output$image_preview <- renderPlot({
    req(uploaded_image(), input$split_position, input$split_direction)

    # Get the uploaded image and its dimensions
    img <- uploaded_image()
    img_info <- magick::image_info(img)

    # Extract image width and height
    img_width <- img_info$width
    img_height <- img_info$height

    # Plot the image
    img_array <- as.raster(img)
    plot(1, type = "n", xlim = c(0, img_width), ylim = c(0, img_height), xlab = "", ylab = "", axes = FALSE, asp = 1)
    rasterImage(img_array, xleft = 0, ybottom = 0, xright = img_width, ytop = img_height)

    # Draw the red line based on split direction and position
    if (input$split_direction == "vertical") {
      split_x <- input$split_position * img_width
      abline(v = split_x, col = "red", lwd = 2)
    } else if (input$split_direction == "horizontal") {
      split_y <- input$split_position * img_height
      abline(h = split_y, col = "red", lwd = 2)
    }
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

      # Get the user-selected format (jpg or bmp)
      selected_format <- input$output_format

      # Process each PNG file and save in the selected format
      lapply(png_files, function(file) {
        convert_png_to_image(
          file_path = file,
          output_dir = output_dir(),
          fixed_dim = fixed_dim,
          padding = input$padding,
          quality = input$quality,
          format = selected_format  # Pass the selected format
        )
      })

      output$conversion_status <- renderText(paste("All PNG images have been successfully converted to", selected_format, "!"))
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

  # Reactive value for the output directory
  output_dir <- reactiveVal(NULL)

  # Handle output directory selection
  shinyDirChoose(input, "morph_output_dir", roots = volumes, session = session)
  observeEvent(input$morph_output_dir, {
    dir_path <- parseDirPath(volumes, input$morph_output_dir)
    output_dir(dir_path)
    output$morph_output_dir_text <- renderText({ paste("Selected Output Directory:", dir_path) })
  })

  observeEvent(input$morph_shapes_button, {
    req(input$morph_image1, input$morph_image2, output_dir(), input$morph_output_name)

    # Paths for the input images and output
    image1_path <- input$morph_image1$datapath
    image2_path <- input$morph_image2$datapath
    output_path <- file.path(output_dir(), input$morph_output_name)

    tryCatch({
      # Call the morph_shapes function
      morph_shapes(
        image1_path = image1_path,
        image2_path = image2_path,
        output_path = output_path,
        threshold = input$morph_threshold,
        gamma = input$morph_gamma
      )
      # Read the morphed image for preview
      morphed_image(magick::image_read(output_path))
      # Update status on success
      output$morph_status <- renderText(paste("Morphed image saved to:", output_path))
    }, error = function(e) {
      # Handle errors
      output$morph_status <- renderText(paste("Error during morphing:", e$message))
      morphed_image(NULL)  # Reset the morphed image preview
    })
  })

  # Reactive values for input images
  input_image1 <- reactive({
    req(input$morph_image1)
    magick::image_read(input$morph_image1$datapath)
  })

  input_image2 <- reactive({
    req(input$morph_image2)
    magick::image_read(input$morph_image2$datapath)
  })

  # Render the first input image
  output$preview_image1 <- renderPlot({
    req(input_image1())
    img <- input_image1()
    img_info <- magick::image_info(img)
    img_raster <- as.raster(img)
    plot(1, type = "n", xlim = c(0, img_info$width), ylim = c(0, img_info$height),
         axes = FALSE, xlab = "", ylab = "", asp = img_info$height / img_info$width)
    rasterImage(img_raster, 0, 0, img_info$width, img_info$height)
  })

  # Render the second input image
  output$preview_image2 <- renderPlot({
    req(input_image2())
    img <- input_image2()
    img_info <- magick::image_info(img)
    img_raster <- as.raster(img)
    plot(1, type = "n", xlim = c(0, img_info$width), ylim = c(0, img_info$height),
         axes = FALSE, xlab = "", ylab = "", asp = img_info$height / img_info$width)
    rasterImage(img_raster, 0, 0, img_info$width, img_info$height)
  })

  # Render the morphed image
  output$morphed_image_preview <- renderPlot({
    req(morphed_image())
    img <- morphed_image()
    img_info <- magick::image_info(img)
    img_raster <- as.raster(img)
    plot(1, type = "n", xlim = c(0, img_info$width), ylim = c(0, img_info$height),
         axes = FALSE, xlab = "", ylab = "", asp = img_info$height / img_info$width)
    rasterImage(img_raster, 0, 0, img_info$width, img_info$height)
  })

  # Dynamic UI for the first image preview
  output$dynamic_preview_image1 <- renderUI({
    req(input_image1())
    img_info <- magick::image_info(input_image1())
    plotOutput("preview_image1", height = paste0(img_info$height / img_info$width * 300, "px"), width = "100%")
  })

  # Dynamic UI for the second image preview
  output$dynamic_preview_image2 <- renderUI({
    req(input_image2())
    img_info <- magick::image_info(input_image2())
    plotOutput("preview_image2", height = paste0(img_info$height / img_info$width * 300, "px"), width = "100%")
  })

  # Dynamic UI for the morphed image preview
  output$dynamic_preview_morphed <- renderUI({
    req(morphed_image())
    img_info <- magick::image_info(morphed_image())
    plotOutput("morphed_image_preview", height = paste0(img_info$height / img_info$width * 300, "px"), width = "100%")
  })


  # Reactive value for the morphed image
  morphed_image <- reactiveVal(NULL)

  # Perform the morphing process and store the resulting image
  observeEvent(input$morph_shapes_button, {
    req(input$morph_image1, input$morph_image2, input$morph_output_path)

    # Paths for the input images and output
    image1_path <- input$morph_image1$datapath
    image2_path <- input$morph_image2$datapath
    output_path <- file.path(getwd(), input$morph_output_path)

    tryCatch({
      # Call the morph_shapes function
      morph_shapes(
        image1_path = image1_path,
        image2_path = image2_path,
        output_path = output_path,
        threshold = input$morph_threshold,
        gamma = input$morph_gamma
      )
      # Read the morphed image for preview
      morphed_image(magick::image_read(output_path))
      # Update status on success
      output$morph_status <- renderText(paste("Morphed image saved to:", output_path))
    }, error = function(e) {
      # Handle errors
      output$morph_status <- renderText(paste("Error during morphing:", e$message))
      morphed_image(NULL)  # Reset the morphed image preview
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
      # 1. Import shape images as Out object
      shape_files <- list.files(shape_analysis_dir(), pattern = "\\.(jpg|jpeg|png)$", full.names = TRUE, ignore.case = TRUE)
      if (length(shape_files) == 0) stop("No shape images found in the selected folder.")
      shapes <- Momocs::import_jpg(shape_files)

      # 2. Preprocess: center, scale, slide start point
      shapes <- Momocs::coo_center(shapes)
      shapes <- Momocs::coo_scale(shapes)
      shapes <- Momocs::coo_slidedirection(shapes, direction = input$start_point)

      # 3. Stack and panel preview (optional, not shown in UI)
      # output$shape_stack_plot <- renderPlot({ Momocs::stack(shapes) })
      # output$shape_panel_plot <- renderPlot({ Momocs::panel(shapes) })

      # 4. Elliptic Fourier analysis
      ef <- Momocs::efourier(shapes, norm = input$norm)

      # 5. PCA
      pca <- Momocs::PCA(ef)

      # 6. PCcontrib plot
      pc_contrib_plot <- Momocs::PCcontrib(pca, nax = input$num_pcs, plot = FALSE)

      # 7. Prepare summary
      pca_summary <- capture.output(summary(pca))

      # 8. Export PC scores to Excel
      pc_scores <- as.data.frame(pca$x)
      pc_scores$filename <- Momocs::fac(shapes)$file
      out_path <- file.path(output_dir(), input$output_file)
      suppressMessages({
        if (requireNamespace("writexl", quietly = TRUE)) {
          writexl::write_xlsx(pc_scores, out_path)
        } else if (requireNamespace("openxlsx", quietly = TRUE)) {
          openxlsx::write.xlsx(pc_scores, out_path)
        } else {
          stop("Please install either 'writexl' or 'openxlsx' package for Excel export.")
        }
      })

      # 9. Show results in UI
      output$pca_summary <- renderText({ paste(pca_summary, collapse = "\n") })
      output$pc_contrib_plot <- renderPlot({ pc_contrib_plot })

      showNotification("Shape analysis completed successfully! PC scores exported.", type = "message")
    }, error = function(e) {
      showNotification(paste("Error during analysis:", e$message), type = "error")
    })
  })

  # --- ELLIPTIC FOURIER SHAPE ANALYSIS TAB LOGIC ---

  # Reactive values for efourier directories and results
  efourier_shape_dir <- reactiveVal(NULL)
  efourier_output_dir <- reactiveVal(NULL)
  efourier_pca_scores <- reactiveVal(NULL)
  efourier_pc_contrib_plot_obj <- reactiveVal(NULL)
  efourier_pca_summary <- reactiveVal(NULL)

  # Handle shape folder selection
  shinyDirChoose(input, "efourier_shape_dir", roots = volumes, session = session)
  observeEvent(input$efourier_shape_dir, {
    dir_path <- parseDirPath(volumes, input$efourier_shape_dir)
    efourier_shape_dir(dir_path)
    output$efourier_shape_dir_text <- renderText({ paste("Selected Shape Folder:", dir_path) })
  })

  # Handle output directory selection
  shinyDirChoose(input, "efourier_output_dir", roots = volumes, session = session)
  observeEvent(input$efourier_output_dir, {
    dir_path <- parseDirPath(volumes, input$efourier_output_dir)
    efourier_output_dir(dir_path)
    output$efourier_output_dir_text <- renderText({ paste("Selected Output Directory:", dir_path) })
  })

  # Run Elliptic Fourier Analysis
  observeEvent(input$run_efourier_analysis, {
    req(efourier_shape_dir(), efourier_output_dir())
    tryCatch({
      # 1. Import shape images
      shape_files <- list.files(efourier_shape_dir(), pattern = "\\.(jpg|jpeg|png)$", full.names = TRUE, ignore.case = TRUE)
      if (length(shape_files) == 0) stop("No shape images found in the selected folder.")
      shapes <- Momocs::import_jpg(shape_files)

      # 2. Preprocess
      shapes <- Momocs::coo_center(shapes)
      shapes <- Momocs::coo_scale(shapes)
      shapes <- Momocs::coo_slidedirection(shapes, direction = input$efourier_start)

      # 3. Elliptic Fourier analysis
      ef <- Momocs::efourier(shapes, nb.h = input$efourier_nharm, norm = input$efourier_norm)

      # 4. PCA
      pca <- Momocs::PCA(ef)

      # 5. PCcontrib plot (as ggplot object)
      pc_contrib_plot <- Momocs::PCcontrib(pca, nax = 2, plot = FALSE) # Always 2 PCs for plot
      efourier_pc_contrib_plot_obj(pc_contrib_plot)

      # 6. Prepare summary
      pca_summary <- capture.output(summary(pca))
      efourier_pca_summary(paste(pca_summary, collapse = "\n"))

      # 7. Export PC scores to Excel
      pc_scores <- as.data.frame(pca$x)
      pc_scores$filename <- Momocs::fac(shapes)$file
      efourier_pca_scores(pc_scores)
      out_path <- file.path(efourier_output_dir(), input$efourier_output_file)
      suppressMessages({
        if (requireNamespace("writexl", quietly = TRUE)) {
          writexl::write_xlsx(pc_scores, out_path)
        } else if (requireNamespace("openxlsx", quietly = TRUE)) {
          openxlsx::write.xlsx(pc_scores, out_path)
        } else {
          stop("Please install either 'writexl' or 'openxlsx' package for Excel export.")
        }
      })

      # 8. Show results in UI
      output$efourier_pca_summary <- renderText({ efourier_pca_summary() })
      output$efourier_pc_contrib_plot <- renderPlot({ efourier_pc_contrib_plot_obj() })
      output$efourier_pca_scores_table <- DT::renderDataTable({ head(efourier_pca_scores(), 10) })

      showNotification("Elliptic Fourier analysis completed! PC scores and plot export available.", type = "message")
    }, error = function(e) {
      showNotification(paste("Error during elliptic Fourier analysis:", e$message), type = "error")
    })
  })

  # Download handler for PCA scores
  output$download_efourier_scores <- downloadHandler(
    filename = function() {
      input$efourier_output_file
    },
    content = function(file) {
      req(efourier_pca_scores())
      if (requireNamespace("writexl", quietly = TRUE)) {
        writexl::write_xlsx(efourier_pca_scores(), file)
      } else if (requireNamespace("openxlsx", quietly = TRUE)) {
        openxlsx::write.xlsx(efourier_pca_scores(), file)
      } else {
        stop("Please install either 'writexl' or 'openxlsx' package for Excel export.")
      }
    }
  )

  # Download handler for PC contribution plot
  output$download_efourier_pc_contrib <- downloadHandler(
    filename = function() {
      "efourier_pc_contrib_plot.tiff"
    },
    content = function(file) {
      req(efourier_pc_contrib_plot_obj())
      ggsave(
        filename = file,
        plot = efourier_pc_contrib_plot_obj(),
        device = "tiff",
        dpi = 600,
        width = 8,
        height = 6
      )
    }
  )

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

    # Validate directory if export_pdf is selected
    if (input$overview_export_pdf && is.null(overview_pdf_dir())) {
      showNotification("Please select an output directory for the PDF.", type = "error")
      return(NULL)
    }

    # Call Haug_overview with the new output directory parameter
    result <- Haug_overview(
      data = dataset(),
      cols = input$overview_cols,
      group_col = ifelse(input$overview_group_col == "", NULL, input$overview_group_col),
      group_vals = if (is.null(input$overview_group_vals) || length(input$overview_group_vals) == 0) NULL else input$overview_group_vals,
      show_all_hulls = input$overview_show_all_hulls,
      show_all_contours = input$overview_show_all_contours,
      show_table = input$overview_show_table,
      export_pdf = input$overview_export_pdf,
      pdf_file_name = input$overview_pdf_name,
      output_dir = overview_pdf_dir()  # Pass the selected directory
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
      paste0(input$function_select, "_output", ".tiff")
    },
    content = function(file) {
      tryCatch({
        req(input$function_select, dataset(), input$x_col, input$y_col)

        # Generate the plot dynamically based on user inputs
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
                                 show_hull_for_groups = input$show_hull_for_groups,
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
                                 title = ifelse(input$plot_title == "", "", input$plot_title),
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

        # Export: width fixed at 4000 px, height is user-defined ratio * 4000 px, dpi = 300
        width_px <- 4000
        height_px <- as.numeric(input$export_plot_height_ratio) * 4000
        ggsave(
          filename = file,
          plot = plot_to_save,
          device = "tiff",
          dpi = 300,
          width = width_px / 300,   # inches
          height = height_px / 300  # inches
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


  # Observe the Map Shapes button
  observeEvent(input$map_shapes, {
    req(dataset(), input$shape_id_col, shape_directory())

    # Extract the selected ID column
    id_col <- input$shape_id_col

    tryCatch({
      # 1. Read and preprocess the data
      data <- dataset() %>%
        select(all_of(id_col)) %>%
        distinct() %>%
        mutate(across(everything(), ~ trimws(as.character(.))))  # Trim whitespace

      # 2. List and preprocess shape files
      shape_files <- list.files(shape_directory(), pattern = "\\.(jpg|jpeg|png)$", full.names = TRUE, ignore.case = TRUE)
      shape_files <- basename(shape_files)  # Keep only file names

      # 3. Match shapes to data
      matched_shapes <- data %>%
        mutate(shape_file = paste0(!!sym(id_col), ".jpg")) %>%  # Assuming shape files are in JPG format
        filter(shape_file %in% shape_files)

      # 4. Update the mapping status
      output$shape_mapping_status <- renderText({
        paste("Mapped", nrow(matched_shapes), "shapes to data out of", nrow(data), "total IDs.")
      })

      # 5. Save the mapping result as a CSV file
      output_file <- file.path(shape_directory(), "shape_mapping_result.csv")
      write.csv(matched_shapes, output_file, row.names = FALSE)

      showNotification(paste("Shape mapping completed. Results saved to:", output_file), type = "message")
    }, error = function(e) {
      showNotification(paste("Error during shape mapping:", e$message), type = "error")
    })
  })

  # --- END OF SERVER LOGIC ---
}

shinyApp(ui, server)
