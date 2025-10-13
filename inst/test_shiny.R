library(shiny)
library(leaflet)
library(sf)
library(htmlwidgets)
library(mapycusmaximus)  # Your package

# Define UI
ui <- fluidPage(
  titlePanel("Interactive Fisheye Transformation"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      # File input for custom sf objects (optional)
      fileInput("sf_file", "Upload Shapefile (optional)",
                accept = c(".shp", ".dbf", ".shx", ".prj"),
                multiple = TRUE),
      
      hr(),
      
      # Fisheye parameters
      h4("Fisheye Parameters"),
      
      sliderInput("r_in", 
                  "Inner Radius (r_in):",
                  min = 0.1, max = 0.8, value = 0.34, step = 0.01),
      
      sliderInput("r_out", 
                  "Outer Radius (r_out):",
                  min = 0.2, max = 1.0, value = 0.5, step = 0.01),
      
      sliderInput("zoom_factor", 
                  "Zoom Factor:",
                  min = 0.5, max = 3.0, value = 1.5, step = 0.1),
      
      sliderInput("squeeze_factor", 
                  "Squeeze Factor:",
                  min = 0.1, max = 1.0, value = 0.35, step = 0.05),
      
      selectInput("method", 
                  "Method:",
                  choices = c("expand", "outward"),
                  selected = "expand"),
      
      sliderInput("revolution", 
                  "Revolution (rotation):",
                  min = -2, max = 2, value = 0, step = 0.1),
      
      hr(),
      
      # Control buttons
      checkboxInput("show_original", "Show Original", value = TRUE),
      checkboxInput("show_transformed", "Show Transformed", value = TRUE),
      checkboxInput("show_zones", "Show Zones", value = TRUE),
      checkboxInput("show_basemap", "Show Background Map", value = FALSE),
      
      actionButton("reset_center", "Reset Center", class = "btn-warning"),
      
      hr(),
      
      # Center coordinates display
      h5("Current Center:"),
      verbatimTextOutput("center_coords"),
      
      # Instructions
      div(
        style = "font-size: 12px; color: #666;",
        h5("Instructions:"),
        p("• Click on the map to set fisheye center"),
        p("• Use sliders to adjust transformation"),
        p("• Toggle layers with checkboxes"),
        p("• Upload your own shapefile (optional)")
      )
    ),
    
    mainPanel(
      width = 9,
      
      # Map output
      leafletOutput("map", height = "700px"),
      
      # Status/info panel
      div(
        style = "margin-top: 10px; padding: 10px; background-color: #f8f9fa; border-radius: 5px;",
        h5("Transformation Info:"),
        fluidRow(
          column(6, verbatimTextOutput("transformation_info")),
          column(6, verbatimTextOutput("zone_counts"))
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive values to store state
  values <- reactiveValues(
    sf_data = NULL,
    center_point = NULL,
    transformed_data = NULL
  )
  
  # Initialize with vic dataset
  observe({
    if (is.null(values$sf_data)) {
      # Load and clean vic dataset
      vic_data <- mapycusmaximus::vic
      
      # Fix geometric validity issues
      vic_data <- st_make_valid(vic_data)
      
      # Ensure proper CRS - transform to WGS84 for leaflet
      if (!st_is_longlat(vic_data)) {
        vic_data <- st_transform(vic_data, 4326)
      } else if (!identical(st_crs(vic_data), st_crs(4326))) {
        # Fix datum issues by explicitly setting to WGS84
        vic_data <- st_transform(vic_data, 4326)
      }
      
      values$sf_data <- vic_data
      
      # Set initial center to centroid of vic - use safer method
      tryCatch({
        # Use bounding box center as fallback to avoid geometry issues
        bbox <- st_bbox(vic_data)
        center_lng <- (bbox["xmin"] + bbox["xmax"]) / 2
        center_lat <- (bbox["ymin"] + bbox["ymax"]) / 2
        values$center_point <- c(center_lng, center_lat)
      }, error = function(e) {
        # Default to Melbourne CBD if everything fails
        values$center_point <- c(144.9631, -37.8136)
      })
    }
  })
  
  # Handle file upload
  observeEvent(input$sf_file, {
    req(input$sf_file)
    
    tryCatch({
      # Handle shapefile upload (multiple files)
      file_paths <- input$sf_file$datapath
      names(file_paths) <- input$sf_file$name
      
      # Find the .shp file
      shp_file <- file_paths[grepl("\\.shp$", names(file_paths))]
      
      if (length(shp_file) > 0) {
        # Read the shapefile
        uploaded_sf <- st_read(shp_file, quiet = TRUE)
        
        # Fix geometric validity
        uploaded_sf <- st_make_valid(uploaded_sf)
        
        # Transform to WGS84 for leaflet if needed
        if (!st_is_longlat(uploaded_sf)) {
          uploaded_sf <- st_transform(uploaded_sf, 4326)
        } else if (!identical(st_crs(uploaded_sf), st_crs(4326))) {
          uploaded_sf <- st_transform(uploaded_sf, 4326)
        }
        
        values$sf_data <- uploaded_sf
        
        # Update center to new data centroid - use bbox center for safety
        bbox <- st_bbox(uploaded_sf)
        center_lng <- (bbox["xmin"] + bbox["xmax"]) / 2
        center_lat <- (bbox["ymin"] + bbox["ymax"]) / 2
        values$center_point <- c(center_lng, center_lat)
        
        showNotification("Shapefile loaded successfully!", type = "success")
      }
    }, error = function(e) {
      showNotification(paste("Error loading file:", e$message), type = "error")
    })
  })
  
  # Apply fisheye transformation
  observe({
    req(values$sf_data, values$center_point)
    req(input$r_out > input$r_in)  # Validation
    
    tryCatch({
      # Prepare data for transformation
      sf_data_clean <- values$sf_data
      
      # Ensure data is valid before transformation
      if (!all(st_is_valid(sf_data_clean))) {
        sf_data_clean <- st_make_valid(sf_data_clean)
      }
      
      # Apply fisheye transformation
      transformed <- sf_fisheye(
        sf_data_clean,
        center = values$center_point,
        center_crs = "EPSG:4326",
        r_in = input$r_in,
        r_out = input$r_out,
        zoom_factor = input$zoom_factor,
        squeeze_factor = input$squeeze_factor,
        method = input$method,
        revolution = input$revolution
      )
      
      # Clean and validate transformed result
      transformed <- st_make_valid(transformed)
      
      # Ensure result is in WGS84 for leaflet
      if (!st_is_longlat(transformed)) {
        transformed <- st_transform(transformed, 4326)
      } else if (!identical(st_crs(transformed), st_crs(4326))) {
        transformed <- st_transform(transformed, 4326)
      }
      
      values$transformed_data <- transformed
      
    }, error = function(e) {
      showNotification(paste("Transformation error:", e$message), type = "error")
      cat("Transformation error details:", e$message, "\n")
    })
  })
  
  # Create base map
  output$map <- renderLeaflet({
    m <- leaflet() %>%
      setView(lng = 144.9631, lat = -37.8136, zoom = 7)  # Melbourne-centered
    
    # Only add tiles if basemap is enabled
    if (input$show_basemap %||% FALSE) {
      m <- m %>% addProviderTiles(providers$CartoDB.Positron)
    }
    
    m
  })
  
  # Update basemap when checkbox changes
  observeEvent(input$show_basemap, {
    if (input$show_basemap) {
      leafletProxy("map") %>%
        addProviderTiles(providers$CartoDB.Positron, group = "basemap")
    } else {
      leafletProxy("map") %>%
        clearTiles()
    }
  })
  
  # Update map when data or parameters change
  observe({
    req(values$sf_data)
    
    leafletProxy("map") %>%
      clearShapes() %>%
      clearMarkers()
    
    # Add original data
    if (input$show_original && !is.null(values$sf_data)) {
      leafletProxy("map") %>%
        addPolygons(
          data = values$sf_data,
          fillColor = "blue",
          fillOpacity = 0.3,
          color = "darkblue",
          weight = 1,
          group = "Original",
          popup = ~if("LGA_NAME" %in% names(.)) LGA_NAME else "Original Polygon"
        )
    }
    
    # Add transformed data
    if (input$show_transformed && !is.null(values$transformed_data)) {
      leafletProxy("map") %>%
        addPolygons(
          data = values$transformed_data,
          fillColor = "red",
          fillOpacity = 0.4,
          color = "darkred",
          weight = 2,
          group = "Transformed",
          popup = ~if("LGA_NAME" %in% names(.)) paste("Transformed:", LGA_NAME) else "Transformed Polygon"
        )
    }
    
    # Add center marker
    if (!is.null(values$center_point)) {
      leafletProxy("map") %>%
        addMarkers(
          lng = values$center_point[1],
          lat = values$center_point[2],
          popup = "Fisheye Center",
          group = "Center"
        )
    }
    
    # Add zone circles if enabled
    if (input$show_zones && !is.null(values$center_point) && !is.null(values$sf_data)) {
      # Calculate approximate radius in degrees for visualization
      # This is a rough approximation - in a real app you'd want more precise calculation
      bbox <- st_bbox(st_transform(values$sf_data, 4326))
      map_width <- bbox["xmax"] - bbox["xmin"]
      map_height <- bbox["ymax"] - bbox["ymin"]
      map_scale <- max(map_width, map_height)
      
      r_in_deg <- input$r_in * map_scale * 0.5
      r_out_deg <- input$r_out * map_scale * 0.5
      
      leafletProxy("map") %>%
        addCircles(
          lng = values$center_point[1],
          lat = values$center_point[2],
          radius = r_in_deg * 111000,  # Rough conversion to meters
          color = "purple",
          fillColor = "purple",
          fillOpacity = 0.1,
          weight = 2,
          dashArray = "5,5",
          group = "Zones",
          popup = paste("Focus Zone (r_in =", input$r_in, ")")
        ) %>%
        addCircles(
          lng = values$center_point[1],
          lat = values$center_point[2],
          radius = r_out_deg * 111000,  # Rough conversion to meters
          color = "orange",
          fillColor = "orange",
          fillOpacity = 0.05,
          weight = 2,
          dashArray = "10,5",
          group = "Zones",
          popup = paste("Glue Zone (r_out =", input$r_out, ")")
        )
    }
  })
  
  # Handle map clicks to set center
  observeEvent(input$map_click, {
    click <- input$map_click
    values$center_point <- c(click$lng, click$lat)
    
    showNotification(
      paste("Center updated to:", round(click$lng, 4), ",", round(click$lat, 4)),
      type = "message"
    )
  })
  
  # Reset center button
  observeEvent(input$reset_center, {
    req(values$sf_data)
    
    tryCatch({
      # Use bbox center as safer alternative to centroid
      bbox <- st_bbox(values$sf_data)
      center_lng <- (bbox["xmin"] + bbox["xmax"]) / 2
      center_lat <- (bbox["ymin"] + bbox["ymax"]) / 2
      values$center_point <- c(center_lng, center_lat)
      
      showNotification("Center reset to data center", type = "message")
    }, error = function(e) {
      # Fallback to Melbourne CBD
      values$center_point <- c(144.9631, -37.8136)
      showNotification("Center reset to default location", type = "message")
    })
  })
  
  # Display current center coordinates
  output$center_coords <- renderText({
    if (!is.null(values$center_point)) {
      paste("Lng:", round(values$center_point[1], 4), 
            "\nLat:", round(values$center_point[2], 4))
    } else {
      "No center set"
    }
  })
  
  # Display transformation info
  output$transformation_info <- renderText({
    paste(
      "r_in:", input$r_in,
      "\nr_out:", input$r_out,
      "\nZoom:", input$zoom_factor,
      "\nSqueeze:", input$squeeze_factor,
      "\nMethod:", input$method,
      "\nRevolution:", input$revolution
    )
  })
  
  # Display zone counts (if applicable)
  output$zone_counts <- renderText({
    if (!is.null(values$sf_data)) {
      n_features <- nrow(values$sf_data)
      paste(
        "Features:", n_features,
        "\nCRS:", st_crs(values$sf_data)$input,
        "\nGeometry:", paste(unique(st_geometry_type(values$sf_data)), collapse = ", ")
      )
    } else {
      "No data loaded"
    }
  })
  
  # Validate slider inputs
  observe({
    if (input$r_out <= input$r_in) {
      showNotification("r_out must be greater than r_in", type = "warning")
      updateSliderInput(session, "r_out", value = input$r_in + 0.1)
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)