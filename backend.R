library(googleway)
library(dplyr)
library(DT)
library(htmltools)
library(jsonlite)


source("api_key.R")
bus_routes <- read.csv("bus_routes.csv", stringsAsFactors = FALSE)
map_style <- '[
  {
    "elementType": "geometry",
    "stylers": [
      {
        "color": "#f5f5f5"
      }
    ]
  },
  {
    "elementType": "labels.icon",
    "stylers": [
      {
        "visibility": "on"
      }
    ]
  },
  {
    "elementType": "labels.text.fill",
    "stylers": [
      {
        "color": "#616161"
      }
    ]
  },
  {
    "elementType": "labels.text.stroke",
    "stylers": [
      {
        "color": "#f5f5f5"
      }
    ]
  },
  {
    "featureType": "administrative.land_parcel",
    "elementType": "labels.text.fill",
    "stylers": [
      {
        "color": "#bdbdbd"
      }
    ]
  },
  {
    "featureType": "poi",
    "elementType": "geometry",
    "stylers": [
      {
        "color": "#eeeeee"
      }
    ]
  },
  {
    "featureType": "poi",
    "elementType": "labels.text.fill",
    "stylers": [
      {
        "color": "#757575"
      }
    ]
  },
  {
    "featureType": "poi.park",
    "elementType": "geometry",
    "stylers": [
      {
        "color": "#e5f5e0"
      }
    ]
  },
  {
    "featureType": "poi.park",
    "elementType": "labels.text.fill",
    "stylers": [
      {
        "color": "#9e9e9e"
      }
    ]
  },
  {
    "featureType": "road",
    "elementType": "geometry",
    "stylers": [
      {
        "color": "#ffffff"
      }
    ]
  },
  {
    "featureType": "road.arterial",
    "elementType": "labels.text.fill",
    "stylers": [
      {
        "color": "#757575"
      }
    ]
  },
  {
    "featureType": "road.highway",
    "elementType": "geometry",
    "stylers": [
      {
        "color": "#dadada"
      }
    ]
  },
  {
    "featureType": "road.highway",
    "elementType": "labels.text.fill",
    "stylers": [
      {
        "color": "#616161"
      }
    ]
  },
  {
    "featureType": "road.local",
    "elementType": "labels.text.fill",
    "stylers": [
      {
        "color": "#9e9e9e"
      }
    ]
  },
  {
    "featureType": "transit.line",
    "elementType": "geometry",
    "stylers": [
      {
        "color": "#e5e5e5"
      }
    ]
  },
  {
    "featureType": "transit.station",
    "elementType": "geometry",
    "stylers": [
      {
        "color": "#eeeeee"
      }
    ]
  },
  {
    "featureType": "water",
    "elementType": "geometry",
    "stylers": [
      {
        "color": "#c9e9ff"
      }
    ]
  },
  {
    "featureType": "water",
    "elementType": "labels.text.fill",
    "stylers": [
      {
        "color": "#9e9e9e"
      }
    ]
  }
]'

server <- function(input, output, session) {
  rv <- reactiveValues(
    position_index = 1,
    timer_active = FALSE,
    last_update = Sys.time(),
    initialized = FALSE,
    current_route = NULL
  )
  output$bus_select_ui <- renderUI({
    req(input$route_select)
    route_buses <- bus_routes %>%
      filter(route_id == as.numeric(input$route_select)) %>%
      pull(bus_id) %>%
      unique()
    bus_choices <- setNames(
      as.character(route_buses),
      paste("Bus", route_buses)
    )

    rv$timer_active <- FALSE
    rv$initialized <- FALSE
    pickerInput(
      inputId = "bus_select",
      label = tags$span(icon("bus"), "Select Bus"),
      choices = bus_choices,
      options = list(
        style = "btn-dark",
        size = 5
      )
    )
  })
  
  output$tracking_status <- renderUI({
    if(rv$timer_active) {
      div(
        style = "background-color: #28a745; color: white; padding: 5px; border-radius: 5px;",
        icon("play-circle"), "Tracking Active"
      )
    } else {
      div(
        style = "background-color: #dc3545; color: white; padding: 5px; border-radius: 5px;",
        icon("stop-circle"), "Tracking Stopped"
      )
    }
  })

  current_route_data <- reactive({
    req(input$route_select, input$bus_select)
    bus_routes %>% 
      filter(route_id == as.numeric(input$route_select), 
             bus_id == as.numeric(input$bus_select)) %>%
      arrange(sequence)
  })
  
  output$route_data_table <- DT::renderDataTable({
    req(current_route_data())
    data <- current_route_data()
    if(nrow(data) > 0) {
      DT::datatable(
        data %>% select(
          Sequence = sequence,
          Stop = stop_name,
          Type = stop_type,
          Arrival = arrival_time,
          Departure = departure_time
        ),
        options = list(
          pageLength = 10,
          dom = 'ftip',
          scrollX = TRUE
        ),
        rownames = FALSE
      ) %>%
      DT::formatStyle(
        'Type',
        backgroundColor = styleEqual(
          c('Terminal', 'Major', 'Regular'),
          c('#1e4620', '#3f3000', '#15394b')
        ),
        color = styleEqual(
          c('Terminal', 'Major', 'Regular'),
          c('#b4ffbe', '#ffe7b3', '#b3e0ff')
        )
      )
    }
  })
  
  output$map <- renderGoogle_map({
    req(current_route_data())
    route_data <- current_route_data()
    if(nrow(route_data) > 0) {
      google_map(key = api_key,
                 location = c(route_data$latitude[1], route_data$longitude[1]),
                 zoom = 14,
                 map_type_control = TRUE,
                 styles = map_style)
    }
  })
  observe({
    req(current_route_data())
    route_data <- current_route_data()
    if(!rv$initialized && nrow(route_data) > 0) {
      google_map_update(map_id = "map") %>%
        add_polylines(
          data = route_data,
          lat = "latitude", 
          lon = "longitude",
          stroke_colour = "#4099ff",
          stroke_weight = 5,
          stroke_opacity = 0.7
        )
      google_map_update(map_id = "map") %>%
        add_markers(
          data = route_data,
          lat = "latitude",
          lon = "longitude",
          marker_icon = ifelse(route_data$stop_type == "Terminal", 
                              "https://maps.google.com/mapfiles/ms/icons/green-dot.png",
                              ifelse(route_data$stop_type == "Major",
                                     "https://maps.google.com/mapfiles/ms/icons/yellow-dot.png",
                                     "https://maps.google.com/mapfiles/ms/icons/blue-dot.png")),
          info_window = paste(
            "<div style='min-width: 150px;'>",
            "<h4 style='margin-top: 0; border-bottom: 1px solid #ccc; padding-bottom: 5px;'>", 
            route_data$stop_name, "</h4>",
            "<p><strong>Type:</strong> ", route_data$stop_type, "</p>",
            "<p><strong>Arrival:</strong> ", route_data$arrival_time, "</p>",
            "<p><strong>Departure:</strong> ", route_data$departure_time, "</p>",
            "</div>"
          )
        )
      rv$initialized <- TRUE
    }
  })
  output$bus_info <- renderText({
    req(current_route_data())
    if(rv$timer_active) {
      invalidateLater(100)
    }
    current_pos <- current_route_data()[rv$position_index, ]
    paste(
      paste("Bus ID:", current_pos$bus_id),
      paste("Route:", current_pos$route_id),
      paste("Current Stop:", current_pos$stop_name),
      paste("Stop Type:", current_pos$stop_type),
      paste("Arrival Time:", current_pos$arrival_time),
      paste("Departure Time:", current_pos$departure_time),
      paste("Latitude:", current_pos$latitude),
      paste("Longitude:", current_pos$longitude),
      paste("Sequence:", current_pos$sequence),
      paste("Last Updated:", format(rv$last_update, "%H:%M:%S")),
      sep = "\n"
    )
  })
  observe({
    req(current_route_data())
    route_data <- current_route_data()
    if(nrow(route_data) > 0) {
      if(rv$timer_active) {
        invalidateLater(100)
        current_time <- Sys.time()
        if(as.numeric(difftime(current_time, rv$last_update, units = "secs")) >= 2) {
          rv$position_index <- (rv$position_index %% nrow(route_data)) + 1
          rv$last_update <- current_time
          print(paste("Updated to position", rv$position_index, "at", format(current_time, "%H:%M:%S")))
        }
      }
      current_pos <- route_data[rv$position_index, ]
      google_map_update(map_id = "map") %>%
        clear_markers() %>%
        add_markers(
          data = current_pos,
          lat = "latitude",
          lon = "longitude",
          marker_icon = "https://maps.google.com/mapfiles/ms/icons/red-dot.png",
          info_window = paste(
            "<div style='min-width: 150px;'>",
            "<h4 style='margin-top: 0; border-bottom: 1px solid #ccc; padding-bottom: 5px;'>Bus ", 
            current_pos$bus_id, "</h4>",
            "<p><strong>Stop:</strong> ", current_pos$stop_name, "</p>",
            "<p><strong>Arrival:</strong> ", current_pos$arrival_time, "</p>",
            "<p><strong>Departure:</strong> ", current_pos$departure_time, "</p>",
            "</div>"
          )
        )
    }
  })
  
  observeEvent(input$bus_select, {
    rv$position_index = 1
    rv$initialized = FALSE
  }, ignoreNULL = TRUE)
  observeEvent(input$start_button, {
    rv$timer_active <- TRUE
    rv$last_update <- Sys.time()
    rv$initialized <- FALSE
    print("Bus tracking started")
  })
  observeEvent(input$stop_button, {
    rv$timer_active <- FALSE
    print("Bus tracking stopped")
  })
}