library(googleway)
library(shinydashboard)
library(shinyWidgets)
library(htmltools)
library(DT)

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Mumbai Bus Tracker"),
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar",
      menuItem("Live Map", tabName = "map", icon = icon("map-marker-alt")),
      menuItem("Information", tabName = "info", icon = icon("info"))
    ),
    tags$div(
      class = "controls-container",
      style = "padding: 15px; border-top: 1px solid #1a2035;",
      pickerInput(
        inputId = "route_select",
        label = tags$span(icon("route"), "Select Route"),
        choices = c("Route 1: CST to Andheri" = 1, 
                    "Route 2: BKC to Mahalaxmi" = 2),
        options = list(
          style = "btn-dark",
          size = 5
        )
      ),
      uiOutput("bus_select_ui"),
      tags$div(
        style = "margin-top: 20px; display: flex; justify-content: space-between;",
        actionBttn(
          "start_button", 
          label = "Start",
          icon = icon("play"),
          style = "gradient",
          color = "success",
          size = "sm",
          block = TRUE
        ),
        tags$div(style = "width: 10px"),
        actionBttn(
          "stop_button", 
          label = "Stop",
          icon = icon("stop"),
          style = "gradient",
          color = "danger",
          size = "sm",
          block = TRUE
        )
      ),
      tags$div(
        style = "margin-top: 20px; text-align: center;",
        tags$label(style = "color: #8a94ad;", "Status"),
        uiOutput("tracking_status")
      )
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("body, .content-wrapper, .right-side, .skin-black .main-sidebar, .skin-black .left-side {
          background-color: #1a2035;
          color: #e4e8f0;
        }
        .box {
          background-color: #222b45;
          box-shadow: 0 2px 10px rgba(0,0,0,0.2);
          border-radius: 6px;
          border-top: 3px solid #3c4b64;
        }
        .box-header {
          color: #e4e8f0;
        }
        .box.box-primary {
          border-top-color: #4099ff;
        }
        .box.box-info {
          border-top-color: #2ed8b6;
        }
        .box.box-success {
          border-top-color: #2ed8b6;
        }
        .nav-tabs-custom > .tab-content {
          background: #222b45;
        }
        .form-control {
          background-color: #283046;
          color: #e4e8f0;
          border: 1px solid #3c4b64;
        }
        .form-control:focus {
          border-color: #4099ff;
        }
        .content-wrapper, .right-side {
          background-color: #1a2035;
        }
        .bus-info-box {
          background-color: #283046;
          font-family: 'Courier New', monospace;
          border-left: 4px solid #4099ff;
          padding: 10px;
          height: 300px;
          overflow-y: auto;
          white-space: pre-wrap;
          color: #e4e8f0;
        }
        .legend-item {
          display: flex;
          align-items: center;
          margin-bottom: 8px;
          color: #e4e8f0;
        }
        .legend-color {
          width: 15px;
          height: 15px;
          margin-right: 8px;
          border-radius: 50%;
        }
        .dataTables_wrapper .dataTables_length, 
        .dataTables_wrapper .dataTables_filter, 
        .dataTables_wrapper .dataTables_info {
          color: #e4e8f0;
        }
        table.dataTable tbody tr {
          background-color: #283046;
          color: #e4e8f0;
        }
        table.dataTable.stripe tbody tr.odd {
          background-color: #222b45;
        }
        .dataTables_wrapper .dataTables_paginate .paginate_button {
          color: #e4e8f0 !important;
        }
        .skin-black .main-header .logo {
          background-color: #222b45;
          color: #e4e8f0;
          border-bottom: 0 solid transparent;
          border-right: 1px solid #3c4b64;
        }
        .skin-black .main-header .logo:hover {
          background-color: #1a2035;
        }
        .skin-black .main-header .navbar {
          background-color: #222b45;
          border-bottom: 1px solid #3c4b64;
        }
        .skin-black .main-header .navbar .sidebar-toggle {
          color: #e4e8f0;
        }
        .skin-black .main-header .navbar .sidebar-toggle:hover {
          background-color: #1a2035;
          color: #4099ff;
        }
        .skin-black .main-sidebar {
          background-color: #222b45;
        }
        .skin-black .user-panel > .info, 
        .skin-black .user-panel > .info > a {
          color: #e4e8f0;
        }
        .skin-black .sidebar-menu > li.header {
          color: #8a94ad;
          background: #1a2035;
        }
        .skin-black .sidebar-menu > li > a {
          border-left: 3px solid transparent;
          color: #bac1d4;
        }
        .skin-black .sidebar-menu > li:hover > a, 
        .skin-black .sidebar-menu > li.active > a {
          color: #e4e8f0;
          background: #283046;
          border-left-color: #4099ff;
        }
        .skin-black .sidebar-menu > li > .treeview-menu {
          background: #1a2035;
        }
        .treeview-menu > li > a {
          color: #8a94ad;
        }
        .treeview-menu > li.active > a, .treeview-menu > li > a:hover {
          color: #e4e8f0;
        }
        .selectize-input {
          background: #283046 !important;
          color: #e4e8f0 !important;
          border: 1px solid #3c4b64 !important;
        }
        .selectize-dropdown {
          background: #283046 !important;
          color: #e4e8f0 !important;
          border: 1px solid #3c4b64 !important;
        }
        .selectize-dropdown-content .option {
          color: #e4e8f0 !important;
        }
        .selectize-dropdown-content .option.active {
          background: #4099ff !important;
        }
      "))
    ),
    tabItems(
      tabItem(tabName = "map",
        fluidRow(
          column(
            width = 9,
            box(
              width = NULL,
              title = tagList(icon("map-marked-alt"), "Live Bus Tracking"),
              status = "primary",
              solidHeader = TRUE,
              collapsible = FALSE,
              google_mapOutput("map", height = "600px")
            )
          ),
          column(
            width = 3,
            box(
              width = NULL,
              title = tagList(icon("info-circle"), "Bus Information"),
              status = "info",
              solidHeader = TRUE,
              collapsible = TRUE,
              div(
                class = "bus-info-box",
                verbatimTextOutput("bus_info")
              )
            ),
            box(
              width = NULL,
              title = tagList(icon("list"), "Legend"),
              status = "success",
              solidHeader = TRUE,
              collapsible = TRUE,
              div(
                class = "legend-container",
                div(class = "legend-item", 
                    div(class = "legend-color", style = "background-color: red;"),
                    "Current Bus Position"),
                div(class = "legend-item", 
                    div(class = "legend-color", style = "background-color: blue;"),
                    "Route Path"),
                div(class = "legend-item", 
                    div(class = "legend-color", style = "background-color: green;"),
                    "Terminal Stop"),
                div(class = "legend-item", 
                    div(class = "legend-color", style = "background-color: yellow;"),
                    "Major Stop"),
                div(class = "legend-item", 
                    div(class = "legend-color", style = "background-color: #4285F4;"),
                    "Regular Stop")
              )
            )
          )
        )
      ),
      
      tabItem(tabName = "info",
        fluidRow(
          box(
            width = 12,
            title = "About Mumbai Bus Tracking System",
            status = "primary",
            solidHeader = TRUE,
            tags$div(
              style = "padding: 15px; color: #e4e8f0;",
              tags$h4("Welcome to the Mumbai Bus Tracking System"),
              tags$p("This application provides real-time tracking of buses in Mumbai. The system shows you the current position of buses on their designated routes."),
              tags$h4("How to Use"),
              tags$ul(
                tags$li("Select a route from the dropdown menu in the sidebar."),
                tags$li("Choose a specific bus operating on that route."),
                tags$li("Click 'Start' to begin tracking the bus movement."),
                tags$li("Click 'Stop' to pause the tracking."),
                tags$li("The bus position updates every 2 seconds automatically.")
              ),
              tags$h4("Map Legend"),
              tags$ul(
                tags$li(tags$span(style = "color: red;", "Red marker:"), " Current bus position"),
                tags$li(tags$span(style = "color: blue;", "Blue line:"), " Bus route"),
                tags$li(tags$span(style = "color: green;", "Green markers:"), " Terminal stops"),
                tags$li(tags$span(style = "color: #FFD700;", "Yellow markers:"), " Major stops"),
                tags$li(tags$span(style = "color: #4285F4;", "Blue markers:"), " Regular stops")
              )
            )
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Route Information",
            status = "info",
            solidHeader = TRUE,
            DT::dataTableOutput("route_data_table")
          )
        )
      )
    )
  )
)