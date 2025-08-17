library(shiny)

source("frontend.R")
source("backend.R")

#Merge frontend and backend (ui and server environment variables into one single Shiny app)
shinyApp(ui = ui, server = server) 