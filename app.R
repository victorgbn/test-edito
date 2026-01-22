# Script de test minimal pour debug Shiny
cat("🚀 MON app.R EST BIEN LANCÉ\n")

library(shiny)
library(terra)
library(httr)

ui <- fluidPage(
  titlePanel("Test App"),
  mainPanel("Hello world")
)

server <- function(input, output, session) {}

shinyApp(ui, server)
