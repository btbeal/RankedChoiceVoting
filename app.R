source("Components/VoterPreference.R")
source("Components/InitialDistribution.R")

library(shiny)
library(shinyWidgets)
library(markdown)
library(shinyBS)
library(plotly)
library(tidyverse)
library(ggthemes)
library(scales)

# Define UI for application that draws a histogram
ui <- fluidPage(
  navbarPage("RCV", position = "fixed-top", collapsible = TRUE,
    tabPanel("Home",
             tags$style(type="text/css", "body {padding-top: 70px;}"),
             includeHTML("www/HomePage/rcv_homePage.html")),
    tabPanel("Simulation",
             titlePanel("The Ranked Choice Voting Simulation"),
             hr(),
             initialDistributionUI("init_dist"))

  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session){

  callModule(initialDistribution, "init_dist")
  
}

# Run the application 
shinyApp(ui = ui, server = server)

