source("Components/InitialDistribution.R")
source("aux.R")

library(shiny)
library(V8)
library(shinyWidgets)
library(shinythemes)
library(shinyjs)
library(markdown)
library(shinyBS)
library(plotly)
library(tidyverse)
library(ggthemes)
library(scales)
library(htmltools)


# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("simplex"),
  useShinyjs(),
  extendShinyjs(text = toBottom),
  tags$style(type="text/css", "body {padding-top: 70px;}"),
  navbarPage("IRV", position = "fixed-top", collapsible = TRUE,
    tabPanel("Home",
             tags$iframe(src = "HomePage/rcv_homePage.html",
                         frameBorder = "0",
                         seamless = TRUE,
                         width = "1250px", height = "1000px")), 
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

