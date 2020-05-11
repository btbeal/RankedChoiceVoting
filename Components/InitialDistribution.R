source("Components/Sim_functions/simulationFunction.R")
source("Components/simPrint/simulation_text.R")


initialDistributionUI <- function(id){
  ns <- NS(id)
  choices <- c("A", "B", "C", "D", "E")
  prop    <- c(0.18, 0.22, 0.14, 0.30, 0.16)
  fluidPage(
    chooseSliderSkin("Round"),
    setSliderColor(rep("#7412bd", 6), c(1,2,3,4,5,6)),
    sidebarPanel(simInstructions,
                 hr(),
                 actionBttn(inputId = ns("dist_action"),
                            label = "Press to update simulation",
                            color = "danger",
                            style = "simple",
                            block = TRUE,
                            size = "sm"),
                 br(),
                 br(),
                 dropdownButton(label = "Tuning Parameters",
                                circle = TRUE,
                                tooltip = TRUE,
                                icon = icon("toolbox"),
                   wellPanel(strong("Choose the number of voters: "),
                             br(),
                             sliderInput(inputId = ns("n_voter"),
                                         label = NULL,
                                         max = 500,
                                         min = 100, 
                                         value = 120)), 
                   wellPanel(strong("Choose proportion of inititial votes for each candidate: "),
                             br(),
                             chooseSliderSkin("Simple"),
                             lapply(seq_len(length(choices)), function(i){
                               sliderInput(inputId = ns(paste0(choices[i],"_dist")),
                                           label = paste0(choices[i]),
                                           min = 0,
                                           max = 1,
                                           step = 0.01,
                                           ticks = FALSE,
                                           value = prop[i])}),
                             uiOutput(ns("dist_sum"))))),
    br(),
    br(),
    column(8,
           plotlyOutput(outputId = ns("plot"))),
    bsPopover(id = ns("n_voter"),                 
              title = "Notice:",
              content =  paste("The number of voters impact how closely the simulation will reflect your desired distribution below"), 
              placement = "right", 
              trigger = "hover")
    )


  
}

initialDistribution <- function(input, output, session){
  ns <- session$ns

  
  # using this to throw error before running sim and to relay information to user
  sum_distribution <- reactive(sum(c(input$A_dist, input$B_dist, input$C_dist, input$D_dist, input$E_dist))) 
  dist <- reactive(c(input$A_dist, input$B_dist, input$C_dist, input$D_dist, input$E_dist))
  voters <- reactive(input$n_voter)
  
  output$dist_sum <- renderUI({
    
    if(sum_distribution() == 1){
      wellPanel(strong("Make sure your total sums to 1 (a proportion of 1.00)"),
                style = "background: #b3f5b5",
                paste("Your sum is: ", sum_distribution(), sep = ""))
    } else {
      wellPanel(strong("Make sure your total sums to 1 (a proportion of 1.00)"),
                style = "background: #f7a894",
                paste("Your sum is: ", sum_distribution(), sep = ""))
    }
    
  })
  
  observeEvent(input$dist_action, {
    if(sum_distribution() != 1){
      sendSweetAlert(         # add error message if user needs more information
        session = session,
        title = "Oops!",
        text = "Make sure all of your proportions add to 1!",
        type = "error",
        btn_labels = c("Go back")
      )
    } else {
      delay(500,js$toBottom())
      
      return()
    }
  })
  
  s_results <- eventReactive(input$dist_action, {
   if(sum_distribution() != 1){
     return()
   } else {
     sim_results <- sim_function(voters(), dist())
     return(sim_results)
   }
  })
  
  output$plot <- renderPlotly({
    if(is.null(s_results()[[1]])){
      return()
    } else {
      p <- plot_function(s_results()[[1]], voters())
      print(p)
    }
    })

  
  output$rcv_text <- renderText({
    check <- s_results()[[2]] # return the list of polling vectors
    
    # initial plurality
    initial_plurality <- check[[1]][rank(check[[1]]) == 5] # value
    plural_name <- names(initial_plurality)  # candidate
    
    print(paste("In a plurality voting system, candidate ", plural_name, " would win this election with ", unname(initial_plurality)*100, "% of the voting populaations support"))
    
    
  })
  
  
  
  
}

