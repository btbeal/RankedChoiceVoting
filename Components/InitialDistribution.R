source("Components/Sim_functions/simulationFunction.R")
source("Components/Instructions/simInstructions.R")


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
                                circle = FALSE,
                                tooltip = TRUE,
                                up = TRUE,
                                icon = icon("toolbox"),
                   wellPanel(strong("Choose the number of voters: "),
                             br(),
                             sliderInput(inputId = ns("n_voter"),
                                         label = NULL,
                                         max = 500,
                                         min = 100, 
                                         value = 100)), 
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
    column(6, offset = 1,
             uiOutput(outputId = ns("results_text"))),
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

  
  output$results_text <- renderUI({
    check <- s_results()[[2]] # return the list of polling vectors
    html_select <- c(A = "#00CC96",
                     B = "#FFA15A",
                     C = "#636EFA",
                     D = "#F28DCD",
                     E = "#B6E880")
    
    
    # initial plurality
    initial_plurality <- check[[1]][rank(check[[1]]) == 5] # value
    plural_name <- names(initial_plurality)  # candidate
    plurality_html <- unname(html_select[names(html_select) %in% plural_name])
    
    # Ultimate winner
    final_poll <- length(check)
    winning_candidate <- names(which.max(rank(check[[final_poll]]))) # extract the name of the candidate polling the highest on the final cycle
    winner_html <- unname(html_select[names(html_select) %in% winning_candidate])
    if(winning_candidate == plural_name){
      div(style = "text-align:center",
          p(HTML(paste("In a plurality voting system, candidate <span style='color:",plurality_html,"'><b>", plural_name,"</b></span>would wins this election with <span style='color:",plurality_html,"'><b>", round(unname(initial_plurality)*100,2), 
                       "%</b></span> of the voting populations support."))),
          p(HTML(paste("Of course, in this simulation, <span style='color:",winner_html,"'><b>", winning_candidate, "</b></span> still wins. Though, everyone's voice has been heard and the process now seems much more fair."))))
    } else {
      div(style = "text-align:center",
          p(HTML(paste("In a plurality voting system, candidate <span style='color:",plurality_html,"'><b>", plural_name,"</b></span>would win this election with <span style='color:",plurality_html,"'><b>", round(unname(initial_plurality)*100,2), 
                       "%</b></span> of the voting populations support."))),
          p(HTML(paste("However, using RCV (better known as instant runoff voting), the winner is <span style='color:",winner_html,"'><b>", winning_candidate, "</b></span>"))))
    }
    
    
  })
  
  
  
  
}

