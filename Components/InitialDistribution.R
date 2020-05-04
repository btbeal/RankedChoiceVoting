source("Components/Sim_functions/simulationFunction.R")
initialDistributionUI <- function(id){
  ns <- NS(id)
  choices <- c("A", "B", "C", "D", "E")
  prop    <- c(0.18, 0.22, 0.14, 0.30, 0.16)
  

  fluidPage(
    chooseSliderSkin("Round"),
    setSliderColor(rep("#7412bd", 6), c(1,2,3,4,5,6)),
    column(4, 
           wellPanel("First, choose the number of voters: ",
                     sliderInput(inputId = ns("n_voter"),
                                 label = NULL,
                                 max = 1000,
                                 min = 100, 
                                 value = 120)),
           wellPanel(
             chooseSliderSkin("Round"),
             setSliderColor(rep("#7412bd", length(choices)), c(1, 2, 3, 4, 5)),
             lapply(seq_len(length(choices)), function(i){
               sliderInput(inputId = ns(paste0(choices[i],"_dist")),
                           label = paste0("Proportion of votes for candidate ", choices[i]),
                           min = 0,
                           max = 1,
                           value = prop[i])})),
           wellPanel("Make sure your total sums to 1 (a proportion of 1.00)",
                     textOutput(ns("dist_sum"))),
      actionBttn(inputId = ns("dist_action"),
                 label = "Press to update simulation",
                 color = "royal",
                 style = "pill",
                 size = "sm")
    ),
    column(8,
           plotlyOutput(outputId = ns("plot"))),
    bsPopover(id = ns("n_voter"),                 # insert ID into standard popover
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
  
  output$dist_sum <- renderText({
    
    paste("Your sum is: ", sum_distribution())
    
  })
  
  plot_data <- eventReactive(input$dist_action, {
   df <- sim_function(voters(), dist())
   return(df)
  })
  
  output$plot <- renderPlotly({
  
      p <- plot_data() %>% 
          group_by(initial, cycle) %>% 
          count(x) %>% 
          mutate(n = n/voters()) %>% 
           plot_ly(x = ~x,
                   y = ~n,
                   color = ~initial,
                   frame = ~cycle) %>% 
      add_bars() %>% 
      layout(barmode = "stack",
             yaxis = list(range = c(0, 1)),
             xaxis = list(type =  "category",
                          categoryorder = "array",
                          categoryarray = c("A", "B", "C", "D", "E"))) %>% 
    animation_opts(
      2000, easing = "bounce", redraw = TRUE
    )
      
      print(p)
    
    })
  
  
  
  
  
}

