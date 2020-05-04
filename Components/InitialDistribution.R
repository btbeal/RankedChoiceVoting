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
    # --------------------------- #
    # ---- Input Parameters
    # --------------------------- #
    
    n_voters     <- voters()                         # number of voters
    choice_names <- c("A", "B", "C", "D", "E")   # choice names
    choice_total <- length(choice_names)
    
    
    # ------------------------------------ #
    # ---- Voter Preference/Choice Frames
    # ------------------------------------ #
    # ------------------------------------------------------ party 1
    # create voter preference matrix
    preference.mat <- matrix(0,
                             nrow = choice_total,
                             ncol = choice_total,
                             dimnames = list(choice_names, choice_names))
    # filling in matrix
    #   A     B     C      D    E
    preference.mat["A", ] <- c(0.00, 0.40, 0.50, 0.05, 0.05) # filling in preferences (of course, I could have just put this in initial matrix... but if this will be more flexible in the future, should probably remain like this)
    preference.mat["B", ] <- c(0.30, 0.00, 0.59, 0.10, 0.01) # could make them all flexible... but that would be a lot
    preference.mat["C", ] <- c(0.38, 0.42, 0.00, 0.10, 0.10)
    preference.mat["D", ] <- c(0.08, 0.02, 0.05, 0.00, 0.85)
    preference.mat["E", ] <- c(0.08, 0.02, 0.05, 0.55, 0.00)
    
    candidate_df <- matrix(NA,                      # Creating null matrix of rank x voters; a voter first choice in column 1 and so on
                           nrow = n_voters,
                           ncol = choice_total,
                           dimnames = list(paste("Voter", 1:n_voters), 1:choice_total))
    
    
    
    
    # -- Assign first vote by probability within a population (these could be felxible)
    candidate_df[,1] <- sample(choice_names, n_voters, replace = TRUE, prob = dist()) # populating with initial distribution of favorites (column 1 - voter' first choice)
    new_df <- candidate_df
    v <- 1
    for(v in 1:n_voters){
      first_choice <- new_df[v,1]                                                     # exclude this candidate from further options for this voter
      count <- 1                              # Beginning the count (to go up to = total choices)
      exclusions <- c(first_choice)           # Beginning the vector of exclusions to pick from in loop
      loop.preferences <- preference.mat      # reassigning preference.mat to loop.preferences each time (since I mutilate it in the while loop). Could've done this a different way (assign probability as a vector from the beginning)
      while(count < choice_total){
        probability <-  loop.preferences[first_choice, ][which(loop.preferences[first_choice, ] != 0)]    # extracting non-zero probabilities from preferences matrix (pref matrix updated each loop)
        exclusion_index <- choice_names %in% exclusions                                                   # Which indeces contain choices to exclude (T/F vec)
        new_df[v,(count+1)] <- sample(choice_names[!exclusion_index], 1, prob = probability)              # filling in second row of candidate df with second choice
        
        # update while loop parameters
        loop.preferences[first_choice, new_df[v,(count+1)]] <- 0 # making the probability of picking this choice on the next run 0
        exclusions <- c(exclusions, new_df[v,(count+1)])       # putting the next choice in the exclusions vector
        count <- count + 1                                     # update count
      }
    }
    
    # ------------------------------------ ## ------------------------------------ #
    # ---- Run the simulation from the given data
    # ------------------------------------ ## ------------------------------------ #
    
    # store df into new one (so I don't have to run above code not really necessary)
    df <- new_df
    df_full <- data.frame(x = NULL, initial = NULL, cycle = NULL)
    init_choices <- df[,1]    # to bind to df for visualization
    poll_check <- 0  # begin the simulation by creating a "poll check," which will see if any candidate has >= 50% polling
    cycle <- 1
    while(all(poll_check < 0.5)){            # loop until someone has majority vote
      
      i <- 1
      vec <- c()
      for(i in 1:nrow(df)){
        vote <- rank(which(!is.na(df[i,])))  # this represents the voters next best choice if their previous candidate(s) is/are eliminated
        index_vote <- names(vote[vote == 1]) # index that choice to know which row to include in vector of all voters' favorite viable candidate
        choice <- df[i, index_vote]          # index the choice from above from the given voter
        vec <- c(vec, choice)                # store that choice in a vector to then give a polling update
      }
      
      poll_check <- prop.table(table(vec))  # checking proportion of votes for each candidate
      print(poll_check)                     # print to console
      
      # ---- Create ggplot of polling results and update each round
      dat <- data.frame(x = vec, initial = init_choices, cycle = cycle)           # create df from polling results
      cycle <- cycle + 1
      dat$x <- factor(dat$x, levels = choice_names)
      
      index <- rank(poll_check)             # see who is polling the lowest
      elim <- names(sort(index)[1])         # index candidate (Note that if there are ties it will simply take the first of those... not great but good)
      df[df == elim] <- NA                  # eliminate that candidate option from dataset
      
      if(any(poll_check >= 0.5)){           # if any is at or above 50%, stop the polls and declare a winner
        print(paste("Your winner is", names(poll_check[poll_check >= 0.5])))
      }
      df_full <- rbind(dat, df_full)
    }
    
    return(df_full)
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

