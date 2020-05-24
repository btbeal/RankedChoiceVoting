sim_function <- function(voter_number = voters(), distributions = dist()){
  # --------------------------- #
  # ---- Input Parameters
  # --------------------------- #
  
  n_voters     <- voter_number                 # number of voters
  choice_names <- c("A", "B", "C", "D", "E")   # choice names
  choice_total <- length(choice_names)
  
  
  # ------------------------------------ #
  # ---- Voter Preference/Choice Frames
  # ------------------------------------ #
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
  preference.mat["E", ] <- c(0.15, 0.20, 0.20, 0.45, 0.00)
  
  candidate_df <- matrix(NA,                      # Creating null matrix of rank x voters; a voter first choice in column 1 and so on
                         nrow = n_voters,
                         ncol = choice_total,
                         dimnames = list(paste("Voter", 1:n_voters), 1:choice_total))
  
  
  
  
  # -- Assign first vote by probability within a population (these could be felxible)
  candidate_df[,1] <- sample(choice_names, n_voters, replace = TRUE, prob = distributions) # populating with initial distribution of favorites (column 1 - voter' first choice)
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
  pcheck_list <- list()
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
    pcheck_list[[cycle]] <- poll_check
    
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
  
  return(list(df_full, pcheck_list))
  
}



plot_function <- function(data = plot_data(), vote = voters()){
  
  a <- list(  # creating annotation
    x = 4,
    y = (vote/2 + 1),
    opacity = 0.7,
    text = "Majority Line",
    xref = "x",
    yref = "y",
    showarrow = TRUE,
    arrowhead = 4,
    ax = 20,
    ay = -40
  )
  
  p <- data %>% 
    group_by(cycle, x) %>% 
    mutate(y_var = 1:n()) %>% 
    plot_ly(x = ~x,
            y = ~y_var,
            frame = ~cycle,
            color = ~initial,
            type = "scatter",
            mode = "markers",
            height = "650")  %>% 
    layout(annotations = a,
           shapes=list(type='line', 
                       x0="A", x1="E", 
                       y0= (vote/2 + 1), y1= (vote/2 + 1), 
                       line=list(dash='dot', width=1, color = "red")),
           xaxis = list(title = "Candidate",
                        type =  "category",
                        categoryorder = "array",
                        categoryarray = c("A", "B", "C", "D", "E")),
           yaxis = list(title = "Total Votes",
                        range = c(0, 0.75*vote)),
           paper_bgcolor='transparent',
           plot_bgcolor = 'transparent') %>% 
    config(displayModeBar = FALSE, displaylogo = FALSE) %>% 
    animation_opts(2000, easing = "linear", redraw = FALSE)
  
  return(p)
}
