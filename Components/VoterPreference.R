
voterpreferenceUI <- function(id){
  ns <- NS(id)
  choice_names <- c("A", "B", "C", "D", "E")  # defining names to loop through
  num_choices <- length(choice_names)
  list(
      wellPanel("People that voted for candidate A initially would choose these candidates otherwise",
                lapply(seq_len(num_choices-1), function(i){
                  name_update <- c("B", "C", "D", "E")
                  prob_vec    <- c(0.40, 0.50, 0.05, 0.05)
                  sliderInput(inputId = ns(paste("pA2", name_update[i])),
                              label = paste("For candidate",name_update[i],"..."),
                              min = 0,
                              max = 1,
                              value = prob_vec[i])})),
      wellPanel("People that voted for candidate B initially would choose these candidates otherwise",
                lapply(seq_len(num_choices-1), function(i){
                  name_update <- c("A", "C", "D", "E")
                  prob_vec    <- c(0.30, 0.59, 0.10, 0.01)
                  sliderInput(inputId = ns(paste("pB2", name_update[i])),
                              label = paste("For candidate",name_update[i],"..."),
                              min = 0,
                              max = 1,
                              value = prob_vec[i])})),
      wellPanel("People that voted for candidate C initially would choose these candidates otherwise",
                lapply(seq_len(num_choices-1), function(i){
                  name_update <- c("A", "B", "D", "E")
                  prob_vec    <- c(0.38, 0.42, 0.10, 0.10)
                  sliderInput(inputId = ns(paste("pC2", name_update[i])),
                              label = paste("For candidate",name_update[i],"..."),
                              min = 0,
                              max = 1,
                              value = prob_vec[i]) })),
      wellPanel("People that voted for candidate D initially would choose these candidates otherwise",
                lapply(seq_len(num_choices-1), function(i){
                  name_update <- c("A", "B", "C", "E")
                  prob_vec    <- c(0.08, 0.02, 0.05, 0.85)
                  sliderInput(inputId = ns(paste("pD2", name_update[i])),
                              label = paste("For candidate",name_update[i],"..."),
                              min = 0,
                              max = 1,
                              value = prob_vec[i])})),
      wellPanel("People that voted for candidate E initially would choose these candidates otherwise",
                lapply(seq_len(num_choices-1), function(i){
                  name_update <- c("A", "B", "C", "D")
                  prob_vec    <- c(0.08, 0.02, 0.05, 0.55)
                  sliderInput(inputId = ns(paste("pE2", name_update[i])),
                              label = paste("For candidate",name_update[i],"..."),
                              min = 0,
                              max = 1,
                              value = prob_vec[i])}))
      
      )
}