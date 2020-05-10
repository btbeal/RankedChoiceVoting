homePageUI <- bootstrapPage(
  div(h3("First, some instructions..."),
      hr(),
      p(HTML("This tool simulates polling results of ranked choice voting. Keeping with the naming conventions 
        in the post, we have candidates <span style='color:#00CC96'><b>A</b></span>, <span style='color:#FFA15A'><b>B</b></span>, <span style='color:#636EFA'><b>C</b></span>, <span style='color:#f28dcd'><b>D</b></span>,
              <span style='color:#B6E880'><b>E</b></span>. Each voter has built in preferences (more on that on my github).")),
      p(HTML("Once simulated, you can see every voters <i>initial preference</i> - notice that the markers will remain color coded by a candidates initial vote so that 
        one can see how the votes get redistributed.")),
      p("Because this is ranked choice, if a candidate's first choice is polling the lowest, their vote gets redistributed to their next choice. This process
        happens until a candidate has a majority of the votes (> 50%)."),
      p(strong("Notice that the candidate with initial plurality is not always the winner.")),
      br(),
      p("Press the simulation button below to simulate, or adjust the parameters as you'd like.")))