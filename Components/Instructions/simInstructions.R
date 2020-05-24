simInstructions <- bootstrapPage(
  div(h3("First, some instructions..."),
      hr(),
      p(HTML("This tool simulates polling results of ranked choice voting. Keeping with the naming conventions 
        in the post, we have candidates <span style='color:#00CC96'><b>A</b></span>, <span style='color:#FFA15A'><b>B</b></span>, <span style='color:#636EFA'><b>C</b></span>, <span style='color:#f28dcd'><b>D</b></span>,
              <span style='color:#B6E880'><b>E</b></span>. Each voter has built in preferences (more on that in the 'Voter Preferences' section below).")),
      p(HTML("Once simulated, you can see every voters <i>initial preference</i> - notice that the markers will remain color coded by a candidates initial vote so that 
        one can see how the votes get redistributed.")),
      p("Because this is ranked choice, if a candidate's first choice is polling the lowest, their vote gets redistributed to their next choice. This process
        happens until a candidate has a majority of the votes (> 50%)."),
      p(HTML("<span style='color:indianred'><b>Notice that the candidate with initial plurality is not always the winner.</b></span>")),
      br(),
      div(shinyBS::bsCollapse(id = "collapse",
                              shinyBS::bsCollapsePanel(title = "Voter Preferences (FYI)", div(
                                 p("The voters have built in preferences, which means that if the voter initially chooses 'A', then they 
                                   are more likely to vote for some candidates over others as their second, third, or fourth options. This is meant
                                   to simulate the way people make decisions in real life (i.e., if one can't have Chocolate, maybe that group that prefers chooses vanilla > mint 90% of the time)."),
                                 p("The preferences are built in as probabilities. In other words, if a person chooses a particular candidate,
                                   how likely are the to vote for the others? Here they are, as follows:"),
                                 p(HTML("<span style='color:#00CC96'><b>A</b></span> chooses C (50%) > B (40%) > D/E (5%)")),
                                 p(HTML("<span style='color:#FFA15A'><b>B</b></span> chooses C (59%) > A (30%) > D (10%) > E (1%)")),
                                 p(HTML("<span style='color:#636EFA'><b>C</b></span> chooses B (42%) > A (38%) > D (10%) > E (10%)")),
                                 p(HTML("<span style='color:#f28dcd'><b>D</b></span> chooses E (85%) > A (8%) > C (5%) > B (2%)")),
                                 p(HTML("<span style='color:#B6E880'><b>E</b></span> chooses D (45%) > B/C (20%) > A (15%)")),
                                 p("Hopefully this makes sense, but if you really want to dive in, checkout the github below")
                                 )))),
      br(),
  p("Press the simulation button below to get started, or adjust the 'tuning parameters' as you'd like. These are random draws, so each time is different!"),
  hr(),
  div(style = "text-align:center",
  tagList(a(icon("github", "fa-2x"), href = "https://github.com/btbeal/RankedChoiceVoting", 
            target = "_blank"),
          a(icon("linkedin", "fa-2x"), href = "https://www.linkedin.com/in/btbeal/", target = "_blank"),
          a(icon("twitter", "fa-2x"), href = "https://twitter.com/BealBrennan", target = "_blank")))))
                                       