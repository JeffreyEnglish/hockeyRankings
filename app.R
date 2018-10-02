library(shiny)
library(googlesheets)

outputSheet = gs_url("https://docs.google.com/spreadsheets/d/1kUcy4Y9JETKV-9s5xIhjdtLja1BlI6vwa3Tjtz3jy4w/")

#List of all 31 teams
allTeams = c("Carolina Hurricanes","Columbus Blue Jackets","New Jersey Devils","New York Islanders","New York Rangers","Philadelphia Flyers","Pittsburgh Penguins","Washington Capitals",
             "Boston Bruins","Buffalo Sabres","Detroit Red Wings","Florida Panthers","Montreal Canadiens","Ottawa Senators","Toronto Maple Leafs","Tampa Bay Lightning",
             "Chicago Blackhawks","Colorado Avalanche","Dallas Stars","Minnesota Wild","Nashville Predators","St. Louis Blues","Winnipeg Jets",
             "Anaheim Ducks","Arizona Coyotes","Calgary Flames","Edmonton Oilers","Los Angeles Kings","San Jose Sharks","Vancouver Canucks","Vegas Golden Knights")

#Server side functions
server <- function(input, output, session) {
  
  #Initiate reactiveValues object to store results
  appendRow = reactiveValues()

  #Actions to perform when the action button is clicked
  observeEvent(input$submit,{
    
    #Read the answers from the form
    appendRow$bestTeam = input$bestTeam
    appendRow$worstTeam = input$worstTeam
    
    #Generate four new team options
    newChoices = allTeams[sample(1:31,4)]
    
    #Update the choices in the radio buttons to the new selection
    updateRadioButtons(session, "bestTeam",
                       choices = newChoices)
    updateRadioButtons(session, "worstTeam",
                       choices = newChoices)
    
    #If this is the first click, change the action button to say "Submit"
    if (input$submit == 1){updateActionButton(session,"submit","Submit")}
    
    #If this isn't the first click, record the four team options, best team, and worst team
    if (input$submit > 1){gs_add_row(outputSheet, input = unlist(reactiveValuesToList(appendRow))[c(2:5,6,1)])}
    
    #Update the appendRow with the choices that the user sees now
    appendRow$newChoices = newChoices
    })
  
}

ui <- fluidPage(
  
  #Import formatting from a seperate CSS file
  includeCSS("formatting.css"),
  
  #Show a title panel
  titlePanel(h2("NHL Team Rankings")),
  
  #Radio button panel that only shows up after the user clicks to begin
  conditionalPanel("input.submit > 0", 
                   
                   #Center everything below on the screen
                   div(style="text-align:center",
                       
                       #Add a horizontal divider
                       hr(width="40%"),
                       
                       #Add a box of descriptive text, 600 pixels wide
                       div(style="display:inline-block;width:600px;text-align:left","Below is a selection of four NHL teams. Select the team you think is the best on the right and the team you think is worst on the left, then click Submit. Your response will be recorded and another selection of teams will appear. Thank you for your input!"),
                       
                       #Another horizontal divider
                       hr(width="40%"),
                       
                       #Divide a centered 600px box in two
                       div(style="display:inline-block;width:600px", splitLayout(cellWidths = c("50%","50%"),
                                                                                 
                               #Add radio buttons to select the best and worst team
                               #Choices will not be added until after the user clicks to begin
                               #Choices are updated on the server side and sent to the UI
                               radioButtons("bestTeam", "Select the best team:", choices = NULL, selected=character(0)),
                               radioButtons("worstTeam", "Select the worst team:", choices = NULL, selected=character(0))
                   )))
                   
  ),
  
  #Add a centered submit button
  div(style="display:inline-block;width:100%;text-align: center;",actionButton("submit","Click to begin"))

)

#Run everything
shinyApp(ui = ui, server = server)