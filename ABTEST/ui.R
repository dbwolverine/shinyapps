library('shiny')
library('ggplot2')
source("functions.R")

shinyUI(fluidPage(
  
  # Application title
  titlePanel("A/B Test Design"),
  
  
  # Input
  verticalLayout(
    inputPanel(
      helpText("Begin new A/B test or examine previous results"),
      column(12,
             selectInput("panelchooser", "Choose Task",
                         c("Start" = "plan",
                           "Evaluate" = "evaluate"),
                         selected="plan"))),
    
    conditionalPanel(condition="input.panelchooser=='plan'",
                     helpText("This application allows a data scientist to enter some given inputs about an A/B testing campaign in order to estimate the absolute error. This absolute error is used to determine the cohort sample sizes that should lead to a reasonable approximation of the relative error. (Tip: enter two different rate values so you can tell them apart in the results)"),
                     helpText("Each row represents an A/B Test campaign. To use this tool properly, you must enter a reasonable lower bound on the success rate (most likely derived from prior knowledge) and the amount per success that is assumed for the test."),
                     helpText("These controls fill in the Suggested Campaign Actions/Sizes interface and the Possible Values interface."),
                     fluidRow(
                       column(6,
                              numericInput("conv1a", "Success Rate:", 0.5,
                                           min = 0, max = 1)),
                       column(6,
                              numericInput("value1a", "Success Value:", 1,
                                           min=0, max=10000))
                     ), # end fluidRow
                     fluidRow(
                       column(6,
                              numericInput("conv2a", "Success Rate:", 0.5,
                                           min = 0, max = 1)),
                       column(6,
                              numericInput("value2a", "Success Value:", 1,
                                           min=0, max=10000))
                     ), # end fluidRow
                     hr(),
                     helpText("Below, you must enter the [Error] probability of misestimating each campaign--no more than a relative multiple (i.e. relative error) of the most valuable campaign and a minimum number of observed successes goal. These values will determine the absolute error rate (in $ per action) for the problem and help fill out the Suggested Test Cohort Sizes table."),
                     fluidRow(
                       column(4,
                              numericInput("errorProb", "Error Probability:", 0.05,
                                           min = 0, max = 1)),
                       column(4,
                              numericInput("relErr", "Relative Error:", 0.2,
                                           min=0, max=1)),
                       column(4,
                              numericInput("countGoal", "Count Goal", 5,
                                           min=0, max=1000000))
                     ) # end fluidRow
                     
                     ), # end conditionalPanel
    
    conditionalPanel(condition="input.panelchooser=='evaluate'",
                     helpText("The evaluation tab takes results from a previously run A/B test campaign and shows the likely possibilities for the unknown true values of the traffic sources."),
                     helpText("For each of two already run campaigns, enter the campaign results: size (# of samples in cohorts), number of successes and the value weight of each success."),
                     fluidRow(
                       column(4,
                              numericInput("actions1b", "Actions   ", 100,
                                           min=0, max=100000)),
                       column(4,
                              numericInput("success1b", "Successes", 1,
                                           min = 0, max = 100000)),
                       column(4,
                              numericInput("value1b", "Success Value:", 1,
                                           min=0, max=10000))
                     ), #end fluidRow
                     fluidRow(
                       column(4,
                              numericInput("actions2b", "Actions   ", 100,
                                           min=0, max=100000)),
                       column(4,
                              numericInput("success2b", "Successes", 1,
                                           min = 0, max = 100000)),
                       column(4,
                              numericInput("value2b", "Success Value:", 1,
                                           min=0, max=10000))
                     ), #end fluidRow
                     helpText("The wish price adds a price annotation to the graph and when Sale Factor > 1, allows you to see what a larger campaign with similar result rates should look like."),
                     fluidRow(
                       column(6,
                              numericInput("wishPrice", "Wish Price", 0.05,
                                           min=0, max=100000)),
                       column(6,
                              numericInput("rescale", "Scale Factor", 1,
                                           min=0, max=100000))) #end fluidRow
    ), # end conditionalPanel
    
    hr(),
    
    # Output
    mainPanel(
      # plan campaign: has both input and output
      conditionalPanel(condition="input.panelchooser=='plan'",
                       h4("Suggested Campaign Actions/Sizes"),
                       tableOutput("plan"),
                       helpText("Enter the proposed cohort sizes (look above to the Suggested Test Cohort Sizes for values to try).  This will automatically fill out the Possible Values interface"),
                       h4("Enter Campaign Actions/Sizes"),
                       # giving up and hard-coding the campaign names
                       numericInput("sizes1a", "Size of first test cohort", 100, min=0, max=100000),
                       numericInput("sizes2a", "Size of secont test cohort", 100, min=0, max=100000),
                       h4("Possible Values"), 
                       helpText("This section shows the distribution with probabilities of observed success frequencies/values for a specified (unobserved) true campaign rates. These are the plots of the likely distribution of what will bee seen during estimation if the two campaigns had rates that you specified earlier.  (Hint: we need to know how often the more valuable campaign appears to actually be more valuable during measurement."),
                       plotOutput("planGraph"),
                       h4("Posterior Probabilities"),
                       verbatimTextOutput("probTable"),
                       h4("Typical Outcome"), 
                       helpText("This section simulates the evaluation tab for the above campaigns. It shows for a given emprical obseration (drawn at random) from the specified campaigns what distribution would be estimated for the true values of the campaigns. In practice, the true values of the campaigns are unknown quantities that we are attempting to estimate in the Evaluation Tab.  In this simulation interface, check if the values observed are near the simulation values specified often enough."),
                       actionButton("reseed", "Regenerate"),
                       verbatimTextOutput("typicalTable"),
                       plotOutput("planGraph2T"),
                       verbatimTextOutput("probTable2T")
                       
                       
      ), # end conditionalPanel
      
      conditionalPanel(condition="input.panelchooser=='evaluate'",
                       helpText("Here we show the distribution of the true values of the campaigns that we can infer from the results entered."),
                       verbatimTextOutput("resTable"),
                       plotOutput("planGraph2"),
                       h4("Posterior Probabilities"),
                       verbatimTextOutput("probTable2")
      ) # end conditionalPanel
    ) # end mainPanel
  )) # end sidebarLayout, fluidPage
) # end shinyUI