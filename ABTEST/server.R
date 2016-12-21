library('shiny')
library('ggplot2')
source("functions.R")


makeTypicalTable = function(planTable, sizes, dummy) {
  dummy 
  ptab = typicalTable(planTable, sizes)
  if(sum(sizes) > 0) {
    ptab$expectedSuccessRate = with(ptab, (Successes+0.5)/(Actions+1)) 
    ptab$expectedValuePerAction = ptab$expectedSuccessRate*planTable$ValueSuccess
  }
  ptab
}

labeledPlan = function(sizes,rates,values,countGoal) {
  data.frame(Label=paste("Campaign_", seq_len(length(sizes)), sep=''),
             ActionsToMeetErrorGoals=sizes,
             ActionsToMeetCountGoals=ceiling(countGoal/rates),
             MatchingRates=c(rates[2]*values[2]/values[1],rates[1]*values[1]/values[2]))
}

displayGraph = function(pgraph, doplot) {
  if(doplot) {
    plotSample(pgraph)
  } else NULL
}

# ---------------------------------

assembleResultTable = function(actions, successes, values, wishPrice) {
  ptab = data.frame(Label=paste("Campaign_", seq_len(length(successes)), sep=''),
                    Actions=actions,
                    Successes=successes,
                    ValueSuccess=values)
  ptab$observedSuccessRate = (successes+0.5)/(actions+1) 
  ptab$observedValuePerAction = ptab$observedSuccessRate*values
  ptab$pAboveWishPrice = pbeta(wishPrice/values,
                               shape1=0.5+successes,
                               shape2=0.5+actions-successes,
                               lower.tail=FALSE)
  ptab
}

shinyServer(function(input, output) {
  #
  cprobabilities = reactive(c(input$conv1a, input$conv2a))
  values = reactive(c(input$value1a, input$value2a))
  sizes = reactive(c(input$sizes1a, input$sizes2a))
  proposedsizes = reactive(heuristicPowerPlan(data.frame(Probability=cprobabilities(), 
                                                         ValueSuccess=values()), 
                                              errorProbability=input$errorProb,relativeError=input$relErr)) 
  countGoalV <- reactive(input$countGoal)
  
  docalc = reactive(sum(sizes()) != 0)
  
  planTable = reactive(data.frame(Label=c('Campaign1','Campaign2'),
                                  Probability=cprobabilities(), 
                                  ValueSuccess=values()))
  typicalTable = reactive(makeTypicalTable(planTable(), sizes(), input$reseed))
  pgraph2T = reactive(posteriorGraph(typicalTable()))
  output$planGraph2T = renderPlot(plotPosterior(pgraph2T()))
  output$probTable2T = renderPrint(computeProbsGEP(typicalTable(),pgraph2T()$graph))
  
  pgraph = reactive(sampleGraph(planTable(),sizes()))
  bgraph = reactive(computeProbsGES(planTable(),pgraph()))
  
  output$plan = renderTable(labeledPlan(proposedsizes(),cprobabilities(),values(),countGoalV()),digits=4)
  output$typicalTable = renderPrint(typicalTable()) 
  output$planGraph = renderPlot(displayGraph(pgraph(), docalc()))
  output$probTable = renderPrint(bgraph())
  
  
  actions = reactive(c(input$actions1b, input$actions2b))
  successes = reactive(c(input$success1b, input$success2b))
  svalues = reactive(c(input$value1b, input$value2b))
  
  resTable = reactive(assembleResultTable(round(input$rescale*actions()), 
                                          round(input$rescale*successes()), 
                                          svalues(),
                                          input$wishPrice))
  pgraph2 = reactive(posteriorGraph(resTable()))
  
  output$resTable = renderPrint(resTable())
  output$planGraph2 = renderPlot(plotPosterior(pgraph2(),input$wishPrice))
  output$probTable2 = renderPrint(computeProbsGEP(resTable(),pgraph2()$graph))
  
})