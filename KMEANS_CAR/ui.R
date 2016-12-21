library(shiny)
library(ggplot2)
library(datasets)

pageWithSidebar(
  headerPanel('K-Means Clustering: Motor Trend Data'),
  sidebarPanel(
    selectInput('xcol', 'X Variable', names(mtcars)),
    selectInput('ycol', 'Y Variable', names(mtcars),
                selected=names(mtcars)[[2]]),
    numericInput('clusters', 'Cluster count', 3,
                 min = 1, max = 9)
  ),
  mainPanel(
    plotOutput('plot1', height="700px", width="700px")
  )
)
