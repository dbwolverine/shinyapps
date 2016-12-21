# Load the ggplot2 package which provides
# the 'mpg' dataset.
library(ggplot2)
server <- function(input, output) {
  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable({
    data <- mpg
    if (input$man != "All") {
      data <- data[data$manufacturer == input$man,]
    }
    if (input$cyl != "All") {
      data <- data[data$cyl == input$cyl,]
    }
    if (input$trans != "All") {
      data <- data[data$trans == input$trans,]
    }
    data
  }))
}
ui <- fluidPage(
  titlePanel("Basic Car Data Tabulation"),
  # Create a new Row in the UI for selectInputs
  fluidRow(
    column(4,
        selectInput("man",
                    "Manufacturer:",
                    c("All",
                      unique(as.character(mpg$manufacturer))))
    ),
    column(4,
        selectInput("trans",
                    "Transmission:",
                    c("All",
                      unique(as.character(mpg$trans))))
    ),
    column(4,
        selectInput("cyl",
                    "Cylinders:",
                    c("All",
                      unique(as.character(mpg$cyl))))
    )
  ),
  # Create a new row for the table.
  fluidRow(
    DT::dataTableOutput("table")
  )
)

shinyApp(server = server, ui = ui)