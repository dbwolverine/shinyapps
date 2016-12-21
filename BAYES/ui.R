library(shinydashboard)

sidebar <- dashboardSidebar(
  hr(),
  sidebarMenu(id="tabs", 
              menuItem("Plots",  icon = icon("line-chart"), 
                       menuSubItem("data and predictions", tabName = "plot1", icon = icon("angle-right")), selected=TRUE,
                       menuSubItem("parameters distributions", tabName = "plot2", icon = icon("angle-right")),
                       menuSubItem("MCMC", tabName = "plot3", icon = icon("angle-right"))
              ),
              menuItem("Parameters", tabName = "tabparam", icon = icon("table")),
              menuItem("Codes",  icon = icon("file-text-o"),
                       menuSubItem("ui.R", tabName = "ui", icon = icon("angle-right")),
                       menuSubItem("server.R", tabName = "server", icon = icon("angle-right"))
              ),
              menuItem("ReadMe", tabName = "readme", icon = icon("mortar-board")),
              menuItem("About", tabName = "about", icon = icon("question"))
  ),
  hr(),
  conditionalPanel("input.tabs=='plot1'",
                   sidebarMenu(
                     menuItem("Display", icon = icon("chevron-circle-right"),
                              fluidRow(
                                column(1),
                                column(10, checkboxInput("disp_data", "data", TRUE))
                              ),
                              fluidRow(
                                column(1),
                                column(6, checkboxInput("disp_cond", "LS estimate", TRUE)),
                                column(4, checkboxInput("disp_pint1", "C.I. ", FALSE))
                              ),
                              fluidRow(
                                column(1),
                                column(6, checkboxInput("disp_prior", "Prior ", TRUE)),
                                column(4, checkboxInput("disp_pint0", "P.I ", FALSE))                         
                              ),
                              fluidRow(
                                column(1),
                                column(6, checkboxInput("disp_pmedian", "MAP", TRUE)),
                                column(4, checkboxInput("disp_pint2", "P.I ", FALSE))                         
                              ),
                              fluidRow(
                                column(1),
                                # column(5, checkboxInput("disp_pint", "Intervals ", FALSE)),
                                column(5, numericInput("predint", "", min=1, max=99, value=90))
                              )
                     )
                   )
  ),
  conditionalPanel("input.tabs=='plot2'",
                   sidebarMenu(
                     menuItem("Display", icon = icon("chevron-circle-right"),
                              fluidRow(
                                column(1),
                                column(10, checkboxInput("dens_prior", "prior densities", TRUE))
                              ),
                              fluidRow(
                                column(1),
                                column(10, checkboxInput("dens_post", "posterior densities", TRUE))
                              )
                     )
                   )
  ),
  conditionalPanel("input.tabs=='plot1' | input.tabs=='plot2' | input.tabs=='plot3'",
                   sidebarMenu(
                     menuItem("Distributions", icon = icon("chevron-circle-right"),
                              fluidRow(
                                column(1),
                                column(10, radioButtons("distribution","",c("prior" = "1","observations" = "2"), selected="2"))
                              )
                     ),
                     menuItem("MCMC", icon = icon("chevron-circle-right"),
                              fluidRow(
                                column(1),
                                column(8, numericInput("niter", "Total iterations", 1000))
                              ),
                              fluidRow(
                                column(1),
                                column(8, numericInput("nburn", "Burning iterations", 200))
                              ),
                              fluidRow(
                                column(1),
                                column(10, actionButton("run", "Run"))
                              )
                     )
                   )
  ),
  hr()
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "plot1",
            fluidRow( 
              box(width = 12, status = "primary",
                  plotOutput("plot1",  height="400px",  
                             click = "plot1_click",
                             brush = brushOpts(id = "plot1_brush")
                  )
              )
            )
    ),
    tabItem(tabName = "plot2",
            fluidRow( 
              box(width = 12, status = "primary",
                  plotOutput("plot2",  height="400px")
              )
            )
    ),         
    tabItem(tabName = "plot3",
            fluidRow( 
              box(width = 12, status = "primary",
                  plotOutput("plot3",  height="400px")
              )
            )
    ),         
    tabItem(tabName = "tabparam",
            box( width = NULL, status = "primary", solidHeader = TRUE, title="Parameters",
                 tableOutput("tablep")
            ),
            box( width = NULL, status = "primary", solidHeader = TRUE, title="Variance - Covariance (Least-square estimate)",
                 tableOutput("tableC1")
            ),
            box( width = NULL, status = "primary", solidHeader = TRUE, title="Variance - Covariance (prior distribution)",
                 tableOutput("tableC0")
            ),
            box( width = NULL, status = "primary", solidHeader = TRUE, title="Variance - Covariance (posterior distribution)",
                 tableOutput("tableC2"),digits=5
            )
    ),
    tabItem(tabName = "ui",
            box( width = NULL, status = "primary", solidHeader = TRUE, title="ui.R",
                 downloadButton('downloadData2', 'Download'),
                 br(),br(),
                 pre(includeText("ui.R"))
            )
    ),
    tabItem(tabName = "server",
            box( width = NULL, status = "primary", solidHeader = TRUE, title="server.R",
                 downloadButton('downloadData3', 'Download'),
                 br(),br(),
                 pre(includeText("server.R"))
            )
    ),
    tabItem(tabName = "readme",
            withMathJax(), 
            includeMarkdown("readMe.Rmd")
    ),
    tabItem(tabName = "about",
            includeMarkdown("about.Rmd")
    )
  ),
  conditionalPanel(condition = "input.distribution=='1' & (input.tabs=='plot1'|input.tabs=='plot2'|input.tabs=='plot3') ",              
                   box(width = 4, status = "primary", solidHeader = FALSE,        
                       sliderInput("ka", "ka*:", value = 1, min = 0, max = 2, step=0.1)
                   ),
                   box(width = 4, status = "primary", solidHeader = FALSE,        
                       sliderInput("V", "V*:", value = 1, min = 0, max = 2, step=0.1)
                   ),
                   box(width = 4, status = "primary", solidHeader = FALSE,        
                       sliderInput("Cl", "Cl*:", value = 0.05, min = 0, max = 0.1, step=0.005)
                   ),
                   box(width = 4, status = "primary", solidHeader = FALSE,        
                       sliderInput("oka", "omega_ka:", value = 0.2, min = 0, max = 1, step=0.05)
                   ),
                   box(width = 4, status = "primary", solidHeader = FALSE,        
                       sliderInput("oV", "omega_V:", value = 0.2, min = 0, max = 1, step=0.05)
                   ),
                   box(width = 4, status = "primary", solidHeader = FALSE,        
                       sliderInput("oCl", "omega_Cl:", value = 0.2, min = 0, max = 1, step=0.05)
                   )
  ),
  conditionalPanel(condition = "input.distribution=='2' & (input.tabs=='plot1'|input.tabs=='plot2'|input.tabs=='plot3') ",              
                   box(width = 4, status = "primary", solidHeader = FALSE,        
                       sliderInput("sigma", "sigma:", value = 5, min = 0.5, max = 20, step=0.5)
                   ),
                   box(width = 4, status = "primary", solidHeader = FALSE,        
                       selectInput("N", label = h5("Number of data points"),choices = c(5,10,25,50,100),selected="25")
                   ),
                   conditionalPanel(condition = "input.tabs=='plot1'",              
                                    box(width = 4, status = "primary", solidHeader = FALSE,        
                                        checkboxInput("disp_exclude", "display unused data", FALSE),
                                        radioButtons("add","",c("add data" = "1","exclude data" = "2")),
                                        actionButton("exclude_reset", "Reset")
                                        #                        actionButton("run", "Run")
                                    ))
                   #                    box(width = 4, status = "primary", solidHeader = FALSE,        
                   #                    actionButton("exclude_reset", "Reset")
                   #                    )
  )
  
)

dashboardPage(
  dashboardHeader(title = "Bayesian Model Fitting"),
  sidebar,
  body
)
