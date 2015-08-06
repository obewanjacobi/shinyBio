library(shiny)
library(shinythemes)

max.time<-1000
max.b <- 1
min.d <- 1
max.m <-1000
max.n_0 <- 2000

navbarPage(
  title = "Logistic Growth for a Population",
  theme = shinytheme("spacelab"),
  tabPanel(
  title = "The App",
  sidebarPanel(
    conditionalPanel(condition = "output.beginning == false",
      selectInput(inputId = "display", "Choose plots displayed:",
                choices = c('Show All'=1,'Show Simulation'=2,
                            'Show Theoretical'=3))
      ),
    conditionalPanel(condition = "input.goButton == 0 || output.beginning == true",
      sliderInput(inputId = "totalTime", label = "Extent of Time",
                min = max.time*.1, max = max.time, value = max.time*0.5),
      sliderInput(inputId = "n_0", label = "Initial Population",
                min = 2, max = max.n_0, value = 20, step = 1),
      sliderInput(inputId = "b", label = "Max Birth Rate",
                min = 0, max = max.b, value = .05),
      sliderInput(inputId = "d", label = "Min Death Rate",
                min = 0, max = min.d, value = .02),
      conditionalPanel(condition = "input.b > input.d",
        sliderInput(inputId = "m", label = "Carrying Capacity",
                  min = 10, max = max.m, value = max.m*0.8)),
      conditionalPanel(condition = "input.b < input.d",
        helpText("Carrying Capacity doesn't affect population when death rate",
               "is greater than birth rate.")),
      checkboxInput(inputId = 'seed', label = "Do you want to set the seed?"),
      conditionalPanel(condition = "input.seed == true",
        numericInput(inputId = 'setter', label = "Set your seed", value = 2200,
                    min = 1, step = 1))
    ),
    helpText("Click the button below for simulation results:"),
    actionButton("goButton", "Simulate"),
    conditionalPanel(
      condition = "output.beginning == false",
      br(),
      actionButton("reset", "Start Over")
    )
  ),
  mainPanel(
    conditionalPanel(
      condition = "input.goButton == 0 || output.beginning == true",
      radioButtons("initialGraphType", "Y-Axis shows Population: ",
                   choices = c("Size" = "pop",
                               "Growth Rate" = "rate",
                               "Per-captita Growth Rate" = "relRate"),
                   inline = TRUE),
      plotOutput("initialGraph"),
      htmlOutput("initialDiscuss")
    ),
    conditionalPanel(
      condition = "input.goButton > 0 && output.beginning == false",
      tabsetPanel(
      tabPanel(
        title = "Population Graphs",
        radioButtons("graphType", "Y-Axis shows Population: ",
                     choices = c("Size" = "pop",
                                 "Growth Rate" = "rate",
                                 "Per-captita Growth Rate" = "relRate"),
                     inline = TRUE),
        plotOutput("pop"),
        htmlOutput('discuss')
      ),
      tabPanel(
        title = "Field"
        , uiOutput("momentF")
        , plotOutput("field")
        , textOutput("population")
        , tableOutput("babies")
      ),
      tabPanel(
        title = "Graveyard"
        , uiOutput("momentG")
        , plotOutput("gy")
        , tableOutput("deathTallies")
      ),
      id = "tabset"
    )
    ) # end 2nd cond panel in main panel
  )
  ),
  tabPanel(
    title = "About"
    , includeHTML("./info.files/about.html")
  )
)