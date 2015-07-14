max.time<-1000
max.b <- 1
min.d <- 1
max.m <-1000
max.n_0 <- 2000

fluidPage(
  titlePanel("Logistic Growth"),
  sidebarPanel(
    selectInput(inputId = "display", "Choose plots displayed:", 
                choices = c('Show All'=1,'Show Simulation'=2,
                            'Show Theoretical'=3)),
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
    actionButton("goButton", "Simulate"),
    helpText("Click the button to display the",
              "graph with the inputs you selected.")
  ),
  mainPanel(
    tabsetPanel(
      tabPanel(
        title = "Population Size",
        plotOutput("pop")
      ),
      tabPanel(
        title = "Growth Rate"
      ),
      tabPanel(
        title = "Field"
        , uiOutput("momentF")
        , textOutput("population")
        , textOutput("babies")
      ),
      tabPanel(
        title = "Graveyard"
      ),
      id = "tabset"
    )
  )
)