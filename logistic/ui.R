max.time<-500
max.Rmax<-1
max.k<-1000

fluidPage(
  titlePanel("Logistic Growth"),
  sidebarPanel(
    sliderInput(inputId = "time", label = "Extent of Time", min = 0, 
                max = max.time, value = max.time*0.2),
    sliderInput(inputId = "Rmax", label = "Rmax",
                min = 0, max = max.Rmax, value = max.Rmax*0.1),
    sliderInput(inputId = "k", label = "Carrying Capacity",
                min = 10, max = max.k, value = max.k*0.8)
  ),
  mainPanel(
    tabsetPanel(
      tabPanel(
        title = "Population Size",
        plotOutput("pop")
      ),
      tabPanel(
        title = "Growth Rate",
        plotOutput("growth"),
        selectInput(inputId = "growthRate", label = "Type of Growth Rate to Show",
                    choices = c("Population Growth Rate" =1, "Per Capita Growth Rate" =2),
                    selected = 1)
      ),
      id = "tabset"
    )
  )
)