library(shiny)


popGraph <- function(m, b, d, n_0, totalTime) {
  totalTime <- 550
  t <- 0
  n_t <- n_0 # initial value, garbage value to start


  bt <- 0 # Initial values to make these objects persistant
  dt <- 0 # Initial values to make these objects persistant
  st <- 0 # Initial values to make these objects persistant
  lt <- 0 # Initial values to make these objects persistant
  Lt <- 0 # Initial values to make these objects persistant
  littersizes <- list(rep(-1, totalTime + 1)) # Initial values to make these objects persistan_t
  dead <- list(rep(-1, totalTime + 1)) # Initial values to make these objects persistant
  living <- list(rep(-1, totalTime + 1)) # Initial values to make these objects


  daframe <- data.frame(time = rep(0, totalTime + 1), numLitters = rep(0, totalTime + 1)
                        , population = rep(n_0, totalTime + 1)
                        , birthrate = rep(n_0, totalTime + 1), born = rep(0, totalTime + 1)
                        , deathrate = rep(0, totalTime + 1), dead = rep(0, totalTime + 1)
                        , theoretical = rep(n_0, totalTime + 1))
  dfrow <- numeric(length = 8)
  dfrow[1] <- t
  dfrow[2] <- Lt
  dfrow[3] <- n_t
  dfrow[4] <- bt
  dfrow[5] <- 0
  dfrow[6] <- 0
  dfrow[7] <- 0
  dfrow[8] <- 0

  update <- function() {
    t <<- t + 1
    bstar <-  b + (d - b)/2*(n_t/m) # Important factor in birth rate at time t
    dstar <-  d + (b - d)/2*(n_t/m) # Important factor in death rate at time t
    bt <<- max(0, bstar)          # birth rate at time t
    dt <<- dstar + max(0, -bstar) # death rate at time t
    st <<- 8*sqrt(bt)/sqrt(b) # averate size of litters
    lt <<- sqrt(bt)*sqrt(b)/8 # rate of litters
    Lt <<- rpois(n = 1, lambda = lt*n_t) # number of litters based on litter rate
    if (is.null(littersizes)) {} # Do nothing
    else {
      littersizes[[t]] <<- rpois(Lt, lambda = st) # produce sizes of each litter
    }
    if (is.null(dead)) {} # Do nothing
    else {
      dead[[t]] <<- rpois(1, lambda = dt*n_t) # produce dead rabbits
    }

    # keep track of the living rabbits and when they were born
    birthsThisGeneration <- sum(littersizes[[t]])
    if (birthsThisGeneration >= 1) {
      living[[t]] <<- 1:birthsThisGeneration
    }
    else {
      living[[t]] <<- NULL
    }
    for (i in 1:dead[[t]]) {
    }

    if (length(littersizes[[t]])) {
      # population sizes = previous population + sum of all litters - dead rabbits
      n_t <<- n_t + sum(littersizes[[t]]) - sum(dead[[t]])
    }

    dfrow[1] <- t
    dfrow[2] <- Lt
    dfrow[3] <- n_t
    dfrow[4] <- bt
    dfrow[5] <- sum(littersizes[[t]])
    dfrow[6] <- dt
    dfrow[7] <- sum(dead[[t]])
    # below line is jacob's broken equation
    # dfrow[8] <- ((m*n_0)/(m-n_0)*exp((b-d)/(m^2)*t))/(1+(n_0)/(m-n_0)*exp((b-d)/(m^2)*t))
    dfrow[8] <- m / (1 + ((m-n_0)/n_0)*exp(-1*(b-d)*t))
    daframe[t+1,] <<- dfrow
    #####
  }


  for (i in 1:totalTime) {
    update()
  }

  plot(x=daframe$time, daframe$population, ylab = "Population (n)"
       , xlab = "Time (Insert Units)")

  lines(x=daframe$time, daframe$theoretical, col = "red")

}

function(input, output) {

#  rv <- reactiveValues(
#    t = NULL,
#    ne = vector(),
#    nl = vector(),
#    Re = vector(),
#    re = vector(),
#    rl = vector(),
#    Rl = vector()
#  )


  output$pop <- renderPlot({
    input$goButton
    popGraph(  isolate(input$m)
             , isolate(input$b)
             , isolate(input$d)
             , isolate(input$n_0)
             , isolate(input$totalTime))
  })

#  output$growth <- renderPlot({
#    if(input$growthRate==1) {
#      plot(rv$ne,rv$Re, type="l", xlim=c(0,input$k*1.05), ylim=c(0,max(rv$Rl)*2),
#           lwd=2, col="red",
#           main="Population Growth Rate (R) v. Population Size (n)",
#           xlab="number of individuals (n)", ylab="new individuals/ time (R)",
#           cex.main=0.8, cex.lab=0.8, cex.axis=0.8)
#      points(rv$nl,rv$Rl, type="l", lwd=2, col="black")
#    }

#    if(input$growthRate==2) {
#      plot(rv$ne,rv$re, type="l", xlim=c(0,max.k*1.05), ylim=c(0,max.Rmax*1.05) ,
#           lwd=2, col="red",
#           main="Per Capita Growth Rate (r) v. Population Size (n)",
#           xlab="number of individuals (n)", ylab="new individuals/ time/ n (r)",
#           cex.main=0.8, cex.axis=0.8, cex.lab=0.8)
#      points(rv$nl,rv$rl, type="l", lwd=2, col="black")
#    }
#  })

}